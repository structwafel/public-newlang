use crate::ast::{Expression, TypedArg};
use core::fmt;
use instructions::StackIR;
pub use stackvalue::StackValue;
use std::collections::{HashMap, HashSet};
pub use vm::VM;

mod instructions;
mod stackvalue;
mod vm;

#[derive(Copy, Clone, PartialEq, Debug, Hash, Eq, PartialOrd, Ord)]
pub struct Label(usize);
pub type FunctionName = String;

#[derive(Clone, PartialEq, Debug)]
pub struct FunctionInfo {
    entry_label: Label,
    parameter_names: Vec<String>,
    captures: Vec<String>,
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub fn setup_vm(expressions: Vec<Expression>) -> (VM, Vec<StackIR>) {
    let mut vm = VM::new();
    let (instructions, function_table) = vm.expressions_to_stack_irs(expressions);

    // Add all base functions to table
    for (name, info) in function_table.iter() {
        vm.register_function(name.clone(), info.clone());
    }

    // Position map for knowing where on the instructions to jump
    for (pos, instruction) in instructions.iter().enumerate() {
        if let StackIR::Label(label) = instruction {
            vm.label_positions.insert(*label, pos);
        }
    }

    // Prepare final instructions
    let instructions = if function_table.contains_key("main") {
        let mut main_instructions = vec![];
        main_instructions.extend(instructions);
        main_instructions.push(StackIR::Call("main".to_string()));
        main_instructions
    } else {
        instructions
    };

    // println!("Instructions: {:?}", instructions);
    // println!("Function table: {:?}", function_table);
    // println!("Label positions: {:?}", vm.label_positions);

    (vm, instructions)
}

pub fn execute_program(expressions: Vec<Expression>) -> Option<StackValue> {
    // optimize the expressions:
    let optimized = expressions.into_iter().map(|e| e.optimize()).collect();

    let (mut vm, instructions) = setup_vm(optimized);
    vm.execute(&instructions)
}

impl VM {
    pub fn convert_to_stack_ir(&mut self, expr: Expression, keep_value: bool) -> Vec<StackIR> {
        match expr {
            Expression::Value(value) => {
                if keep_value {
                    vec![StackIR::Const(value.into())]
                } else {
                    vec![]
                }
            }
            Expression::Variable(var_name) => {
                if keep_value {
                    vec![StackIR::Load(var_name)]
                } else {
                    vec![]
                }
            }
            Expression::BinaryOperation { lhs, operator, rhs } => {
                let mut instructions = Vec::new();
                instructions.extend(self.convert_to_stack_ir(*lhs, true));
                instructions.extend(self.convert_to_stack_ir(*rhs, true));
                instructions.push(StackIR::from_operator(&operator));
                instructions
            }
            Expression::VariableAssignment { name, value } => {
                let mut instructions = Vec::new();
                instructions.extend(self.convert_to_stack_ir(*value, true));
                instructions.push(StackIR::Store(name));
                if keep_value {
                    instructions.push(StackIR::Const(StackValue::Unit));
                }
                instructions
            }
            Expression::Block { expressions } => {
                let mut instructions = vec![StackIR::EnterBlock];
                for (i, expr) in expressions.iter().enumerate() {
                    let is_last = i == expressions.len() - 1;
                    instructions
                        .extend(self.convert_to_stack_ir(expr.clone(), is_last && keep_value));
                }
                instructions.push(StackIR::ExitBlock);
                instructions
            }
            Expression::Print { value } => {
                let mut instructions = Vec::new();
                instructions.extend(self.convert_to_stack_ir(*value, true));
                instructions.push(StackIR::Print);
                if keep_value {
                    instructions.push(StackIR::Const(StackValue::Unit));
                }
                instructions
            }
            Expression::FunctionCall { name, args } => {
                let mut instructions = Vec::new();
                for arg in args {
                    instructions.extend(self.convert_to_stack_ir(arg, true));
                }
                instructions.push(StackIR::Call(name));
                // If we don't need the value, pop it
                if !keep_value {
                    instructions.push(StackIR::Pop);
                }
                instructions
            }
            Expression::Discard { value } => self.convert_to_stack_ir(*value, false),
            Expression::FunctionDefinition { .. } => unreachable!("don't do functiondef  here"),
            Expression::Lambda {
                id,
                args: _,
                body: _,
                ..
            } => {
                let lambda_name = format!("lambda_{}", id);
                // just load lambda, as it is hoisted
                vec![StackIR::Load(lambda_name)]
            }
            Expression::IfElse {
                condition,
                then_branch,
                else_branch,
            } => {
                let mut instructions = Vec::new();
                let end_label = self.generate_label(); // label at the end of the branching, if false we need to jump to here

                instructions.extend(self.convert_to_stack_ir(*condition, true));

                // with else branch, we need two labels
                if let Some(else_branch) = else_branch {
                    let else_label = self.generate_label();

                    // if it is false, we need to jump to the else_label
                    instructions.push(StackIR::JumpIfFalse(else_label));

                    // otherwise we can just fall through and to then
                    instructions.extend(self.convert_to_stack_ir(*then_branch, keep_value));

                    // jump over the else branch
                    instructions.push(StackIR::Jump(end_label));

                    // else
                    instructions.push(StackIR::Label(else_label));
                    instructions.extend(self.convert_to_stack_ir(*else_branch, keep_value));
                } else {
                    // if false jump to end
                    instructions.push(StackIR::JumpIfFalse(end_label));

                    instructions.extend(self.convert_to_stack_ir(*then_branch, keep_value));

                    if keep_value {
                        instructions.push(StackIR::Jump(end_label));
                        instructions.push(StackIR::Const(StackValue::Unit));
                    }
                }

                instructions.push(StackIR::Label(end_label));
                instructions
            } // e => panic!("not yet implemented: {:?}", e),
        }
    }

    // Update function definition to use keep_value
    pub fn convert_function_definition(
        &mut self,
        _name: String,
        args: Vec<TypedArg>,
        body: Expression,
        captures: Vec<String>,
    ) -> (Vec<StackIR>, FunctionInfo) {
        let entry_label = self.generate_label();

        // Store function info for table
        let function_info = FunctionInfo {
            entry_label,
            parameter_names: args.iter().map(|arg| arg.name.clone()).collect(),
            captures,
        };

        // First we need the entry label
        let mut instructions = vec![StackIR::Label(entry_label)];

        // Then we need the function body instructions
        instructions.extend(self.convert_to_stack_ir(body, true));

        // Finally return
        instructions.push(StackIR::Return);

        (instructions, function_info)
    }

    pub fn expressions_to_stack_irs(
        &mut self,
        expressions: Vec<Expression>,
    ) -> (Vec<StackIR>, HashMap<String, FunctionInfo>) {
        let mut all_instructions = Vec::new();
        let mut function_table = HashMap::new();
        let mut function_instructions = Vec::new();

        // Collect and process lambdas first
        let mut all_lambdas = Vec::new();
        for expr in expressions.iter() {
            all_lambdas.extend(collect_lambdas(expr));
        }

        // Process all lambdas
        for (lambda_name, args, body, captures) in all_lambdas {
            let (func_instructions, func_info) =
                self.convert_function_definition(lambda_name.clone(), args, *body, captures);
            function_instructions.extend(func_instructions);
            function_table.insert(lambda_name, func_info);
        }

        // process all function_definitions
        for expr in expressions.clone() {
            if let Expression::FunctionDefinition {
                name,
                args,
                body,
                return_type: _,
            } = expr
            {
                if name.starts_with("lambda_") {
                    panic!("don't use lambda_% as a function name")
                }
                let (func_instructions, func_info) = self.convert_function_definition(
                    name.clone(),
                    args.clone(),
                    *body.clone(),
                    vec![],
                );
                function_instructions.extend(func_instructions);
                function_table.insert(name.clone(), func_info);
            }
        }

        // Handle main program
        for expr in expressions {
            if !matches!(expr, Expression::FunctionDefinition { .. }) {
                all_instructions.extend(self.convert_to_stack_ir(expr, true));
            }
        }

        // Add jump to end before function definitions
        let end_label = self.generate_label();
        all_instructions.push(StackIR::Jump(end_label));

        // Add function bodies
        all_instructions.extend(function_instructions);

        // Add end label
        all_instructions.push(StackIR::Label(end_label));

        (all_instructions, function_table)
    }
}
fn collect_lambdas(
    expr: &Expression,
    // name, args, body, captures
) -> Vec<(String, Vec<TypedArg>, Box<Expression>, Vec<String>)> {
    let mut lambdas = Vec::new();
    match expr {
        Expression::Lambda { id, args, body, .. } => {
            let nested = collect_lambdas(body);
            let param_names: HashSet<_> = args.iter().map(|arg| &arg.name).collect();
            let captures = body
                .find_free_variables()
                .into_iter()
                .filter(|name| !param_names.contains(name))
                .collect();

            lambdas.extend(nested);
            lambdas.push((
                format!("lambda_{}", id),
                args.clone(),
                body.clone(),
                captures,
            ));
        }
        Expression::Block { expressions } => {
            for expr in expressions {
                lambdas.extend(collect_lambdas(expr));
            }
        }
        Expression::FunctionDefinition { body, .. } => {
            lambdas.extend(collect_lambdas(body));
        }
        Expression::IfElse {
            condition,
            then_branch,
            else_branch,
        } => {
            lambdas.extend(collect_lambdas(condition));
            lambdas.extend(collect_lambdas(then_branch));
            if let Some(else_) = else_branch {
                lambdas.extend(collect_lambdas(else_));
            }
        }
        Expression::VariableAssignment { name: _, value } => {
            lambdas.extend(collect_lambdas(value));
        }
        Expression::Discard { value } => lambdas.extend(collect_lambdas(value)),
        _ => {}
    }
    lambdas
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use crate::{
        ast::typechecker::check_types,
        stack::{instructions::StackIR, setup_vm, FunctionInfo, Label, StackValue},
    };

    #[test]
    fn simple_stack_ir() {
        let source_code = r#"
fn add(x: string, y: string) -> string {
    x + y
}

let y = add("hello","world");
print(y);
"#;
        let ast = crate::create_ast(source_code);
        assert_eq!(check_types(&ast), Ok(()));
        let (_vm, instructions) = setup_vm(ast);

        assert_eq!(
            instructions,
            vec![
                StackIR::Const(StackValue::String("hello".into())),
                StackIR::Const(StackValue::String("world".into())),
                StackIR::Call("add".into()), // will go to entry_label 0
                StackIR::Store("y".into()),
                StackIR::Load("y".into()),
                StackIR::Print,
                StackIR::Jump(Label(1)), // jump over the functions
                // functions
                StackIR::Label(Label(0)),
                StackIR::EnterBlock,
                StackIR::Load("x".into()),
                StackIR::Load("y".into()),
                StackIR::Add,
                StackIR::ExitBlock,
                StackIR::Return,
                // end of program
                StackIR::Label(Label(1)),
            ]
        )
    }

    #[test]
    fn lambda_collection() {
        let source_code = r#"
    let make_adder = |x:int| -> fn(int)->int {
        |y:int| -> int {
            x + y
        }
    };
    "#;
        let ast = crate::create_ast(source_code);
        let (vm, instructions) = setup_vm(ast);

        assert_eq!(
            instructions,
            vec![
                StackIR::Load("lambda_1".into()),
                StackIR::Store("make_adder".into()),
                StackIR::Jump(Label(2)),
                StackIR::Label(Label(0)),
                StackIR::EnterBlock,
                StackIR::Load("x".into()),
                StackIR::Load("y".into()),
                StackIR::Add,
                StackIR::ExitBlock,
                StackIR::Return,
                StackIR::Label(Label(1)),
                StackIR::EnterBlock,
                StackIR::Load("lambda_0".into()),
                StackIR::ExitBlock,
                StackIR::Return,
                StackIR::Label(Label(2))
            ]
        );
        assert_eq!(
            BTreeMap::from_iter(vm.function_table),
            BTreeMap::from([
                (
                    "lambda_0".into(),
                    FunctionInfo {
                        entry_label: Label(0),
                        parameter_names: vec!["y".into()],
                        captures: vec!["x".into()]
                    }
                ),
                (
                    "lambda_1".into(),
                    FunctionInfo {
                        entry_label: Label(1),
                        parameter_names: vec!["x".into()],
                        captures: vec![]
                    }
                )
            ])
        );
        assert_eq!(
            BTreeMap::from_iter(vm.label_positions),
            BTreeMap::from([(Label(2), 15), (Label(1), 10), (Label(0), 3)])
        );
    }

    #[test]
    fn test_reasonable_program() {
        let source_code = r#"
fn main() -> int{
    let x = 2;
    let y = 3;

    let z = add(x,y);

    factorial(z)
}


fn add(x:int, y:int) -> int{
    x + y
}

fn factorial(n: int) -> int{
    if n < 1{
        1
    }else{
        n * factorial(n - 1)
    }
}"#;
        let ast = crate::create_ast(source_code);
        check_types(&ast).unwrap();
        let (vm, instructions) = setup_vm(ast);

        assert_eq!(
            instructions,
            vec![
                StackIR::Jump(Label(5)),  // jump over the functions to main
                StackIR::Label(Label(0)), // main starting point
                StackIR::EnterBlock,
                StackIR::Const(StackValue::Integer(2)),
                StackIR::Store("x".into()),
                StackIR::Const(StackValue::Integer(3)),
                StackIR::Store("y".into()),
                StackIR::Load("x".into()),
                StackIR::Load("y".into()),
                StackIR::Call("add".into()), // label 1
                StackIR::Store("z".into()),
                StackIR::Load("z".into()),
                StackIR::Call("factorial".into()), // label 2
                StackIR::ExitBlock,
                StackIR::Return,
                StackIR::Label(Label(1)), // add function start
                StackIR::EnterBlock,
                StackIR::Load("x".into()),
                StackIR::Load("y".into()),
                StackIR::Add,
                StackIR::ExitBlock,
                StackIR::Return,
                StackIR::Label(Label(2)), // factorial function start
                StackIR::EnterBlock,
                StackIR::Load("n".into()),
                StackIR::Const(StackValue::Integer(1)),
                StackIR::Lt,
                StackIR::JumpIfFalse(Label(4)), // if
                StackIR::EnterBlock,
                StackIR::Const(StackValue::Integer(1)),
                StackIR::ExitBlock,
                StackIR::Jump(Label(3)),
                StackIR::Label(Label(4)), // else
                StackIR::EnterBlock,
                StackIR::Load("n".into()),
                StackIR::Load("n".into()),
                StackIR::Const(StackValue::Integer(1)),
                StackIR::Sub,
                StackIR::Call("factorial".into()),
                StackIR::Mul,
                StackIR::ExitBlock,
                StackIR::Label(Label(3)), // jump over the else
                StackIR::ExitBlock,
                StackIR::Return,
                StackIR::Label(Label(5)), // label to jump over all the functions
                StackIR::Call("main".into())  // label 0
            ]
        );
        assert_eq!(
            BTreeMap::from_iter(vm.function_table),
            BTreeMap::from([
                (
                    "main".into(),
                    FunctionInfo {
                        entry_label: Label(0),
                        parameter_names: vec![],
                        captures: vec![]
                    }
                ),
                (
                    "add".into(),
                    FunctionInfo {
                        entry_label: Label(1),
                        parameter_names: vec!["x".into(), "y".into()],
                        captures: vec![]
                    }
                ),
                (
                    "factorial".into(),
                    FunctionInfo {
                        entry_label: Label(2),
                        parameter_names: vec!["n".into()],
                        captures: vec![]
                    }
                )
            ])
        );
        assert_eq!(
            BTreeMap::from_iter(vm.label_positions),
            BTreeMap::from([
                (Label(0), 1),
                (Label(1), 15),
                (Label(2), 22),
                (Label(3), 41),
                (Label(4), 32),
                (Label(5), 44)
            ])
        );
    }
}
