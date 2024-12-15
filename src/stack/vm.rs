use super::{instructions::StackIR, FunctionInfo, Label, StackValue};
use std::collections::HashMap;

#[derive(Debug)]
pub struct VM {
    instruction_pointer: usize,
    stack: Vec<StackValue>,
    frames: Vec<StackFrame>,
    pub function_table: HashMap<String, FunctionInfo>,
    pub label_positions: HashMap<Label, usize>,
    /// global variables
    variables: HashMap<String, StackValue>,

    label_counter: usize,
}

#[derive(Debug)]
struct StackFrame {
    return_address: usize,
    local_variables: HashMap<String, StackValue>,
}

impl VM {
    pub fn new() -> Self {
        VM {
            stack: Vec::new(),
            frames: Vec::new(),
            function_table: HashMap::new(),
            instruction_pointer: 0,
            variables: HashMap::new(),
            label_positions: HashMap::new(),
            label_counter: 0,
        }
    }

    pub fn generate_label(&mut self) -> Label {
        let label = self.label_counter;
        self.label_counter += 1;
        Label(label)
    }

    pub fn execute(&mut self, instructions: &[StackIR]) -> Option<StackValue> {
        // println!("Starting program execution");
        self.instruction_pointer = 0;

        while self.instruction_pointer < instructions.len() {
            let instruction = &instructions[self.instruction_pointer];
            // println!("---");
            // println!("exec instruction: {:?}", instruction);
            // println!("stack           : {:?}", self.stack);
            // println!("variables       : {:?}", self.variables);
            // println!("frames          : {:?}", self.frames);
            // println!("---");

            match instruction {
                StackIR::Const(value) => {
                    self.stack.push(value.clone());
                    self.instruction_pointer += 1;
                }
                StackIR::Load(name) => {
                    // check first in all frames inner to outer
                    let value = self
                        .frames
                        .iter()
                        .rev()
                        .find_map(|frame| frame.local_variables.get(name))
                        .or_else(|| self.variables.get(name))
                        .cloned();

                    match value {
                        Some(v) => self.stack.push(v),
                        None => {
                            if self.function_table.contains_key(name) {
                                let function = self.create_function(name); // Creates with current captures
                                self.stack.push(function);
                            } else {
                                panic!("variable or function {} not found", name)
                            }
                        }
                    }
                    self.instruction_pointer += 1;
                }
                StackIR::Store(name) => {
                    let value = self.stack.pop().expect("stack underflow in store");

                    if let Some(frame) = self.frames.last_mut() {
                        frame.local_variables.insert(name.clone(), value);
                    } else {
                        self.variables.insert(name.clone(), value);
                    }
                    self.instruction_pointer += 1;
                }
                StackIR::Call(name) => {
                    // search all frames inner to outer
                    // todo, only check current frame
                    let (function_name, captures) = self
                        .frames
                        .iter()
                        .rev()
                        .find_map(|frame| {
                            if let Some(StackValue::Function {
                                name: fname,
                                captures,
                            }) = frame.local_variables.get(name)
                            {
                                Some((fname.clone(), captures.clone()))
                            } else {
                                None
                            }
                        })
                        .or_else(|| {
                            // try in the global variables
                            match self.variables.get(name) {
                                Some(StackValue::Function {
                                    name: fname,
                                    captures,
                                }) => Some((fname.clone(), captures.clone())),
                                Some(_) => panic!("not a function reference"),
                                None => Some((name.clone(), HashMap::new())), // Direct function call
                            }
                        })
                        .expect("function not found");

                    let func_info = self
                        .function_table
                        .get(&function_name)
                        .expect(&format!("function not found {}", function_name));

                    let mut frame = StackFrame {
                        return_address: self.instruction_pointer + 1,
                        local_variables: captures,
                    };

                    // Pop args in reverse order
                    for param_name in func_info.parameter_names.iter().rev() {
                        let arg = self.stack.pop().expect("stack underflow in function call");
                        frame.local_variables.insert(param_name.clone(), arg);
                    }

                    self.frames.push(frame);
                    self.instruction_pointer = self.get_label_position(func_info.entry_label);
                }

                StackIR::Return => {
                    // we always return something
                    let return_value = self.stack.pop().unwrap_or(StackValue::Unit);
                    let frame = self.frames.pop().expect("return without frame");

                    self.instruction_pointer = frame.return_address;
                    self.stack.push(return_value);
                }

                StackIR::Print => {
                    let value = self.stack.pop().expect("stack underflow in print");
                    println!("PRINTING FROM STACK > {:?}", value);
                    self.instruction_pointer += 1;
                }
                StackIR::Label(_) => {
                    // Labels are just markers, no runtime effect
                    self.instruction_pointer += 1;
                }
                StackIR::Jump(label) => {
                    self.instruction_pointer = self.get_label_position(*label);
                }
                StackIR::JumpIfFalse(label) => {
                    let condition = self.stack.pop().expect("stack underflow JumpIfFalse");

                    match condition {
                        StackValue::Boolean(false) => {
                            self.instruction_pointer = self.get_label_position(*label);
                        }
                        StackValue::Boolean(true) => {
                            self.instruction_pointer += 1;
                        }
                        _ => panic!("JumpIfFalse condition must be boolean, got {:?}", condition),
                    }
                }
                StackIR::Pop => {
                    self.stack.pop().expect("stack undeflow with pop");
                    self.instruction_pointer += 1
                }
                StackIR::Add => self.binary_op(|lhs, rhs| lhs + rhs),
                StackIR::Sub => self.binary_op(|lhs, rhs| lhs - rhs),
                StackIR::Mul => self.binary_op(|lhs, rhs| lhs * rhs),
                StackIR::Div => self.binary_op(|lhs, rhs| lhs / rhs),
                StackIR::Eq => self.binary_op(|lhs, rhs| StackValue::Boolean(lhs == rhs)),
                StackIR::Neq => self.binary_op(|lhs, rhs| StackValue::Boolean(lhs != rhs)),
                StackIR::Lt => self.binary_op(|lhs, rhs| StackValue::Boolean(lhs < rhs)),
                StackIR::Gt => self.binary_op(|lhs, rhs| StackValue::Boolean(lhs > rhs)),
                StackIR::Le => self.binary_op(|lhs, rhs| StackValue::Boolean(lhs <= rhs)),
                StackIR::Ge => self.binary_op(|lhs, rhs| StackValue::Boolean(lhs >= rhs)),
                StackIR::And => self.binary_op(|lhs, rhs| {
                    if let (StackValue::Boolean(l), StackValue::Boolean(r)) = (lhs, rhs) {
                        StackValue::Boolean(l && r)
                    } else {
                        panic!("unsupported operation")
                    }
                }),
                StackIR::Or => self.binary_op(|lhs, rhs| {
                    if let (StackValue::Boolean(l), StackValue::Boolean(r)) = (lhs, rhs) {
                        StackValue::Boolean(l || r)
                    } else {
                        panic!("unsupported operation")
                    }
                }),
                StackIR::EnterBlock => {
                    // let parent_frame = self.frames.last().expect("no parent frame?");
                    self.frames.push(StackFrame {
                        return_address: self.instruction_pointer + 1,
                        local_variables: HashMap::new(),
                    });
                    self.instruction_pointer += 1;
                }
                StackIR::ExitBlock => {
                    self.frames.pop().expect("exit block without frame");
                    self.instruction_pointer += 1;
                }
            }
        }

        self.stack.pop()
    }

    fn binary_op<F>(&mut self, op: F)
    where
        F: FnOnce(StackValue, StackValue) -> StackValue,
    {
        let rhs = self.stack.pop().expect("stack underflow");
        let lhs = self.stack.pop().expect("stack underflow");
        let result = op(lhs.clone(), rhs.clone());
        // println!("{:?}  {:?} >>> {:?}", lhs, rhs, result);
        self.stack.push(result);
        self.instruction_pointer += 1;
    }

    pub fn register_function(&mut self, name: String, info: FunctionInfo) {
        self.function_table.insert(name, info);
    }

    fn get_label_position(&self, label: Label) -> usize {
        *self.label_positions.get(&label).expect("Label not found")
    }

    fn create_function(&mut self, name: &str) -> StackValue {
        let info = self.function_table.get(name).unwrap();
        let mut captures = HashMap::new();
        // dbg!(&info);

        // capture from all the frames
        for capture in &info.captures {
            if let Some(value) = self
                .frames
                .iter()
                .rev()
                .find_map(|frame| frame.local_variables.get(capture).cloned())
            {
                captures.insert(capture.clone(), value);
            }
        }

        StackValue::Function {
            name: name.to_owned(),
            captures,
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::{
        ast::typechecker::check_types,
        stack::{execute_program, stackvalue::StackValue},
    };

    #[test]
    fn simple_vm_run() {
        let source_code = r#"
let x = 5;
print(x);
"#;
        let ast = crate::create_ast(source_code);
        execute_program(ast);
    }

    #[test]
    fn simple_function_vm_run() {
        let source_code = r#"
fn add(x: int, y: int) -> int {
    x + y
}
add(5, 3);

print(add(5,5));
"#;
        let ast = crate::create_ast(source_code);
        assert_eq!(execute_program(ast), None);
    }

    #[test]
    fn higher_order_vm() {
        let source_code = r#"
fn add(left: fn(int,int)->int, right: fn(int,int)->int, x: int, y: int) -> int {
    left(x,y) + right(x,y)
}

fn simple_adder(x: int, y: int) -> int{
    x + y
}

let x = simple_adder;

print(add(x, simple_adder, 5, 3));
"#;
        let ast = crate::create_ast(source_code);
        assert_eq!(check_types(&ast), Ok(()));
        execute_program(ast);
    }

    #[test]
    fn negative_things_vm() {
        let source_code = r#"
print(5 - 3);

let x = 5;

let y = {
        let x = 2;
        let o = 3;
        x - o
}

print(x - y)
"#;
        let ast = crate::create_ast(source_code);
        assert!(check_types(&ast) == Ok(()));
        execute_program(ast);
    }

    #[test]
    fn unit_things_vm() {
        let source_code = r#"
let x = {
    5
}

let y = {
        let x = 2;
        let o = 3;
        x - o
}

print(x)
print(y)

x + y
"#;
        let ast = crate::create_ast(source_code);
        assert_eq!(Ok(()), check_types(&ast));
        assert_eq!(execute_program(ast), Some(StackValue::Integer(4)))
    }

    #[test]
    fn main_function_vm() {
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
}
        "#;
        let ast = crate::create_ast(source_code);
        assert_eq!(Ok(()), check_types(&ast));
        assert_eq!(execute_program(ast), Some(StackValue::Integer(120)))
    }

    #[test]
    fn lambda_vm() {
        let source_code = r#"
        let adder = |n:int| -> int{
            n+1
        }

        adder(6) + 1
        "#;

        let ast = crate::create_ast(source_code);
        assert_eq!(check_types(&ast), Ok(()));
        assert_eq!(execute_program(ast), Some(StackValue::Integer(8)))
    }

    #[test]
    fn nested_lambda_vm() {
        let source_code = r#"
        let makeAdder = |x:int| -> fn(int)->int {
            |y:int| -> int {
                x + y
            }
        };
    
        let add5 = makeAdder(5);
        add5(3)  
        "#;

        let ast = crate::create_ast(source_code);
        assert_eq!(check_types(&ast), Ok(()));
        assert_eq!(execute_program(ast), Some(StackValue::Integer(8)))
    }

    #[test]
    fn lambda_but_as_fn_vm() {
        let source_code = r#"
        fn adder(n:int)->int{
            n+1
        } 
        let x = adder;

        x(6) + 1
        "#;

        let ast = crate::create_ast(source_code);
        assert_eq!(check_types(&ast), Ok(()));
        assert_eq!(execute_program(ast), Some(StackValue::Integer(8)))
    }

    #[test]
    #[should_panic(expected = "don't use lambda_% as a function name")]
    fn panic_function_lambda_name_vm() {
        let source_code = r#"
        fn lambda_123(n:int)->int{
            n+1
        } 
        let x = lambda_123;

        x(6) + 1
        "#;

        let ast = crate::create_ast(source_code);
        assert_eq!(check_types(&ast), Ok(()));
        assert_eq!(execute_program(ast), Some(StackValue::Integer(8)))
    }
}
