use super::*;
use crate::linkedlist::LinkedList;

#[derive(Clone, Debug, PartialEq)]
pub enum TypeResult {
    Concrete(Type),
    /// Perhaps we want to keep errors of the entire ast, and not fail on first error
    Error(String),
}

impl std::fmt::Display for TypeResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeResult::Concrete(t) => write!(f, "{}", t),
            TypeResult::Error(msg) => write!(f, "Error: {}", msg),
        }
    }
}

/// env just for type checking, scoping because we want to disallow reassiging, but allow reassignign in block scopes
#[derive(Debug, Clone, PartialEq)]
pub struct TypeEnv {
    scopes: LinkedList<LinkedList<(String, TypeResult)>>,
}

impl TypeEnv {
    pub fn new() -> Self {
        Self {
            scopes: LinkedList::new().prepend(LinkedList::new()),
        }
    }

    pub fn get(&self, name: &str) -> Option<&TypeResult> {
        self.scopes.iter().find_map(|scope| {
            scope.iter().find_map(|(var_name, type_result)| {
                if var_name == name {
                    Some(type_result)
                } else {
                    None
                }
            })
        })
    }

    pub fn extend(self, name: String, type_result: TypeResult) -> Self {
        match self.scopes {
            LinkedList::Node { value: scope, next } => Self {
                scopes: LinkedList::Node {
                    value: scope.prepend((name, type_result)),
                    next,
                },
            },
            LinkedList::Tail => unreachable!("TypeEnv should always have at least one scope"),
        }
    }

    pub fn push_scope(self) -> Self {
        Self {
            scopes: self.scopes.prepend(LinkedList::new()),
        }
    }

    pub fn pop_scope(self) -> Self {
        match self.scopes {
            LinkedList::Node { ref next, .. } => {
                if matches!(**next, LinkedList::Tail) {
                    self
                } else {
                    Self {
                        scopes: (**next).clone(),
                    }
                }
            }
            LinkedList::Tail => unreachable!("TypeEnv should always have at least one scope"),
        }
    }

    pub fn in_current_scope(&self, name: &str) -> bool {
        self.scopes.front().map_or(false, |scope| {
            scope.iter().any(|(var_name, _)| var_name == name)
        })
    }
}

pub fn check_types(ast: &[Expression]) -> Result<(), String> {
    let mut type_env = TypeEnv::new();

    // First pass: Register all function types
    for statement in ast.iter() {
        if let Expression::FunctionDefinition {
            name,
            args,
            return_type,
            ..
        } = statement
        {
            let function_type = TypeResult::Concrete(Type::Function {
                args: typed_args_to_vec_type(args),
                return_type: Box::new(return_type.clone()),
            });
            type_env = type_env.extend(name.clone(), function_type);
        }
    }

    // second pass, check function bodies and other things
    let mut _type_result = TypeResult::Concrete(Type::Unit);
    for statement in ast.iter() {
        match statement {
            Expression::FunctionDefinition {
                name,
                args,
                body,
                return_type,
            } => {
                // Create environment with function parameters
                let function_env = args.iter().fold(type_env.clone(), |env, arg| {
                    env.extend(arg.name.clone(), TypeResult::Concrete(arg.t.clone()))
                });

                // Check function body
                let (body_type, _) = body.check_types(function_env)?;

                if !matches!(body_type, TypeResult::Concrete(ref t) if t == return_type) {
                    return Err(format!(
                        "Function '{}' body type {:?} doesn't match return type {:?}",
                        name, body_type, return_type
                    ));
                }
            }
            other => {
                (_type_result, type_env) = other.check_types(type_env)?;
            }
        }
    }

    Ok(())
}

impl Expression {
    pub fn check_types(&self, env: TypeEnv) -> Result<(TypeResult, TypeEnv), String> {
        match self {
            Expression::Value(value) => Ok((TypeResult::Concrete(value.type_of()), env)),

            Expression::Variable(name) => {
                let type_result = env
                    .get(name)
                    .cloned()
                    .ok_or_else(|| format!("Variable {} not found", name))?;
                Ok((type_result, env))
            }

            Expression::VariableAssignment { name, value } => {
                if env.in_current_scope(name) {
                    return Err(format!("Cannot reassign to variable '{}'", name));
                }

                let (value_type, env) = value.check_types(env)?;
                let new_env = env.extend(name.clone(), value_type);
                Ok((TypeResult::Concrete(Type::Unit), new_env))
            }

            Expression::Block { expressions } => {
                let block_env = env.clone().push_scope();

                let mut current_env = block_env;
                let mut last_type = TypeResult::Concrete(Type::Unit);

                for expr in expressions {
                    let (expr_type, new_env) = expr.check_types(current_env)?;
                    current_env = new_env;
                    last_type = expr_type;
                }

                Ok((last_type, current_env.pop_scope()))
            }

            Expression::BinaryOperation { lhs, operator, rhs } => {
                let (lhs_type, env) = lhs.check_types(env)?;
                let (rhs_type, env) = rhs.check_types(env)?;

                match operator {
                    Operator::Add => match (&lhs_type, &rhs_type) {
                        (
                            TypeResult::Concrete(Type::String),
                            TypeResult::Concrete(Type::String),
                        ) => Ok((TypeResult::Concrete(Type::String), env)),
                        (TypeResult::Concrete(Type::Float), TypeResult::Concrete(Type::Float)) => {
                            Ok((TypeResult::Concrete(Type::Float), env))
                        }
                        (
                            TypeResult::Concrete(Type::Integer),
                            TypeResult::Concrete(Type::Integer),
                        ) => Ok((TypeResult::Concrete(Type::Integer), env)),
                        _ => Err(format!(
                            "Type mismatch in addition: {} + {}",
                            lhs_type, rhs_type
                        )),
                    },
                    Operator::Sub | Operator::Mul | Operator::Div => match (&lhs_type, &rhs_type) {
                        (
                            TypeResult::Concrete(Type::Integer),
                            TypeResult::Concrete(Type::Integer),
                        ) => Ok((lhs_type, env)),
                        (TypeResult::Concrete(Type::Float), TypeResult::Concrete(Type::Float)) => {
                            Ok((lhs_type, env))
                        }
                        _ => Err(format!(
                            "Invalid types for operation {:?}: {} {}",
                            operator, lhs_type, rhs_type
                        )),
                    },
                    Operator::Eq
                    | Operator::Neq
                    | Operator::Lt
                    | Operator::Gt
                    | Operator::Le
                    | Operator::Ge => {
                        if lhs_type == rhs_type {
                            Ok((TypeResult::Concrete(Type::Boolean), env))
                        } else {
                            Err(format!(
                                "Cannot compare different types: {} and {}",
                                lhs_type, rhs_type
                            ))
                        }
                    }
                    Operator::And | Operator::Or => match (&lhs_type, &rhs_type) {
                        (
                            TypeResult::Concrete(Type::Boolean),
                            TypeResult::Concrete(Type::Boolean),
                        ) => Ok((TypeResult::Concrete(Type::Boolean), env)),
                        _ => Err(format!(
                            "Boolean operation requires boolean operands, got {} and {}",
                            lhs_type, rhs_type
                        )),
                    },
                }
            }

            Expression::IfElse {
                condition,
                then_branch,
                else_branch,
            } => {
                let (cond_type, env) = condition.check_types(env)?;
                if !matches!(cond_type, TypeResult::Concrete(Type::Boolean)) {
                    return Err("If condition must be boolean".to_string());
                }

                let (then_type, env) = then_branch.check_types(env)?;

                if let Some(else_branch) = else_branch {
                    let (else_type, env) = else_branch.check_types(env)?;
                    if then_type != else_type {
                        return Err("If and else branches must have same type".to_string());
                    }
                    Ok((then_type, env))
                } else {
                    Ok((TypeResult::Concrete(Type::Unit), env))
                }
            }

            Expression::FunctionDefinition { .. } => {
                // we already handled it before
                Ok((TypeResult::Concrete(Type::Unit), env))

                // // when defining a function, we need to add it immediately to the enviromnent to support recursion

                // let function_type = TypeResult::Concrete(Type::Function {
                //     args: typed_args_to_vec_type(args),
                //     return_type: Box::new(return_type.clone()),
                // });
                // let new_env = env.extend(name.clone(), function_type);

                // let function_env = args.iter().fold(new_env.clone(), |env, arg| {
                //     env.extend(arg.name.clone(), TypeResult::Concrete(arg.t.clone()))
                // });

                // let (body_type, _) = body.check_types(function_env)?;

                // if !matches!(body_type,TypeResult::Concrete(t) if t == *return_type) {
                //     return Err("Function body type doesn't match return type".to_string());
                // }

                // // let function_type = TypeResult::Concrete(Type::Function {
                // //     args: typed_args_to_vec_type(args),
                // //     return_type: Box::new(return_type.clone()),
                // // });

                // // let new_env = env.extend(name.clone(), function_type);
                // Ok((TypeResult::Concrete(Type::Unit), new_env))
            }

            Expression::FunctionCall { name, args } => {
                let func_type = env
                    .get(name)
                    .cloned()
                    .ok_or_else(|| format!("Function '{}' not found", name))?;

                match func_type {
                    TypeResult::Concrete(Type::Function {
                        args: func_args,
                        return_type,
                    }) => {
                        if args.len() != func_args.len() {
                            return Err(format!(
                                "Function '{}' expects {} arguments, but {} were provided",
                                name,
                                func_args.len(),
                                args.len()
                            ));
                        }

                        let mut current_env = env;
                        for (arg_expr, expected_type) in args.iter().zip(func_args.iter()) {
                            let (arg_type, new_env) = arg_expr.check_types(current_env)?;
                            match arg_type {
                                TypeResult::Concrete(t) if t == *expected_type => (),
                                _ => return Err("Function argument type mismatch".to_string()),
                            }
                            current_env = new_env;
                        }

                        Ok((TypeResult::Concrete(*return_type), current_env))
                    }
                    _ => Err(format!("'{}' is not a function", name)),
                }
            }

            Expression::Print { value } => {
                let (_, env) = value.check_types(env)?;
                Ok((TypeResult::Concrete(Type::Unit), env))
            }

            Expression::Discard { value } => {
                let (_, env) = value.check_types(env)?;
                Ok((TypeResult::Concrete(Type::Unit), env))
            }
            Expression::Lambda {
                id: _id,
                args,
                body,
                return_type,
            } => {
                let lambda_env = args.iter().fold(env.clone(), |env, arg| {
                    env.extend(arg.name.clone(), TypeResult::Concrete(arg.t.clone()))
                });

                // Check lambda body
                let (body_type, _) = body.check_types(lambda_env)?;

                // Verify body type matches return type
                match body_type {
                    TypeResult::Concrete(t) if t == *return_type => {
                        // Create function type for the lambda
                        let lambda_type = TypeResult::Concrete(Type::Function {
                            args: typed_args_to_vec_type(args),
                            return_type: Box::new(return_type.clone()),
                        });
                        Ok((lambda_type, env))
                    }
                    _ => Err(format!(
                        "Lambda body type {:?} doesn't match declared return type {:?}",
                        body_type, return_type
                    )),
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::{ast::typechecker::check_types, create_ast};

    #[test]
    fn test_no_reassing_error() {
        let source_code = r#"
        let x = 5;
        let x = {
            let y = 1;
            y
        }
        "#;
        let ast = create_ast(source_code);

        assert_eq!(
            check_types(&ast),
            Err("Cannot reassign to variable 'x'".into())
        );
    }

    #[test]
    fn failing_type_check() {
        let source_code = r#"

        let x = 5;
        let z = x + "hello world";
        
        "#;

        let ast = create_ast(source_code);
        assert_eq!(
            check_types(&ast),
            Err("Type mismatch in addition: int + string".into())
        );
    }

    #[test]
    fn failing_type_check2() {
        let source_code = r#"
        let x = 5;
        if x {
            print("hello world")
        }
        "#;
        let ast = create_ast(source_code);
        assert_eq!(
            check_types(&ast),
            Err("If condition must be boolean".into())
        );
    }

    #[test]
    fn complicated_type_check() {
        let source_code = r#"
        fn add(a: int, b: int) -> int {
            a + b
        }
        let x = add(5, 10);
        let xx  = x + 10;

        fn add_float(a: float, b: float) -> float {
            a + b
        }
        let y = add_float(5.5, 10.5);
        let yy = y + 0.5;

        fn add_string(a: string, b: string) -> string {
            a + b
        }
        let z = add_string("hello ", "world");
        let zz = z + "!";
        "#;

        let ast = create_ast(source_code);
        assert_eq!(check_types(&ast), Ok(()));
    }

    #[test]
    fn a_float_is_not_an_int() {
        let source_code = r#"
        let x = 5;
        let y = 10.5;
        let z = x + y;
        "#;
        let ast = create_ast(source_code);
        assert_eq!(
            check_types(&ast),
            Err("Type mismatch in addition: int + float".into())
        );
    }

    #[test]
    fn higher_order_function_type_check() {
        let source_code = r#"

        fn double_adder(add1: fn(int, int) -> int, add2: fn(int, int) -> int) -> int {
            add1(5, 10) + add2(5, 10)
        }

        fn simple_adder(a: int, b: int) -> int {
            a + b
        }
        fn simple_adder2(a: int, b: int) -> int {
            a + b
        }

        let xx = simple_adder

        let x = double_adder(xx, simple_adder2);
        let y = x + 10;
        
        y
        "#;
        let ast = create_ast(source_code);
        assert_eq!(check_types(&ast), Ok(()));
    }

    #[test]
    fn recursive() {
        let source_code = r#"

        fn doubler(n: int, double: int) -> int{
            if double == 1 {
                n
            } else{
             let next = double - 1;
                doubler(n,next)
            }
        }

        let x = doubler(10, 3)
        if x == 10{
        print(x)
        }
        "#;
        let ast = create_ast(source_code);
        assert_eq!(check_types(&ast), Ok(()));
    }

    #[test]
    fn recursion_test() {
        let source_code = r#"
        fn factorial(n: int) -> int{
            if n < 1{
                1
            }else{

                n * factorial({n-1})
            }
        }

        let x = factorial(5);
        if x > 10{
            print(x)
        }
        "#;

        let ast = create_ast(source_code);
        assert_eq!(check_types(&ast), Ok(()))
    }

    #[test]
    fn type_check_lambda() {
        let source_code = r#"
        let adder = |n:int| -> int{
            n+1
        }

        adder(6) + 1
        "#;

        let ast = create_ast(source_code);
        assert_eq!(check_types(&ast), Ok(()))
    }
}
