#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use std::{boxed, collections::HashSet};

use super::{AstValue, Operator, Type, TypedArg};
use crate::env::Env;

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Value(AstValue),
    Variable(String),
    VariableAssignment {
        name: String,
        value: Box<Expression>,
    },
    IfElse {
        condition: Box<Expression>,
        then_branch: Box<Expression>,
        else_branch: Option<Box<Expression>>,
    },
    FunctionDefinition {
        name: String,
        args: Vec<TypedArg>,
        body: Box<Expression>,
        return_type: Type,
    },
    BinaryOperation {
        lhs: Box<Expression>,
        operator: Operator,
        rhs: Box<Expression>,
    },
    // "built in function"
    // this should probably be different, a "native function call" which is an enum of all native ones
    // built in functinos can be added to env before runtime
    Print {
        value: Box<Expression>,
    },
    FunctionCall {
        name: String,
        args: Vec<Expression>,
    },
    Block {
        expressions: Vec<Expression>,
    },
    Lambda {
        id: usize,
        args: Vec<TypedArg>,
        body: Box<Expression>,
        return_type: Type,
    },
    Discard {
        value: Box<Expression>,
    },
}

impl Expression {
    /// evaluate an expression directly.
    /// Currently not in use anymore
    pub fn eval(&self, env: Env) -> (AstValue, Env) {
        match self {
            Expression::Value(value) => (value.clone(), env),
            Expression::Variable(name) => {
                let value = env
                    .get(&name)
                    .expect(&format!("var: {name} not in env"))
                    .clone();

                (value, env)
            }
            // todo, move these things to value instead of expression
            Expression::BinaryOperation { lhs, operator, rhs } => {
                // should you keep the env and pass it to the rhs?
                let (lhs2, env) = lhs.eval(env);
                let (rhs, env) = rhs.eval(env);
                let result = match operator {
                    Operator::Add => match (lhs2, rhs) {
                        (AstValue::Integer(lhs), AstValue::Integer(rhs)) => {
                            AstValue::Integer(lhs + rhs)
                        }
                        (AstValue::Float(lhs), AstValue::Float(rhs)) => AstValue::Float(lhs + rhs),
                        (AstValue::Integer(lhs), AstValue::Float(rhs)) => {
                            AstValue::Float(lhs as f64 + rhs)
                        }
                        (AstValue::Float(lhs), AstValue::Integer(rhs)) => {
                            AstValue::Float(lhs + rhs as f64)
                        }
                        // simple concat?
                        (AstValue::String(lhs), AstValue::String(rhs)) => {
                            AstValue::String(lhs + &rhs)
                        }
                        _ => panic!("Invalid types for addition"),
                    },
                    Operator::Sub => match (lhs2, rhs) {
                        (AstValue::Integer(lhs), AstValue::Integer(rhs)) => {
                            AstValue::Integer(lhs - rhs)
                        }
                        (AstValue::Float(lhs), AstValue::Float(rhs)) => AstValue::Float(lhs - rhs),
                        (AstValue::Integer(lhs), AstValue::Float(rhs)) => {
                            AstValue::Float(lhs as f64 - rhs)
                        }
                        (AstValue::Float(lhs), AstValue::Integer(rhs)) => {
                            AstValue::Float(lhs - rhs as f64)
                        }
                        _ => panic!("Invalid types for subtraction"),
                    },
                    Operator::Mul => match (lhs2, rhs) {
                        (AstValue::Integer(lhs), AstValue::Integer(rhs)) => {
                            AstValue::Integer(lhs * rhs)
                        }
                        (AstValue::Float(lhs), AstValue::Float(rhs)) => AstValue::Float(lhs * rhs),
                        (AstValue::Integer(lhs), AstValue::Float(rhs)) => {
                            AstValue::Float(lhs as f64 * rhs)
                        }
                        (AstValue::Float(lhs), AstValue::Integer(rhs)) => {
                            AstValue::Float(lhs * rhs as f64)
                        }
                        _ => panic!("Invalid types for multiplication"),
                    },
                    Operator::Div => match (lhs2, rhs) {
                        (AstValue::Integer(lhs), AstValue::Integer(rhs)) => {
                            AstValue::Integer(lhs / rhs)
                        }
                        (AstValue::Float(lhs), AstValue::Float(rhs)) => AstValue::Float(lhs / rhs),
                        (AstValue::Integer(lhs), AstValue::Float(rhs)) => {
                            AstValue::Float(lhs as f64 / rhs)
                        }
                        (AstValue::Float(lhs), AstValue::Integer(rhs)) => {
                            AstValue::Float(lhs / rhs as f64)
                        }
                        _ => panic!("Invalid types for division"),
                    },
                    Operator::Eq => AstValue::Boolean(rhs == rhs),
                    Operator::Neq => AstValue::Boolean(rhs != rhs),
                    Operator::Lt => match (lhs2, rhs) {
                        (AstValue::Integer(lhs), AstValue::Integer(rhs)) => {
                            AstValue::Boolean(lhs < rhs)
                        }
                        (AstValue::Float(lhs), AstValue::Float(rhs)) => {
                            AstValue::Boolean(lhs < rhs)
                        }
                        (AstValue::Integer(lhs), AstValue::Float(rhs)) => {
                            AstValue::Boolean((lhs as f64) < rhs)
                        }
                        (AstValue::Float(lhs), AstValue::Integer(rhs)) => {
                            AstValue::Boolean(lhs < rhs as f64)
                        }
                        _ => panic!("Invalid types for less than"),
                    },
                    Operator::Gt => match (lhs2, rhs) {
                        (AstValue::Integer(lhs), AstValue::Integer(rhs)) => {
                            AstValue::Boolean(lhs > rhs)
                        }
                        (AstValue::Float(lhs), AstValue::Float(rhs)) => {
                            AstValue::Boolean(lhs > rhs)
                        }
                        (AstValue::Integer(lhs), AstValue::Float(rhs)) => {
                            AstValue::Boolean((lhs as f64) > rhs)
                        }
                        (AstValue::Float(lhs), AstValue::Integer(rhs)) => {
                            AstValue::Boolean(lhs > rhs as f64)
                        }
                        _ => panic!("Invalid types for greater than"),
                    },
                    Operator::Le => match (lhs2, rhs) {
                        (AstValue::Integer(lhs), AstValue::Integer(rhs)) => {
                            AstValue::Boolean(lhs <= rhs)
                        }
                        (AstValue::Float(lhs), AstValue::Float(rhs)) => {
                            AstValue::Boolean(lhs <= rhs)
                        }
                        (AstValue::Integer(lhs), AstValue::Float(rhs)) => {
                            AstValue::Boolean((lhs as f64) <= rhs)
                        }
                        (AstValue::Float(lhs), AstValue::Integer(rhs)) => {
                            AstValue::Boolean(lhs <= rhs as f64)
                        }
                        _ => panic!("Invalid types for less than or equal"),
                    },
                    Operator::Ge => match (lhs2, rhs) {
                        (AstValue::Integer(lhs), AstValue::Integer(rhs)) => {
                            AstValue::Boolean(lhs >= rhs)
                        }
                        (AstValue::Float(lhs), AstValue::Float(rhs)) => {
                            AstValue::Boolean(lhs >= rhs)
                        }
                        (AstValue::Integer(lhs), AstValue::Float(rhs)) => {
                            AstValue::Boolean((lhs as f64) >= rhs)
                        }
                        (AstValue::Float(lhs), AstValue::Integer(rhs)) => {
                            AstValue::Boolean(lhs >= rhs as f64)
                        }
                        _ => panic!("Invalid types for greater than or equal"),
                    },
                    Operator::And => match (lhs2, rhs) {
                        (AstValue::Boolean(lhs), AstValue::Boolean(rhs)) => {
                            AstValue::Boolean(lhs && rhs)
                        }
                        _ => panic!("Invalid types for AND"),
                    },
                    Operator::Or => match (lhs2, rhs) {
                        (AstValue::Boolean(lhs), AstValue::Boolean(rhs)) => {
                            AstValue::Boolean(lhs || rhs)
                        }
                        _ => panic!("Invalid types for OR"),
                    },
                };
                (result, env)
            }
            Expression::Block { expressions } => {
                let block_env = env.clone();

                let (last_value, _) = expressions.iter().fold(
                    (AstValue::Unit, block_env),
                    |(last_value, block_env), expression| expression.eval(block_env),
                );

                (last_value, env)
            }

            // "built in function"
            Expression::Print { value } => {
                let (value, _) = value.eval(env.clone());
                println!("{:?}", value);
                (AstValue::Unit, env)
            }
            Expression::VariableAssignment { name, value } => {
                let (value, env) = value.eval(env);

                let new_env = env.set(name.clone(), value.clone());

                (value, new_env)
            }
            Expression::Discard { value } => {
                let (_, env) = value.eval(env);
                (AstValue::Unit, env)
            }
            Expression::IfElse {
                condition,
                then_branch,
                else_branch,
            } => {
                // todo, check type of branches to be the same

                let (condition, _) = condition.eval(env.clone());

                if matches!(condition, AstValue::Boolean(true)) {
                    let (value, _) = then_branch.eval(env.clone());
                    return (value, env);
                }

                if let Some(else_branch) = else_branch {
                    let (value, _) = else_branch.eval(env.clone());
                    return (value, env);
                }

                (AstValue::Unit, env)
            }
            // Expression::FunctionDefinition { name, args, body } => todo!(),
            Expression::FunctionCall {
                name,
                args: call_args,
            } => {
                // get function from env
                let function = env
                    .get(&name)
                    .expect(&format!("function {} not found", name));

                match function {
                    AstValue::Function {
                        name,
                        args,
                        body,
                        return_type,
                    } => {
                        assert!(
                            args.len() == call_args.len(),
                            "function {name} has wrong number of arguments"
                        );

                        // set the call_args in the env, based on the args name
                        let mut call_env = env.clone();

                        for (arg, call_arg) in args.iter().zip(call_args.iter()) {
                            // evaluate the call_arg
                            let (evaluated_call_arg, _) = call_arg.eval(env.clone());

                            call_env = call_env.set(arg.name.clone(), evaluated_call_arg);
                        }

                        // evaluate the body
                        let (value, _) = body.eval(call_env);

                        (value, env)
                    }
                    _ => panic!("value {name} is not a function {function:?}"),
                }
            }
            Expression::FunctionDefinition {
                name,
                args,
                body,
                return_type,
            } => {
                // assert!(&body.type_of(env) == return_type)
                // set function in env
                (
                    AstValue::Unit,
                    env.set(
                        name.clone(),
                        AstValue::Function {
                            name: name.clone(),
                            args: args.clone(),
                            body: body.clone(),
                            return_type: return_type.clone(),
                        },
                    ),
                )
            }
            Expression::Lambda {
                id,
                args,
                body,
                return_type,
            } => todo!(),
        }
    }

    /// optimize an expression
    pub fn optimize(self) -> Expression {
        let expr = self.constant_fold().algebraic_simplify();

        match expr {
            Expression::Value(_) => expr,
            Expression::Variable(_) => expr,
            Expression::VariableAssignment { name, value } => Expression::VariableAssignment {
                name,
                value: value.optimize().into(),
            },
            Expression::IfElse {
                condition,
                then_branch,
                else_branch,
            } => {
                // if the condition can be optimized to true/false. no need to keep both branches

                let optimized_condition = condition.optimize();

                match optimized_condition {
                    Expression::Value(AstValue::Boolean(true)) => then_branch.optimize(),
                    Expression::Value(AstValue::Boolean(false)) => {
                        if let Some(else_branch) = else_branch {
                            else_branch.optimize()
                        } else {
                            Expression::Value(AstValue::Unit)
                        }
                    }
                    _ => {
                        // cannot optimize branching. have to do both
                        let optimized_then = then_branch.optimize();
                        let optimized_else = else_branch.map(|b| b.optimize());

                        // if they optimize to the same, just use one
                        if let Some(else_branch) = &optimized_else {
                            if *else_branch == optimized_then {
                                return optimized_then;
                            }
                        }

                        Expression::IfElse {
                            condition: optimized_condition.into(),
                            then_branch: optimized_then.into(),
                            else_branch: optimized_else.map(Box::new),
                        }
                    }
                }
            }
            Expression::FunctionDefinition {
                name,
                args,
                body,
                return_type,
            } => {
                // if the function is "consant" we don't need to have a function but a variable definition?
                // ? not sure that is implementable at the moment

                // for now just optimize the body
                let optimized_body = body.optimize();

                Expression::FunctionDefinition {
                    name,
                    args,
                    body: optimized_body.into(),
                    return_type,
                }
            }
            // todo other optimizationsc
            _ => self,
            // Expression::BinaryOperation { lhs, operator, rhs } => todo!(),
            // Expression::Print { value } => todo!(),
            // Expression::FunctionCall { name, args } => todo!(),
            // Expression::Block { expressions } => todo!(),
            // Expression::Discard { value } => todo!(),
        }
    }

    /// fold all constants
    /// todo complete this operation
    fn constant_fold(&self) -> Expression {
        let i = |e: i32, t: i32| {};
        match self {
            Expression::BinaryOperation { lhs, operator, rhs } => {
                if let (Expression::Value(v1), Expression::Value(v2)) = (&**lhs, &**rhs) {
                    if let Some(result) = evaluate_const_operation(v1, operator, v2) {
                        return Expression::Value(result);
                    }
                }
                self.clone()
            }
            _ => self.clone(),
        }
    }

    // simplify some algebraic operations
    // todo, add all cases
    fn algebraic_simplify(&self) -> Expression {
        match self {
            Expression::BinaryOperation {
                lhs,
                operator: Operator::Add,
                rhs,
            } => {
                if let Expression::Value(AstValue::Integer(0)) = **rhs {
                    return (**lhs).clone();
                }
                // todo all other patterns
                self.clone()
            }
            _ => self.clone(),
        }
    }

    pub fn find_free_variables(&self) -> HashSet<String> {
        let mut vars = HashSet::new();

        match self {
            Expression::Value(ast_value) => {}
            Expression::Variable(name) => {
                vars.insert(name.clone());
            }
            Expression::VariableAssignment { name, value } => {
                vars.extend(value.find_free_variables());
            }
            Expression::IfElse {
                condition,
                then_branch,
                else_branch,
            } => {
                vars.extend(condition.find_free_variables());
                vars.extend(then_branch.find_free_variables());
                if let Some(else_) = else_branch {
                    vars.extend(else_.find_free_variables());
                }
            }
            Expression::FunctionDefinition {
                name,
                args,
                body,
                return_type,
            } => {
                let mut body_vars = body.find_free_variables();
                // Remove parameters from free variables
                for arg in args {
                    body_vars.remove(&arg.name);
                }
                vars.extend(body_vars);
            }
            Expression::BinaryOperation { lhs, operator, rhs } => {
                vars.extend(lhs.find_free_variables());
                vars.extend(rhs.find_free_variables());
            }
            Expression::Print { value } => {
                vars.extend(value.find_free_variables());
            }
            Expression::FunctionCall { name, args } => {
                for arg in args {
                    vars.extend(arg.find_free_variables());
                }
            }
            Expression::Block { expressions } => {
                for expr in expressions {
                    vars.extend(expr.find_free_variables());
                }
            }
            Expression::Lambda {
                id,
                args,
                body,
                return_type,
            } => {
                let mut body_vars = body.find_free_variables();
                // Remove parameters from free variables
                for arg in args {
                    body_vars.remove(&arg.name);
                }
                vars.extend(body_vars);
            }
            Expression::Discard { value } => {
                vars.extend(value.find_free_variables());
            }
        }
        vars
    }
}

// Helper for constant folding
fn evaluate_const_operation(v1: &AstValue, op: &Operator, v2: &AstValue) -> Option<AstValue> {
    match (v1, op, v2) {
        (x, Operator::Add, y) => x.try_add(y),
        (x, Operator::Mul, y) => x.try_mul(y),
        // (Value::)
        // todo, add other operators here
        _ => None,
    }
}
