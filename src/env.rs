use crate::{
    ast::{Expression, Operator, Type, TypedArg, AstValue},
    linkedlist::LinkedList,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    statements: LinkedList<(String, AstValue)>,
}

// default functions
fn default_functions() -> Vec<(String, AstValue)> {
    vec![(
        "equal".into(),
        AstValue::Function {
            name: "equal".to_owned(),
            args: vec![
                TypedArg {
                    name: "a".to_owned(),
                    t: Type::Integer,
                },
                TypedArg {
                    name: "b".to_owned(),
                    t: Type::Integer,
                },
            ],
            body: Box::new(Expression::BinaryOperation {
                lhs: Box::new(Expression::Variable("a".into())),
                operator: Operator::Eq,
                rhs: Box::new(Expression::Variable("b".into())),
            }),
            return_type: Type::Boolean,
        },
    )]
}
impl Env {
    pub fn new() -> Self {
        let mut env = Self {
            statements: LinkedList::default(),
        };

        // add default functions
        for (name, value) in default_functions() {
            env.statements = env.statements.prepend((name, value));
        }

        env
    }

    pub fn get(&self, name: &str) -> Option<&AstValue> {
        for (var_name, value) in self.statements.iter() {
            if var_name == name {
                return Some(value);
            }
        }
        None
    }

    pub fn set(self, name: String, value: AstValue) -> Self {
        Self {
            statements: self.statements.prepend((name, value)),
        }
    }
}
