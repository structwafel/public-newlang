use core::fmt;

use super::*;

#[derive(Clone, Debug, PartialEq)]
pub enum AstValue {
    Unit,
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Function {
        name: String,
        args: Vec<TypedArg>,
        body: Box<Expression>,
        return_type: Type,
    },
}

impl AstValue {
    pub fn type_of(&self) -> Type {
        match self {
            AstValue::Unit => Type::Unit,
            AstValue::Integer(_) => Type::Integer,
            AstValue::Float(_) => Type::Float,
            AstValue::Boolean(_) => Type::Boolean,
            AstValue::String(_) => Type::String,
            AstValue::Function {
                args, return_type, ..
            } => Type::Function {
                args: args.iter().map(|arg| arg.t.clone()).collect(),
                return_type: Box::new(return_type.clone()),
            },
        }
    }

    pub fn try_add(&self, other: &AstValue) -> Option<AstValue> {
        match (self, other) {
            (AstValue::Integer(x), AstValue::Integer(y)) => Some(AstValue::Integer(x + y)),
            (AstValue::Float(x), AstValue::Float(y)) => Some(AstValue::Float(x + y)),
            (AstValue::String(x), AstValue::String(y)) => Some(AstValue::String(x.clone() + y)),
            _ => None,
        }
    }

    pub fn try_mul(&self, other: &AstValue) -> Option<AstValue> {
        match (self, other) {
            (AstValue::Integer(x), AstValue::Integer(y)) => Some(AstValue::Integer(x * y)),
            (AstValue::Float(x), AstValue::Float(y)) => Some(AstValue::Float(x * y)),
            _ => None,
        }
    }

    pub fn add(&self, other: &AstValue) -> AstValue {
        match (self, other) {
            (AstValue::Integer(a), AstValue::Integer(b)) => AstValue::Integer(a + b),
            (AstValue::Float(a), AstValue::Float(b)) => AstValue::Float(a + b),
            (AstValue::String(a), AstValue::String(b)) => AstValue::String(format!("{}{}", a, b)),
            _ => panic!("Invalid operation: {:?} + {:?}", self, other),
        }
    }

    pub fn sub(&self, other: &AstValue) -> AstValue {
        match (self, other) {
            (AstValue::Integer(a), AstValue::Integer(b)) => AstValue::Integer(a - b),
            (AstValue::Float(a), AstValue::Float(b)) => AstValue::Float(a - b),
            _ => panic!("Invalid operation: {:?} - {:?}", self, other),
        }
    }

    pub fn mul(&self, other: &AstValue) -> AstValue {
        match (self, other) {
            (AstValue::Integer(a), AstValue::Integer(b)) => AstValue::Integer(a * b),
            (AstValue::Float(a), AstValue::Float(b)) => AstValue::Float(a * b),
            _ => panic!("Invalid operation: {:?} * {:?}", self, other),
        }
    }

    pub fn div(&self, other: &AstValue) -> AstValue {
        match (self, other) {
            (AstValue::Integer(a), AstValue::Integer(b)) => AstValue::Integer(a / b),
            (AstValue::Float(a), AstValue::Float(b)) => AstValue::Float(a / b),
            _ => panic!("Invalid operation: {:?} / {:?}", self, other),
        }
    }

    pub fn gt(&self, other: &AstValue) -> AstValue {
        match (self, other) {
            (AstValue::Integer(a), AstValue::Integer(b)) => AstValue::Boolean(a > b),
            (AstValue::Float(a), AstValue::Float(b)) => AstValue::Boolean(a > b),
            _ => panic!("Invalid operation: {:?} / {:?}", self, other),
        }
    }
}

impl fmt::Display for AstValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
