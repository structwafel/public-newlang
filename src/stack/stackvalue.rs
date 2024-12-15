use std::{
    cmp::Ordering,
    collections::HashMap,
    ops::{Add, BitAnd, BitOr, Div, Mul, Sub},
};

use crate::ast::AstValue;

#[derive(Clone, PartialEq, Debug)]
pub enum StackValue {
    Unit,
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
    Function {
        name: String,
        captures: HashMap<String, StackValue>,
    },
}

impl From<AstValue> for StackValue {
    fn from(value: AstValue) -> Self {
        match value {
            AstValue::Unit => StackValue::Unit,
            AstValue::Integer(i) => StackValue::Integer(i),
            AstValue::Float(f) => StackValue::Float(f),
            AstValue::Boolean(b) => StackValue::Boolean(b),
            AstValue::String(s) => StackValue::String(s),
            AstValue::Function { name, .. } => StackValue::Function {
                name,
                captures: HashMap::new(),
            },
        }
    }
}

macro_rules! impl_arith {
    ($trait:ident, $method:ident, $op:tt) => {
        impl $trait for StackValue {
            type Output = StackValue;

            fn $method(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (StackValue::Integer(l), StackValue::Integer(r)) => StackValue::Integer(l $op r),
                    (StackValue::Float(l), StackValue::Float(r)) => StackValue::Float(l $op r),
                    _ => panic!("unsupported operation"),
                }
            }
        }
    };
}

impl_arith!(Add, add, +);
impl_arith!(Sub, sub, -);
impl_arith!(Mul, mul, *);
impl_arith!(Div, div, /);

macro_rules! impl_logic {
    ($trait:ident, $method:ident, $op:tt) => {
        impl $trait for StackValue {
            type Output = StackValue;

            fn $method(self, rhs: Self) -> Self::Output {
                match (self, rhs) {
                    (StackValue::Boolean(l), StackValue::Boolean(r)) => StackValue::Boolean(l $op r),
                    _ => panic!("unsupported operation"),
                }
            }
        }
    };
}

impl_logic!(BitAnd, bitand, &);
impl_logic!(BitOr, bitor, |);

impl PartialOrd for StackValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (StackValue::Integer(l), StackValue::Integer(r)) => l.partial_cmp(r),
            (StackValue::Float(l), StackValue::Float(r)) => l.partial_cmp(r),
            _ => None,
        }
    }
}
