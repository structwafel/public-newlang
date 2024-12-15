use crate::ast::Operator;

use super::{FunctionName, Label, StackValue};

#[derive(Debug, PartialEq, Clone)]
pub enum StackIR {
    Const(StackValue),
    Load(String),
    Store(String),
    Jump(Label),
    JumpIfFalse(Label),
    Label(Label),
    Call(FunctionName), // function
    EnterBlock,         // entiring a scoped thing
    ExitBlock,          // exiting a scoped thing
    Return,
    Pop,
    Print,
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
    And,
    Or,
}

impl StackIR {
    pub fn from_operator(operator: &Operator) -> Self {
        match operator {
            Operator::Add => Self::Add,
            Operator::Sub => Self::Sub,
            Operator::Mul => Self::Mul,
            Operator::Div => Self::Div,
            Operator::Eq => Self::Eq,
            Operator::Neq => Self::Neq,
            Operator::Lt => Self::Lt,
            Operator::Gt => Self::Gt,
            Operator::Le => Self::Le,
            Operator::Ge => Self::Ge,
            Operator::And => Self::Add,
            Operator::Or => Self::Or,
        }
    }
}
