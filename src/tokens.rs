use logos::Logos;
use std::fmt; // to implement the Display trait
use std::num::{ParseFloatError, ParseIntError};

#[derive(Default, Debug, Clone, PartialEq)]
pub enum LexicalError {
    InvalidInteger(ParseIntError),

    InvalidFloat(ParseFloatError),
    #[default]
    // InvalidToken(String),
    InvalidToken,
}

impl From<ParseIntError> for LexicalError {
    fn from(err: ParseIntError) -> Self {
        LexicalError::InvalidInteger(err)
    }
}

impl From<ParseFloatError> for LexicalError {
    fn from(err: ParseFloatError) -> Self {
        LexicalError::InvalidFloat(err)
    }
}

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(skip r"[ \t\n\f]+", skip r"//.*\n?", error = LexicalError)]
pub enum Token {
    #[token("let")]
    KeywordVar,
    #[token("print")]
    KeywordPrint,

    // types
    #[token("int")]
    KeywordInt,
    #[token("float")]
    KeywordFloat,
    #[token("string")]
    KeywordString,
    #[token("bool")]
    KeywordBool,

    #[token("fn")]
    KeywordFn,
    #[token("return")]
    KeywordReturn,
    #[token("if")]
    KeywordIf,
    #[token("else")]
    KeywordElse,

    // literals
    #[regex("[_a-zA-Z][_0-9a-zA-Z]*", |lex| lex.slice().to_string())]
    Identifier(String),
    #[regex("-?[0-9]*", |lex| lex.slice().parse())]
    IntLiteral(i64),
    #[regex("[0-9]*\\.[0-9]+", |lex| lex.slice().parse())]
    FloatLiteral(f64),
    #[regex("\"[^\"]*\"", |lex| lex.slice()[1..lex.slice().len() - 1].to_string())]
    StringLiteral(String),
    #[regex("true|false", |lex| lex.slice() == "true")]
    BoolLiteral(bool),

    #[token("->")]
    KeywordArrow,
    // #[token("()")]
    // Unit,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(",")]
    Comma,
    #[token("=")]
    Assign,
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,

    #[token("+")]
    OperatorAdd,
    #[token("-")]
    OperatorSub,
    #[token("*")]
    OperatorMul,
    #[token("/")]
    OperatorDiv,
    #[token("==")]
    OperatorEq,
    #[token("!=")]
    OperatorNeq,
    #[token("<")]
    OperatorLt,
    #[token("<=")]
    OperatorLe,
    #[token(">")]
    OperatorGt,
    #[token(">=")]
    OperatorGe,

    #[token("&&")]
    OperatorAnd,
    #[token("||")]
    OperatorOr,

    #[token{"|"}]
    Pipe,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
