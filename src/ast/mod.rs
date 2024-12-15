mod expression;
pub mod typechecker;
pub mod value;

// pub use expression::ElseClause;
pub use expression::Expression;
pub use value::AstValue;

use crate::env::Env;

#[derive(Clone, Debug, PartialEq)]
pub enum Operator {
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Integer,
    Float,
    Boolean,
    String,
    Unit,
    Function {
        args: Vec<Type>,
        return_type: Box<Type>,
    },
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Integer => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Boolean => write!(f, "bool"),
            Type::String => write!(f, "string"),
            Type::Unit => write!(f, "unit"),
            Type::Function { args, return_type } => {
                let args_str: Vec<String> = args.iter().map(|arg| arg.to_string()).collect();
                write!(f, "fn({}) -> {}", args_str.join(", "), return_type)
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedArg {
    pub name: String,
    pub t: Type,
}

fn typed_args_to_vec_type(args: &[TypedArg]) -> Vec<Type> {
    args.iter().map(|arg| arg.t.clone()).collect()
}

/// Directl evaluation of the ast, currently not complete
fn _evaluate_ast(ast: &[Expression]) -> AstValue {
    let mut env = Env::new();
    let mut last_value = AstValue::Unit;

    for statement in ast.iter() {
        (last_value, env) = statement.eval(env);
    }

    last_value
}
