pub mod ast;
pub mod env;
pub mod lexer;
pub mod linkedlist;
pub mod stack;
pub mod tokens;

use ast::Expression;
use grammar::ScriptParser;
use lalrpop_util::lalrpop_mod;
use lexer::Lexer;

lalrpop_mod!(pub grammar);

pub fn create_ast(source_code: &str) -> Vec<Expression> {
    let mut counter = 0;
    let lexer = Lexer::new(&source_code);
    let parser = ScriptParser::new();
    parser.parse(&mut counter, lexer).unwrap()
}
