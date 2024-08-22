use ast::{Ast, Node};

use crate::lexer::Token;

pub mod expressions;
pub mod ast;
pub mod errors;

pub fn generate_ast(tokens: Vec<Token>) -> Vec<Node> {
    let mut ast_tree: Vec<Node> = vec![];
    let mut ast = Ast::new(tokens);
    loop {
        if let Some(node) = ast.parse_line() { ast_tree.push(node); }
        else { break; }
    }
    ast_tree
}