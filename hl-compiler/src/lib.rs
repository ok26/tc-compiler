mod lexer;
mod ast;
mod gen;
mod semantics;

use std::collections::HashMap;

use lexer::{Lexer, LexerErrorType, Token, TokenType};
use ast::{Ast, Node, NodeType};
use semantics::SemanticAnalysis;
use gen::gen::Gen;

fn find_ast_errors(nodes: Vec<Node>) -> Vec<Node> {
    let mut errors = vec![];
    for node in nodes {
        errors.append(&mut match node.ty {
            NodeType::For { variable: _, condition: _, loop_increment: _, body } => find_ast_errors(body),
            NodeType::Function { identifier: _, arguments: _, body } => find_ast_errors(body),
            NodeType::If { condition: _, body } => find_ast_errors(body),
            NodeType::IfElse { condition: _, body, else_body } => {
                let mut out = find_ast_errors(body);
                out.append(&mut find_ast_errors(else_body));
                out
            },
            NodeType::While { condition: _, body } => find_ast_errors(body),
            NodeType::Out { .. } | NodeType::Return { .. } | NodeType::VariableAssignment { .. } | NodeType::VariableDeclaration { .. } => vec![],
            NodeType::Error { .. } => vec![node]
        });
    }
    errors
}

pub fn compile_hl(code: &str) -> String {

    let mut lexer = Lexer::new(code);
    let mut tokens: Vec<Token> = vec![];
    let mut lexer_errors = vec![];
    loop {
        match lexer.next_token() {
            Ok(token) => {
                tokens.push(token.clone());
                if token.ty == TokenType::EOF { break; }
            },
            Err(error) => {
                lexer_errors.push(error.clone());
                if error.ty == LexerErrorType::UnevenFilePunctuation { break; }
            }
        }
    }

    if lexer_errors.len() != 0 {
        for error in lexer_errors {
            println!("{}", error);
        }
        return String::new();
    }

    let mut ast = Ast::new(tokens);
    let ast_tree = ast.generate_ast();
    let ast_errors = find_ast_errors(ast_tree.clone());

    if ast_errors.len() != 0 {
        for error in ast_errors {
            println!("{}", error);
        }
        return String::new();
    }

    let mut functions = HashMap::new();
    for node in &ast_tree { match &node.ty {
        NodeType::Function { identifier, arguments, body: _ } => { functions.insert(identifier.clone(), arguments.clone()); },
        _ => {}
    }}

    let mut sem_analysis = SemanticAnalysis::new(functions.clone());
    let semantic_errors = sem_analysis.analyse(&ast_tree);

    if semantic_errors.len() != 0 {
        for error in semantic_errors {
            println!("{}", error);
        }
        return String::new();
    }

    let mut asm_gen = Gen::new(ast_tree, functions);
    asm_gen.generate_asm()
}