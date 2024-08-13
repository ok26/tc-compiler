mod lexer;
mod ast;
mod gen;

use lexer::{Lexer, Token, TokenType};
use ast::Ast;
use gen::gen::Gen;

pub fn compile_hl(code: &str) -> String {

    let mut lexer = Lexer::new(code);
    let mut tokens: Vec<Token> = vec![];
    loop {
        match lexer.next_token() {
            Ok(token) => {
                tokens.push(token.clone());
                if token.ty == TokenType::EOF { break; }
                else { println!("{}", token)}
            },
            Err(error) => {
                println!("{}", error);
            }
        }
    }
    
    let mut ast = Ast::new(tokens);
    let ast_tree = ast.generate_ast();
    for node in ast_tree.clone() {
        println!("{:?}", node);
    }

    let mut asm_gen = Gen::new(ast_tree);
    asm_gen.generate_asm()
}