mod lexer;
mod ast;
mod gen;

use lexer::{Lexer, LexerErrorType, Token, TokenType};
use ast::Ast;
use gen::gen::Gen;

pub fn compile_hl(code: &str) -> String {

    let mut lexer = Lexer::new(code);
    let mut tokens: Vec<Token> = vec![];
    let mut errors_found = 0;
    loop {
        match lexer.next_token() {
            Ok(token) => {
                tokens.push(token.clone());
                if token.ty == TokenType::EOF { break; }
                else { println!("{}", token)}
            },
            Err(error) => {
                println!("{}", error);
                errors_found += 1;
                if error.ty == LexerErrorType::UnevenFilePunctuation { break; }
            }
        }
    }

    if errors_found != 0 { return String::new(); }

    let mut ast = Ast::new(tokens);
    let ast_tree = ast.generate_ast();
    for node in ast_tree.clone() {
        println!("{}", node);
    }

    let mut asm_gen = Gen::new(ast_tree);
    asm_gen.generate_asm()
}