mod lexer;
mod ast;
mod gen;

use lexer::{Lexer, Token, TokenType};

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
    code.to_string()
}