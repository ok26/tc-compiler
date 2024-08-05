mod lexer;
mod gen;

use lexer::tokenize;
use gen::gen_instructions;

pub fn compile_asm(code: String) -> Vec<u32> {
    gen_instructions(tokenize(code))
}