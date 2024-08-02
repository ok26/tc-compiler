use tc_compiler::{
    gen::gen_instructions,
    lexer::tokenize,
};

fn main() {
    let instructions = gen_instructions(tokenize(include_str!("dance.asm").to_string()));
    for (i, instruction) in instructions.iter().enumerate() {
        if i != 0 && i % 4 == 0 {
            println!("");
        }
        print!("0x{:X} ", instruction);
    }
}
