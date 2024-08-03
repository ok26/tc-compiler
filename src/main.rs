use asm_compiler::compile_asm;

fn main() {
    let instructions = compile_asm(include_str!("dance.asm"));
    for (i, instruction) in instructions.iter().enumerate() {
        if i != 0 && i % 4 == 0 {
            println!("");
        }
        print!("0x{:X} ", instruction);
    }
}