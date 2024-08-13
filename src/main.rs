use asm_compiler::compile_asm;
use hl_compiler::compile_hl;

fn main() {
    let asm_instructions = compile_hl(include_str!("expressions.hl"));
    println!("{}", asm_instructions);
    let machine_instructions = compile_asm(asm_instructions);
    for (i, instruction) in machine_instructions.iter().enumerate() {
        if i != 0 && i % 4 == 0 {
            println!("");
        }
        print!("0x{:X} ", instruction);
    }
}