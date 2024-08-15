use asm_compiler::compile_asm;
use hl_compiler::compile_hl;
use emulator::Emulator;

use std::fs::File;
use std::io::Write;

fn main() {
    let asm_instructions = compile_hl(include_str!("conditions.hl"));
    let mut asm_out_file = File::create("src/asm_out.asm").unwrap();
    write!(asm_out_file, "{}", asm_instructions);
    
    let machine_instructions = compile_asm(asm_instructions);
    let mut out = String::new();
    let mut out_file = File::create("src/out").unwrap();
    for (i, instruction) in machine_instructions.iter().enumerate() {
        if i != 0 && i % 4 == 0 {
            writeln!(out_file, "{}", out);
            out = String::new();
        }
        out.push_str(format!("0x{:X} ", instruction).as_str());
    }
    
    let mut emulator = Emulator::new();
    emulator.run_script(machine_instructions);
    emulator.print_info(20);
}