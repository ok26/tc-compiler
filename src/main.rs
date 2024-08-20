use asm_compiler::compile_asm;
use hl_compiler::compile_hl;
use emulator::Emulator;

fn main() {
    let asm_instructions = compile_hl(include_str!("examples.hl"));
    // println!("{}\n", asm_instructions.clone());
    let machine_instructions = compile_asm(asm_instructions);

//     for (i, instruction) in machine_instructions.iter().enumerate() {
//         if i != 0 && i % 4 == 0 {
//             println!("");
//         }
//             print!("0x{:X} ", instruction);
//     }
//     println!("\n\n");
    
    let mut emulator = Emulator::new();
    emulator.run_script(machine_instructions);
}