use asm_compiler::compile_asm;
use hl_compiler::compile_hl;
use emulator::Emulator;

fn main() {
    let input_code = include_str!("tests.tc");
    let asm_instructions = compile_hl(input_code);
    let machine_instructions = compile_asm(asm_instructions);
    let mut emulator = Emulator::new();
    emulator.run_script(machine_instructions);
}