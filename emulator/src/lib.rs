// Computer instructions
// 
// In general:
// OPCODE ARG1 ARG2 RESULT
//
// ARG1 is immediate if bit 31 is set in OPCODE and ARG2 is immediate if bit 30 is set
//
// Bits 8-11 decide what type of operation it is and bits 0-7 describe exact operation
//
// None of the bits 8-11 set means ALU operation with following exact operations:
// 0 -> Add
// 1 -> Sub
// 2 -> And
// 3 -> Or
// 4 -> Not
// 5 -> Xor
// 6 -> Shift bits left
// 7 -> Shift bits right
// 8 -> Mul
//
// Bit 8 set means Condition operation with the following exact operations:
// 0 -> Equal
// 1 -> Not Equal
// 2 -> Less than
// 3 -> Less than or equal
// 4 -> Greater than
// 5 -> Greater than or equal
//
// Bit 9 set means RAM operation with the following exact operations (reg0 holds RAM address):
// 0 -> Save to RAM
// 1 -> Load from RAM
//
// Bit 10 set means stack operation with the following exact operations:
// 0 -> Push
// 1 -> Pop
//
// Bit 11 is set when functions are called or returned:
// 0 -> Call
// 1 -> Return

pub struct Emulator {
    reg: [u32; 6],
    ram: [u32; 65536],
    stack: Vec<u32>
}

impl Emulator {
    pub fn new() -> Emulator {
        Emulator {
            reg: [0; 6],
            ram: [0; 65536],
            stack: vec![]
        }
    }

    pub fn run_script(&mut self, script: Vec<u32>) {
        
        let mut i = 0;
        while i < script.len() {
            // DEBUG: println!("{}, {:?}, {}", i, self.stack, self.ram[0]);
            let instruction = script[i];
            if instruction & (1 << 8) != 0 {

                let arg1: u32;
                if instruction & (1 << 31) != 0 {
                    arg1 = script[i + 1];
                }
                else {
                    arg1 = self.reg[script[i + 1] as usize];
                }

                let arg2: u32;
                if instruction & (1 << 30) != 0 {
                    arg2 = script[i + 2];
                }
                else {
                    arg2 = self.reg[script[i + 2] as usize];
                }

                let cond = match instruction % 32 {
                    0 => arg1 == arg2,
                    1 => arg1 != arg2,
                    2 => arg1 <  arg2,
                    3 => arg1 <= arg2,
                    4 => arg1 >  arg2,
                    5 => arg1 >= arg2,
                    _ => panic!("Unreachable")
                };

                // Check for halt
                if cond && script[i + 3] as usize == i { break; }

                if cond { i = script[i + 3] as usize; continue; }
            }
            else if instruction & (1 << 9) != 0 {

                if instruction & 1 != 0 {
                    self.reg[script[i + 3] as usize] = self.ram[self.reg[0] as usize];
                }
                else {
                    let arg1: u32;
                    if instruction & (1 << 31) != 0 {
                        arg1 = script[i + 1];
                    }
                    else {
                        arg1 = self.reg[script[i + 1] as usize];
                    }
                    self.ram[self.reg[0] as usize] = arg1;
                }
            }
            else if instruction & (1 << 10) != 0 {
                
                if instruction & 1 != 0 {
                    self.reg[script[i + 3] as usize] = self.stack.pop().expect("Empty Stack");
                }
                else {
                    let arg1: u32;
                    if instruction & (1 << 31) != 0 {
                        arg1 = script[i + 1];
                    }
                    else {
                        arg1 = self.reg[script[i + 1] as usize];
                    }
                    self.stack.push(arg1);
                }
            }
            else if instruction & (1 << 11) != 0 {  

                if instruction & 1 != 0 {
                    i = self.stack.pop().expect("Empty Stack") as usize;
                    continue;
                }
                else {
                    self.stack.push(i as u32 + 4);
                    i = script[i + 1] as usize;
                    continue;
                }
            }
            else {

                let arg1: i32;
                if instruction & (1 << 31) != 0 {
                    arg1 = script[i + 1] as i32;
                }
                else {
                    arg1 = self.reg[script[i + 1] as usize] as i32;
                }

                let arg2: i32;
                if instruction & (1 << 30) != 0 {
                    arg2 = script[i + 2] as i32;
                }
                else {
                    arg2 = self.reg[script[i + 2] as usize] as i32;
                }

                let result = match instruction % 32 {
                    0 => arg1.wrapping_add(arg2),
                    1 => arg1.wrapping_sub(arg2),
                    2 => arg1 & arg2,
                    3 => arg1 | arg2,
                    4 => !arg1,
                    5 => arg1 ^ arg2,
                    6 => arg1 << arg2,
                    7 => arg1 >> arg2,
                    8 => arg1.wrapping_mul(arg2),
                    9 => arg1 / arg2,
                    _ => panic!("Unreachable")
                };

                if script[i + 3] == 7 {
                    println!("{}", result);
                }
                else {
                    self.reg[script[i + 3] as usize] = result as u32;
                }
            }
            i += 4;
        }
    }

    pub fn print_info(&self, ram_addresses_cnt: usize) {
        println!("Registers:\nr0: {}\nr1: {}\nr2: {}\nr3: {}\nr4: {}\nr5: {}", 
            self.reg[0] as i32, 
            self.reg[1] as i32, 
            self.reg[2] as i32, 
            self.reg[3] as i32, 
            self.reg[4] as i32, 
            self.reg[5] as i32
        );
        println!("First {} ram addresses:", ram_addresses_cnt);
        for i in 0..ram_addresses_cnt { println!("nmr {}: {}", i, self.ram[i] as i32); }
        println!("Stack:");
        for item in self.stack.clone() { print!("{}, ", item as i32); }
        println!("");
    }
}