pub struct Emulator {
    reg: [u32; 6],
    ram: [u32; 4096],
    stack: Vec<u32>
}

impl Emulator {
    pub fn new() -> Emulator {
        Emulator {
            reg: [0; 6],
            ram: [0; 4096],
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

                let result = match instruction % 32 {
                    0 => arg1.wrapping_add(arg2),
                    1 => arg1.wrapping_sub(arg2),
                    2 => arg1 & arg2,
                    3 => arg1 | arg2,
                    4 => !arg1,
                    5 => arg1 ^ arg2,
                    6 => arg1 << arg2,
                    7 => arg1 >> arg2,
                    _ => panic!("Unreachable")
                };

                if script[i + 3] == 7 {
                    println!("{}", result);
                }
                else {
                    self.reg[script[i + 3] as usize] = result;
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