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

use std::collections::HashMap;

use crate::lexer::Token;

fn parse_argument(token: &Token, current_inst: &mut u32, shift: u8) -> u32 {
    match token { 
        Token::Reg(nmr) => *nmr,
        Token::Value(value) => { *current_inst |= 1 << shift; return *value as u32 }
        _ => panic!("Incorrect arguments")
    }
}

fn add_dummy_instruction(instructions: &mut Vec<u32>, cnt: usize) {
    for _ in 0..cnt {
        instructions.push(0);
    }
}

pub fn gen_instructions(tokens: Vec<Token>) -> Vec<u32> {
    let mut instructions: Vec<u32> = vec![];
    let mut current_op: u32 = 0;
    let mut i = 0;
    let mut label_locations: HashMap<String, u32> = HashMap::new();
    let mut pending_jumps: Vec<(String, u32)> = vec![];

    while i < tokens.len() {
        let token = &tokens[i];

        match token {
            Token::Label(label) => { label_locations.insert(label.clone(), current_op); i += 1; },
            Token::Reg(_) | Token::Value(_) => panic!("Incorrect arguments"),

            Token::Add | Token::Sub | Token::And | Token::Or | Token::Xor | Token::Shl | Token::Shr |
            Token::Eq | Token::NotEq | Token::Less | Token::LessEq | Token::Greater | Token::GreaterEq |
            Token::Jeq | Token::Jneq | Token::Jl | Token::Jg | Token::Jge | Token::Jle => {

                if i + 3 >= tokens.len() { panic!("Incorrect arguments") }
                
                let mut current_inst: u32 = match token {
                    Token::Add => 0,
                    Token:: Sub => 1,
                    Token::And => 2,
                    Token::Or => 3,
                    Token::Xor => 5,
                    Token::Shl => 6,
                    Token::Shr => 7,
                    Token::Eq | Token::Jeq => 1 << 8,
                    Token::NotEq | Token::Jneq => (1 << 8) + 1,
                    Token::Less | Token::Jl => (1 << 8) + 2,
                    Token::LessEq | Token::Jle => (1 << 8) + 3,
                    Token::Greater | Token::Jg => (1 << 8) + 4,
                    Token::GreaterEq | Token::Jge => (1 << 8) + 5,
                    _ => panic!("Unreachable")
                };

                let arg1 = parse_argument(&tokens[i + 1], &mut current_inst, 31);
                let arg2 = parse_argument(&tokens[i + 2], &mut current_inst, 30);

                instructions.push(current_inst);         
                instructions.push(arg1);
                instructions.push(arg2);

                // Eq:
                // Translates from:
                // Eq arg1 arg2 rx
                // To:
                // Jeq arg1 arg2 current_op + 12
                // Add 0 0 rx
                // Jeq 0 0 current_op + 8
                // Add 1 0 rx
                
                match token {
                    Token::Jeq | Token::Jneq | Token::Jl | Token::Jg | Token::Jge | Token::Jle => {
                        match tokens[i + 3].clone() {
                            Token::Label(label) => { pending_jumps.push((label, current_op + 3)); instructions.push(0); }
                            _ => panic!("Incorrect arguments")
                        }
                    },
                    Token::Eq | Token::NotEq | Token::Less | Token::LessEq | Token::Greater | Token::GreaterEq => {
                        instructions.push(current_op + 12);
                        match tokens[i + 3] {
                            Token::Reg(nmr) => {
                                instructions.append(&mut vec![0xC0000000, 0, 0, nmr, 0xC0000100, 0, 0, current_op + 16, 0xC0000000, 1, 0, nmr]);
                                current_op += 12;
                            }
                            _ => panic!("Incorrect arguments")
                        }
                    },
                    _ => {
                        match tokens[i + 3] {
                            Token::Reg(nmr) => { instructions.push(nmr); }
                            _ => panic!("Incorrect arguments")
                        }
                    }
                }

                current_op += 4;
                i += 4;
            },
            Token::Sram | Token::Lram | Token::Push | Token::Pop => {

                if i + 1 >= tokens.len() { panic!("Incorrect arguments") }

                let mut current_inst = match token {
                    Token::Sram => 1 << 9,
                    Token::Lram => (1 << 9) + 1,
                    Token::Push => 1 << 10,
                    Token::Pop => (1 << 10) + 1,
                    _ => panic!("Unreachable")
                };

                match token {
                    Token::Sram | Token::Push => {
                        let arg1 = parse_argument(&tokens[i + 1], &mut current_inst, 31);
                        instructions.push(current_inst);
                        instructions.push(arg1);
                        add_dummy_instruction(&mut instructions, 2);
                    },
                    Token::Lram | Token::Pop => {
                        instructions.push(current_inst);
                        add_dummy_instruction(&mut instructions, 2);
                        match tokens[i + 1] {
                            Token::Reg(nmr) => { instructions.push(nmr); }
                            _ => panic!("Incorrect arguments")
                        }
                    }
                    _ => panic!("Unreachable")
                }

                current_op += 4;
                i += 2;
            },
            Token::Jt | Token::Jf | Token::Not | Token::Mov => {

                if i + 2 >= tokens.len() { panic!("Incorrect arguments") }

                let mut current_inst = match token {
                    Token::Jf => 1 << 8,
                    Token::Jt => (1 << 8) + 1,
                    Token::Not => 4,
                    Token::Mov => 0,
                    _ => panic!("Unreachable")
                };

                let arg = parse_argument(&tokens[i + 1], &mut current_inst, 31);

                instructions.push(current_inst);
                instructions.push(arg);
                instructions.push(0);

                match token {
                    Token::Jf | Token::Jt => match tokens[i + 2].clone() {
                        Token::Label(label) => { pending_jumps.push((label, current_op + 3)); instructions.push(0); }
                        _ => panic!("Incorrect arguments")
                    },
                    Token::Not | Token::Mov => match tokens[i + 2] {
                        Token::Reg(nmr) => { instructions.push(nmr); }
                        _ => panic!("Incorrect arguments")
                    }
                    _ => panic!("Unreachable")
                }

                current_op += 4;
                i += 3;
            },
            Token::Call => {

                if i + 1 >= tokens.len() { panic!("Incorrect arguments") }

                instructions.push((1 << 11) | (1 << 31));
                match tokens[i + 1].clone() {
                    Token::Label(label) => { pending_jumps.push((label, current_op + 1)); instructions.push(0); }
                    _ => panic!("Incorrect arguments")
                }
                add_dummy_instruction(&mut instructions, 2);

                current_op += 4;
                i += 2;
            },
            Token::Return => {
                instructions.push((1 << 11) + 1);
                add_dummy_instruction(&mut instructions, 3);

                current_op += 4;
                i += 1;
            },
            Token::Halt => {
                instructions.append(&mut vec![1 << 8, 0, 0, current_op]);

                current_op += 4;
                i += 1;
            }
        }   
    }

    for (label, location) in pending_jumps {
        if let Some(dest) = label_locations.get(&label) {
            instructions[location as usize] = *dest;
        }
        else {
            panic!("Label not declared: {}", label);
        }
    }
    instructions
}