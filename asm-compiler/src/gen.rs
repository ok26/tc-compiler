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

use crate::lexer::{Token, TokenType};

fn parse_argument(token: &Token, current_inst: &mut u32, shift: u8) -> u32 {
    match token.ty { 
        TokenType::Reg(nmr) => nmr,
        TokenType::Value(value) => { *current_inst |= 1 << shift; return value as u32 }
        _ => panic!("Incorrect arguments on line: {}, column: {}", token.row, token.column)
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

        match &token.ty {
            TokenType::Label(label) => { label_locations.insert(label.clone(), current_op); i += 1; },
            TokenType::Reg(_) | TokenType::Value(_) => panic!("Incorrect arguments on line: {}, column: {}", token.row, token.column),

            TokenType::Add | TokenType::Sub | TokenType::And | TokenType::Or | TokenType::Xor | TokenType::Shl | TokenType::Shr |
            TokenType::Eq | TokenType::NotEq | TokenType::Less | TokenType::LessEq | TokenType::Greater | TokenType::GreaterEq |
            TokenType::Jeq | TokenType::Jneq | TokenType::Jl | TokenType::Jg | TokenType::Jge | TokenType::Jle => {

                if i + 3 >= tokens.len() { panic!("Incorrect arguments on line: {}, column: {}", token.row, token.column) }
                
                let mut current_inst: u32 = match token.ty {
                    TokenType::Add => 0,
                    TokenType::Sub => 1,
                    TokenType::And => 2,
                    TokenType::Or => 3,
                    TokenType::Xor => 5,
                    TokenType::Shl => 6,
                    TokenType::Shr => 7,
                    TokenType::Eq | TokenType::Jeq => 1 << 8,
                    TokenType::NotEq | TokenType::Jneq => (1 << 8) + 1,
                    TokenType::Less | TokenType::Jl => (1 << 8) + 2,
                    TokenType::LessEq | TokenType::Jle => (1 << 8) + 3,
                    TokenType::Greater | TokenType::Jg => (1 << 8) + 4,
                    TokenType::GreaterEq | TokenType::Jge => (1 << 8) + 5,
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
                
                match token.ty {
                    TokenType::Jeq | TokenType::Jneq | TokenType::Jl | TokenType::Jg | TokenType::Jge | TokenType::Jle => {
                        match tokens[i + 3].clone().ty {
                            TokenType::Label(label) => { pending_jumps.push((label, current_op + 3)); instructions.push(0); }
                            _ => panic!("Incorrect arguments on line: {}, column: {}", token.row, token.column)
                        }
                    },
                    TokenType::Eq | TokenType::NotEq | TokenType::Less | TokenType::LessEq | TokenType::Greater | TokenType::GreaterEq => {
                        instructions.push(current_op + 12);
                        match tokens[i + 3].ty {
                            TokenType::Reg(nmr) => {
                                instructions.append(&mut vec![0xC0000000, 0, 0, nmr, 0xC0000100, 0, 0, current_op + 16, 0xC0000000, 1, 0, nmr]);
                                current_op += 12;
                            }
                            _ => panic!("Incorrect arguments on line: {}, column: {}", token.row, token.column)
                        }
                    },
                    _ => {
                        match tokens[i + 3].ty {
                            TokenType::Reg(nmr) => { instructions.push(nmr); }
                            _ => panic!("Incorrect arguments on line: {}, column: {}", token.row, token.column)
                        }
                    }
                }

                current_op += 4;
                i += 4;
            },
            TokenType::Sram | TokenType::Lram | TokenType::Push | TokenType::Pop => {

                if i + 1 >= tokens.len() { panic!("Incorrect arguments on line: {}, column: {}", token.row, token.column) }

                let mut current_inst = match token.ty {
                    TokenType::Sram => 1 << 9,
                    TokenType::Lram => (1 << 9) + 1,
                    TokenType::Push => 1 << 10,
                    TokenType::Pop => (1 << 10) + 1,
                    _ => panic!("Unreachable")
                };

                match token.ty {
                    TokenType::Sram | TokenType::Push => {
                        let arg1 = parse_argument(&tokens[i + 1], &mut current_inst, 31);
                        instructions.push(current_inst);
                        instructions.push(arg1);
                        add_dummy_instruction(&mut instructions, 2);
                    },
                    TokenType::Lram | TokenType::Pop => {
                        instructions.push(current_inst);
                        add_dummy_instruction(&mut instructions, 2);
                        match tokens[i + 1].ty {
                            TokenType::Reg(nmr) => { instructions.push(nmr); }
                            _ => panic!("Incorrect arguments on line: {}, column: {}", token.row, token.column)
                        }
                    }
                    _ => panic!("Unreachable")
                }

                current_op += 4;
                i += 2;
            },
            TokenType::Jt | TokenType::Jf | TokenType::Not | TokenType::Mov => {

                if i + 2 >= tokens.len() { panic!("Incorrect arguments on line: {}, column: {}", token.row, token.column) }

                let mut current_inst = match token.ty {
                    TokenType::Jf => 1 << 8,
                    TokenType::Jt => (1 << 8) + 1,
                    TokenType::Not => 4,
                    TokenType::Mov => 1 << 30,
                    _ => panic!("Unreachable")
                };

                let arg = parse_argument(&tokens[i + 1], &mut current_inst, 31);

                instructions.push(current_inst);
                instructions.push(arg);
                instructions.push(0);

                match token.ty {
                    TokenType::Jf | TokenType::Jt => match tokens[i + 2].clone().ty {
                        TokenType::Label(label) => { pending_jumps.push((label, current_op + 3)); instructions.push(0); }
                        _ => panic!("Incorrect arguments on line: {}, column: {}", token.row, token.column)
                    },
                    TokenType::Not | TokenType::Mov => match tokens[i + 2].ty {
                        TokenType::Reg(nmr) => { instructions.push(nmr); }
                        _ => panic!("Incorrect arguments on line: {}, column: {}", token.row, token.column)
                    }
                    _ => panic!("Unreachable")
                }

                current_op += 4;
                i += 3;
            },
            TokenType::Call => {
                if i + 1 >= tokens.len() { panic!("Incorrect arguments on line: {}, column: {}", token.row, token.column) }

                instructions.push((1 << 11) | (1 << 31));
                match tokens[i + 1].clone().ty {
                    TokenType::Label(label) => { pending_jumps.push((label, current_op + 1)); instructions.push(0); }
                    _ => panic!("Incorrect arguments on line: {}, column: {}", token.row, token.column)
                }
                add_dummy_instruction(&mut instructions, 2);

                current_op += 4;
                i += 2;
            },
            TokenType::Return => {
                instructions.push((1 << 11) + 1);
                add_dummy_instruction(&mut instructions, 3);

                current_op += 4;
                i += 1;
            },
            TokenType::Halt => {
                instructions.append(&mut vec![1 << 8, 0, 0, current_op]);

                current_op += 4;
                i += 1;
            },
            TokenType::Jmp => {
                if i + 1 >= tokens.len() { panic!("Incorrect arguments on line: {}, column: {}", token.row, token.column) }

                instructions.append(&mut vec![0xC0000100, 0, 0]);

                match tokens[i + 1].clone().ty {
                    TokenType::Label(label) => { pending_jumps.push((label, current_op + 3)); instructions.push(0); }
                    _ => panic!("Incorrect arguments on line: {}, column: {}", token.row, token.column)
                }

                current_op += 4;
                i += 2;
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