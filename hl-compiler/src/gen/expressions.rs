use std::collections::HashMap;

use crate::{ast::expressions::{Expression, ExpressionType}, utils::get_ram_identifier};

use super::ram::Ram;

pub struct ExpressionGen {}

impl ExpressionGen {
    pub fn new() -> ExpressionGen {
        ExpressionGen {}
    }

    fn get_instruction(operator: String) -> String {
        match operator.as_str() {
            "+" => "add",
            "-" => "sub",
            "*" => "mul",
            "&" => "and",
            "|" => "or",
            "^" => "xor",
            "<<" => "shl",
            ">>" => "shr",
            "==" => "eq",
            "!=" => "neq",
            "<" => "lt",
            "<=" => "lte",
            ">" => "gt",
            ">=" => "gte",
            _ => panic!("Unreachable")
        }.to_string()
    }

    fn get_expression_leaf_cnt(&mut self, expression: &Expression) -> usize {
        match &expression.ty {
            ExpressionType::Value(_) => 1,
            ExpressionType::Function(_, _) => 6,
            ExpressionType::Operator(_) => 0,
            ExpressionType::Block(block) => {
                let mut cnt = 0;
                for expression in block {
                    cnt += self.get_expression_leaf_cnt(expression);
                }
                cnt
            },
            ExpressionType::ParenthesisClose(_) | ExpressionType::ParenthesisOpen(_) => panic!("Unreachable")
        }
    }

    fn parse_function_call(&mut self, identifier: &String, arguments: &Vec<Expression>, ram: &mut Ram, current_function: &String, functions: &HashMap<String, Vec<String>>) -> String {

        let mut out = String::new();

        let prefix = if *current_function == identifier.clone() { identifier.clone() } else { "-".to_string() };
        let local_variables = ram.get_local_variables(&prefix);
        for variable in &local_variables {
            let arg_ram_location = ram.get(variable).expect("Unreachable");
            out.push_str(format!("mov {} r0\nlram r1\npush r1\n", arg_ram_location).as_str());
        }

        let function_arguments = functions.get(identifier).expect("Unreachable").clone();
        for (i, argument) in arguments.iter().enumerate() {

            let expression_asm: String;
            if self.get_expression_leaf_cnt(argument) >= 5 {
                expression_asm = self.parse_large_expression(argument, ram, ram.get_local_variables(&String::from("fn")).len(), current_function, functions);
            }
            else {
                expression_asm = self.parse_expression(argument, ram, None, current_function, functions);
            }
            let arg_ram_location = ram.allocate_next(&format!("{}:{}", identifier, function_arguments[i]));
            out.push_str(format!("{}mov {} r0\nsram r3\n",expression_asm, arg_ram_location).as_str());
        }

        out.push_str(format!("call f{}\n", identifier).as_str());

        for variable in local_variables.iter().rev() {
            let arg_ram_location = ram.get(&variable).unwrap();
            out.push_str(format!("pop r1\nmov {} r0\nsram r1\n", arg_ram_location).as_str());
        }
        out
    } 

    fn parse_large_expression(&mut self, expression: &Expression, ram: &mut Ram, ram_identifier: usize, current_function: &String, functions: &HashMap<String, Vec<String>>) -> String {
        let ram_location = ram.allocate_next(&ram_identifier.to_string());

        match &expression.ty {
            ExpressionType::Value(value) => {
                if value.chars().nth(0).expect("Unreachable").is_numeric() {
                    return format!("mov {} r0\nsram {}\n", ram_location, value.parse::<isize>().expect("Unreachable") as usize);
                }
                if let Some(variable_ram_location) = ram.get(&get_ram_identifier(current_function, &value)) {
                    return format!("mov {} r0\nlram r3\nmov {} r0\nsram r3\n", variable_ram_location, ram_location);
                }
                panic!("Non initialized variable: {}", value);
            },
            ExpressionType::Function(identifier, arguments) => {
                let function_call_asm = self.parse_function_call(&identifier, &arguments, ram, current_function, functions);
                return format!("{}mov 0 r0\nlram r3\nmov {} r0\nsram r3\n", function_call_asm, ram_location);
            },
            ExpressionType::Block(block) => {   

                let mut out = format!("mov {} r0\nsram 0\n", ram_location);
                let mut i = 0;
                while i < block.len() {

                    let operator: String;
                    if i == 0 { operator = "add".to_string(); }
                    else {
                        let operator_raw: String;
                        if let ExpressionType::Operator(operator) = &block[i].ty { operator_raw = operator.clone(); }
                        else { panic!("Unreachable"); }
                        operator = ExpressionGen::get_instruction(operator_raw);
                        i += 1;
                    }
                    
                    let asm_calc_value = self.parse_large_expression(&block[i], ram, ram_identifier + 1, current_function, functions);
                    out.push_str(format!("{}mov {} r0\nlram r3\nmov {} r0\nlram r4\n{} r4 r3 r3\nsram r3\n", 
                        asm_calc_value,
                        ram.get(&(ram_identifier + 1).to_string()).expect("Unreachable"),
                        ram_location,
                        operator
                    ).as_str());
                    ram.free(&(ram_identifier + 1).to_string());
                    i += 1;
                }
                return out;
            },      
            ExpressionType::ParenthesisClose(_) | ExpressionType::ParenthesisOpen(_) | ExpressionType::Operator(_) => panic!("Unreachable")
        };
    }

    fn parse_small_expression(&self, expression: &Expression, ram: &mut Ram, regs_used: usize, current_function: &String, functions: &HashMap<String, Vec<String>>) -> String {
        
        match &expression.ty {
            ExpressionType::Value(value) => {
                if value.chars().nth(0).unwrap().is_numeric() {
                    format!("mov {} r{}\n", value, regs_used)
                }
                else {
                    let ram_location = ram.get(&get_ram_identifier(current_function, value)).unwrap();
                    format!("mov {} r0\nlram r{}\n", ram_location, regs_used)
                }
            },
            ExpressionType::Block(block) => {
                let mut out = self.parse_small_expression(&block[0], ram, regs_used, current_function, functions);
                let mut i = 1;
                while i < block.len() {
                    if let ExpressionType::Operator(operator) = &block[i].ty {
                        let instruction = ExpressionGen::get_instruction(operator.to_string());
                        out.push_str(format!("{}{} r{} r{} r{}\n", 
                            self.parse_small_expression(&block[i + 1], ram, regs_used + 1, current_function, functions),
                            instruction,
                            regs_used,
                            regs_used + 1,
                            regs_used
                        ).as_str());
                    }
                    i += 2;
                }

                if regs_used == 1 {
                    out.push_str(format!("mov r1 r3\n").as_str());
                }

                out
            }
            ExpressionType::Operator(_) | ExpressionType::ParenthesisClose(_) | ExpressionType::ParenthesisOpen(_) | ExpressionType::Function(_, _) => panic!("Unreachable")
        }
    }

    fn parse_simple_assignment(&self, expression: &Expression, ram: &mut Ram, current_function: &String, functions: &HashMap<String, Vec<String>>) -> String {
        
        match &expression.ty {
            ExpressionType::Value(value) => {
                if value.chars().nth(0).unwrap().is_numeric() {
                    format!("mov {} r3\n", value)
                }
                else {
                    let ram_location = ram.get(&get_ram_identifier(current_function, value)).unwrap();
                    format!("mov {} r0\nlram r3\n", ram_location)
                }
            }
            ExpressionType::Block(block) => self.parse_simple_assignment(&block[0], ram, current_function, functions),
            ExpressionType::Operator(_) | ExpressionType::ParenthesisClose(_) | ExpressionType::ParenthesisOpen(_) | ExpressionType::Function(_, _) => panic!("Unreachable")
        }
    }

    pub fn parse_expression(&mut self, expression: &Expression, ram: &mut Ram, ram_identifier: Option<String>, current_function: &String, functions: &HashMap<String, Vec<String>>) -> String {
       
        let mut out = match self.get_expression_leaf_cnt(expression) {
            0 => panic!("Unreachable"),
            1 => self.parse_simple_assignment(expression, ram, current_function, functions),
            2..=5 => self.parse_small_expression(expression, ram, 1, current_function, functions),
            _ => {
                let mut out = self.parse_large_expression(expression, ram, 0, current_function, functions);
                let value_ram_location = ram.get(&String::from("0")).expect("Unreachable").clone();
                ram.free(&String::from("0"));
                out.push_str(format!("mov {} r0\nlram r3\n", value_ram_location).as_str());
                out
            }
        };

        if let Some(ram_identifier) = ram_identifier {
            let ram_location = ram.allocate_next(&get_ram_identifier(current_function, &ram_identifier));
            out.push_str(format!("mov {} r0\nsram r3\n", ram_location).as_str());
        }

        out
    }
}