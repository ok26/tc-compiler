use crate::ast::{Expression, Node};
use crate::gen::ram::Ram;

pub struct Gen {
    ast: Vec<Node>,
    ram: Ram,
}

impl Gen {
    pub fn new(ast: Vec<Node>) -> Gen {
        Gen {
            ast,
            ram: Ram::new(4096),
        }
    }

    fn parse_function_call(&self, identifier: &String, arguments: &Vec<String>) -> String {
        String::new()
    }

    // Parses expression and puts final value in r5
    fn parse_expression(&mut self, expression: Expression, ram_identifier: usize) -> String {

        let ram_location = self.ram.allocate_next(&ram_identifier.to_string());

        match expression {
            Expression::Value(value) => {
                if value.chars().nth(0).expect("Unreachable").is_numeric() {
                    return format!("mov {} r0\nsram {}\n", ram_location, value.parse::<isize>().expect("Unreachable") as usize);
                }
                if let Some(variable_ram_location) = self.ram.get(&value) {
                    return format!("mov {} r0\nlram r3\nmov {} r0\nsram r3\n", variable_ram_location, ram_location);
                }
                panic!("Non initialized variable: {}", value);
            },
            Expression::Function(identifier, arguments) => {
                let function_call_asm = self.parse_function_call(&identifier, &arguments);
                return format!("{}mov 0 r0\nlram r3\nmov {} r0\nsram r3\n", function_call_asm, ram_location);
            },
            Expression::Block(block) => {   

                let mut out = format!("mov {} r0\nsram 0\n", ram_location);
                let mut i = 0;
                while i < block.len() {

                    let operator: &str;
                    if i == 0 { operator = "add"; }
                    else {
                        let operator_raw: String;
                        if let Expression::Operator(operator) = &block[i] { operator_raw = operator.clone(); }
                        else { panic!("Unreachable"); }
                        operator = match operator_raw.as_str() {
                            "+" => "add",
                            "-" => "sub",
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
                        };
                        i += 1;
                    }
                    
                    let asm_calc_value = self.parse_expression(block[i].clone(), ram_identifier + 1);
                    out.push_str(format!("{}mov {} r0\nlram r3\nmov {} r0\nlram r4\n{} r4 r3 r3\nsram r3\n", 
                        asm_calc_value,
                        self.ram.get(&(ram_identifier + 1).to_string()).expect("Unreachable"),
                        ram_location,
                        operator
                    ).as_str());
                    self.ram.free(&(ram_identifier + 1).to_string());
                    i += 1;
                }
                return out;
            },      
            Expression::ParenthesisClose(_) | Expression::ParenthesisOpen(_) | Expression::Operator(_) => panic!("Unreachable")
        };
    }

    fn parse_variable_assignment(&mut self, identifier: &String, value: &Expression) -> String {

        let ram_location: usize;
        if let Some(location) = self.ram.get(identifier) {
            ram_location = *location;
        }
        else {
            ram_location = self.ram.allocate_next(identifier);
        }

        let asm_instruction = self.parse_expression(value.clone(), 0);
        let value_ram_location = self.ram.get(&String::from("0")).expect("Unreachable").clone();
        self.ram.free(&String::from("0"));

        format!("{}mov {} r0\nlram r3\nmov {} r0\nsram r3\n", asm_instruction, value_ram_location, ram_location)
    }

    fn parse_node(&mut self, node: &Node) -> String {
        
        return match node {
            Node::VariableAssignment { identifier, value } => self.parse_variable_assignment(identifier, value),
            Node::If { condition, body } => String::new(),
            Node::IfElse { condition, body, else_body } => String::new(),
            Node::While { condition, body } => String::new(),
            Node::For { variable, condition, loop_increment, body } => String::new(),
            Node::Function { identifier, arguments, body } => String::new(),
            Node::Return { value } => String::new(),
            Node::Error { ty, row, column } => panic!("Unreachable")
        }
    }

    pub fn generate_asm(&mut self) -> String {
        let mut asm = String::new();
        for node in self.ast.clone() {
            asm.push_str(self.parse_node(&node).as_str()) 
        }

        asm
    }
}