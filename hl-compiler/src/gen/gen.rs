use crate::ast::{Expression, Node};
use crate::gen::ram::Ram;

pub struct Gen {
    ast: Vec<Node>,
    ram: Ram,
    current_label: usize,
    asm: String
}

impl Gen {
    pub fn new(ast: Vec<Node>) -> Gen {
        Gen {
            ast,
            ram: Ram::new(4096),
            current_label: 0,
            asm: String::new()
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

    fn parse_body(&mut self, body: &Vec<Node>, jump_to: String, jump_back: Option<String>) {
        let mut out = format!("l{}:\n", jump_to);
        for node in body {
            out.push_str(&self.parse_node(node).as_str());
        }
        if let Some(jump_back) = jump_back {
            out.push_str(format!("jmp l{}\n", jump_back).as_str());
        }
        else {
            out.push_str("ret\n");
        }
        self.asm.insert_str(0, out.as_str());
    }

    fn parse_if_statement(&mut self, condition: &Expression, body: &Vec<Node>, else_body: Option<&Vec<Node>>) -> String {
        let asm_instructions = self.parse_expression(condition.clone(), 0);
        let cond_ram_location = self.ram.get(&String::from("0")).expect("Unreachable").clone();
        self.ram.free(&String::from("0"));

        let out: String;
        if let Some(else_body) = else_body {
            out = format!("{}mov {} r0\nlram r3\njt r3 l{}\njf r3 l{}\nl{}:\n",
                asm_instructions,
                cond_ram_location,
                self.current_label,
                self.current_label + 1,
                self.current_label + 2
            );
            self.parse_body(body, self.current_label.to_string(), Some((self.current_label + 2).to_string()));
            self.parse_body(else_body, (self.current_label + 1).to_string(), Some((self.current_label + 2).to_string()));
            self.current_label += 3;
        }
        else {
            out = format!("{}mov {} r0\nlram r3\njt r3 l{}\nl{}:\n", 
                asm_instructions,
                cond_ram_location, 
                self.current_label, 
                self.current_label + 1
            );
            self.parse_body(body, self.current_label.to_string(), Some((self.current_label + 1).to_string()));
            self.current_label += 2;
        }
        out   
    }

    fn parse_function(&mut self, identifier: &String, arguments: &Vec<String>, body: &Vec<Node>) -> String {
        self.parse_body(body, identifier.clone(), None);
        String::new()
    }

    fn parse_node(&mut self, node: &Node) -> String {
        
        return match node {
            Node::VariableAssignment { identifier, value } => self.parse_variable_assignment(identifier, value),
            Node::If { condition, body } => self.parse_if_statement(condition, body, None),
            Node::IfElse { condition, body, else_body } => self.parse_if_statement(condition, body, Some(else_body)),
            Node::While { condition, body } => String::new(),
            Node::For { variable, condition, loop_increment, body } => String::new(),
            Node::Function { identifier, arguments, body } => self.parse_function(identifier, arguments, body),
            Node::Return { value } => String::new(),
            Node::Error { ty, row, column } => panic!("Unreachable")
        }
    }

    pub fn generate_asm(&mut self) -> String {
        for node in self.ast.clone() {
            let asm = self.parse_node(&node);
            self.asm.push_str(asm.as_str());
        }  

        self.asm.insert_str(0, "call lmain\nhlt\n");
        self.asm.clone()
    }
}