use std::collections::HashMap;

use crate::ast::{Expression, Node};
use crate::gen::ram::Ram;

pub struct Gen {
    ast: Vec<Node>,
    ram: Ram,
    current_label: usize,
    current_function: String,
    function_arguments: HashMap<String, Vec<String>>,
    asm: String
}

impl Gen {
    pub fn new(ast: Vec<Node>) -> Gen {

        let mut function_arguments = HashMap::new();
        for node in ast.clone() { match node {
            Node::Function { identifier, arguments, body: _ } => { function_arguments.insert(identifier, arguments); },
            _ => {}
        }}
        Gen {
            ast,
            ram: Ram::new(4096),
            current_label: 0,
            current_function: String::new(),
            function_arguments,
            asm: String::new()
        }
    }

    fn get_ram_identifier(&self, identifier: &String) -> String {
        format!("{}:{}", self.current_function, identifier)
    }

    // Parses expression and puts final value in r5
    fn parse_expression(&mut self, expression: Expression, ram_identifier: usize) -> String {

        let ram_location = self.ram.allocate_next(&ram_identifier.to_string());

        match expression {
            Expression::Value(value) => {
                if value.chars().nth(0).expect("Unreachable").is_numeric() {
                    return format!("mov {} r0\nsram {}\n", ram_location, value.parse::<isize>().expect("Unreachable") as usize);
                }
                if let Some(variable_ram_location) = self.ram.get(&self.get_ram_identifier(&value)) {
                    return format!("mov {} r0\nlram r3\nmov {} r0\nsram r3\n", variable_ram_location, ram_location);
                }
                panic!("Non initialized variable: {}", value);
            },
            Expression::Function(identifier, arguments) => {
                let function_call_asm = self.parse_function_call(&identifier, &arguments, ram_identifier + 1);
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

        let ram_location = self.ram.allocate_next(&self.get_ram_identifier(identifier));
        let asm_instruction = self.parse_expression(value.clone(), 0);
        let value_ram_location = self.ram.get(&String::from("0")).expect("Unreachable").clone();
        self.ram.free(&String::from("0"));

        format!("{}mov {} r0\nlram r3\nmov {} r0\nsram r3\n", asm_instruction, value_ram_location, ram_location)
    }

    fn parse_body(&mut self, body: &Vec<Node>, jump_to: String, jump_back: Option<String>) -> String {
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
        out
    }

    fn parse_if_statement(&mut self, condition: &Expression, body: &Vec<Node>, else_body: Option<&Vec<Node>>) -> String {
        let expression_asm = self.parse_expression(condition.clone(), 0);
        let cond_ram_location = self.ram.get(&String::from("0")).expect("Unreachable").clone();
        self.ram.free(&String::from("0"));

        let out: String;
        if let Some(else_body) = else_body {
            out = format!("{}mov {} r0\nlram r3\njt r3 l{}\njf r3 l{}\nl{}:\n",
                expression_asm,
                cond_ram_location,
                self.current_label,
                self.current_label + 1,
                self.current_label + 2
            );
            let body_asm = self.parse_body(body, self.current_label.to_string(), Some((self.current_label + 2).to_string()));
            self.asm.insert_str(0, body_asm.as_str());
            let else_body_asm = self.parse_body(else_body, (self.current_label + 1).to_string(), Some((self.current_label + 2).to_string()));
            self.asm.insert_str(0, else_body_asm.as_str());
            self.current_label += 3;
        }
        else {
            out = format!("{}mov {} r0\nlram r3\njt r3 l{}\nl{}:\n", 
                expression_asm,
                cond_ram_location, 
                self.current_label, 
                self.current_label + 1
            );
            let body_asm = self.parse_body(body, self.current_label.to_string(), Some((self.current_label + 1).to_string()));
            self.asm.insert_str(0, body_asm.as_str());
            self.current_label += 2;
        }
        out   
    }

    fn parse_while_loop(&mut self, condition: &Expression, body: &Vec<Node>) -> String {
        let jump_to = self.current_label;
        let jump_back = self.current_label + 1;
        self.current_label += 2;
        let body_asm = self.parse_body(body, jump_to.to_string(), Some(jump_back.to_string()));
        self.asm.insert_str(0, body_asm.as_str());
        
        let expression_asm = self.parse_expression(condition.clone(), 0);
        let cond_ram_location = self.ram.get(&String::from("0")).expect("Unreachable").clone();
        self.ram.free(&String::from("0"));

        let out = format!("l{}:\n{}mov {} r0\nlram r3\njt r3 l{}\n", 
            jump_back, 
            expression_asm, 
            cond_ram_location, 
            jump_to
        );
        out
    }

    fn parse_for_loop(&mut self, variable: &Box<Option<Node>>, condition: &Expression, loop_increment: &Box<Option<Node>>, body: &Vec<Node>) -> String {
        let jump_to = self.current_label;
        let jump_back = self.current_label + 1;
        self.current_label += 2;
        let variable_asm: String;

        if let Some(variable_assinment) = &**variable { match variable_assinment {
            Node::VariableAssignment { identifier, value } => variable_asm = self.parse_variable_assignment(identifier, value),
            _ => panic!("Unreachable")
        }}
        else { variable_asm = String::new(); }

        let mut body_asm = self.parse_body(body, jump_to.to_string(), Some(jump_back.to_string()));
        if let Some(loop_increment) = &**loop_increment { match loop_increment {
            Node::VariableAssignment { identifier, value } => {

                let mut i = body_asm.len();
                let mut chars = body_asm.chars().rev();
                chars.next(); // Skip first new line
                while let Some(c) = chars.next() {
                    i -= 1;
                    if c == '\n' { break; }
                }
                body_asm.insert_str(i, self.parse_variable_assignment(identifier, value).as_str());
            },
            _ => panic!("Unreachable")
        }}
        self.asm.insert_str(0, body_asm.as_str());

        let expression_asm = self.parse_expression(condition.clone(), 0);
        let cond_ram_location = self.ram.get(&String::from("0")).expect("Unreachable").clone();
        self.ram.free(&String::from("0"));

        let out = format!("{}l{}:\n{}mov {} r0\n lram r3\n jt r3 l{}\n",
            variable_asm,
            jump_back,
            expression_asm,
            cond_ram_location,
            jump_to            
        );
        out
    }

    fn parse_function_definition(&mut self, identifier: &String, body: &Vec<Node>) -> String {
        let mut out = format!("f{}:\n", identifier);

        self.current_function = identifier.clone();
        for argument in self.function_arguments.get(&self.current_function).expect("Unreachable") {
            self.ram.allocate_next(&self.get_ram_identifier(argument));
        }

        for node in body {
            out.push_str(self.parse_node(node).as_str());
        }

        // Some functions do not return values
        out.push_str("ret\n");
        self.asm.insert_str(0, out.as_str());
        String::new()
    }

    fn parse_function_call(&mut self, identifier: &String, arguments: &Vec<Expression>, ram_identifier: usize) -> String {

        let mut out = String::new();

        let prefix = if self.current_function == identifier.clone() { identifier.clone() } else { "-".to_string() };
        let local_variables = self.ram.get_local_variables(&prefix);
        for variable in &local_variables {
            let arg_ram_location = self.ram.get(variable).expect("Unreachable");
            out.push_str(format!("mov {} r0\nlram r1\npush r1\n", arg_ram_location).as_str());
        }

        let function_arguments = self.function_arguments.get(identifier).expect("Unreachable").clone();
        for (i, argument) in arguments.iter().enumerate() {

            let expression_asm = self.parse_expression(argument.clone(), ram_identifier);
            let passed_arg_ram_location = self.ram.get(&ram_identifier.to_string()).expect("Unreachable").clone();
            self.ram.free(&ram_identifier.to_string());

            let arg_ram_location = self.ram.allocate_next(&format!("{}:{}", identifier, function_arguments[i]));            
            out.push_str(format!("{}mov {} r0\nlram r1\nmov {} r0\nsram r1\n",expression_asm, passed_arg_ram_location, arg_ram_location).as_str());
        }

        out.push_str(format!("call f{}\n", identifier).as_str());

        for variable in local_variables.iter().rev() {
            let arg_ram_location = self.ram.get(&variable).expect("Unreachable");
            out.push_str(format!("pop r1\nmov {} r0\nsram r1\n", arg_ram_location).as_str());
        }
        out
    }

    fn parse_return(&mut self, value: &Option<Expression>) -> String {

        let mut out = String::new();
        if let Some(value) = value {
            let expression_asm = self.parse_expression(value.clone(), 0);
            let value_ram_location = self.ram.get(&String::from("0")).expect("Unreachable").clone();
            self.ram.free(&String::from("0"));
            out.push_str(format!("{}mov {} r0\nlram r1\nmov 0 r0\nsram r1\n", expression_asm, value_ram_location).as_str());
        }
        out.push_str("ret\n");
        out
    }

    fn parse_node(&mut self, node: &Node) -> String {
        
        return match node {
            Node::VariableAssignment { identifier, value } => self.parse_variable_assignment(identifier, value),
            Node::If { condition, body } => self.parse_if_statement(condition, body, None),
            Node::IfElse { condition, body, else_body } => self.parse_if_statement(condition, body, Some(else_body)),
            Node::While { condition, body } => self.parse_while_loop(condition, body),
            Node::For { variable, condition, loop_increment, body } => self.parse_for_loop(variable, condition, loop_increment, body),
            Node::Function { identifier, arguments: _, body } => self.parse_function_definition(identifier, body),
            Node::Return { value } => self.parse_return(value),
            Node::FunctionCall { identifier, arguments } => self.parse_function_call(identifier, arguments, 0),
            Node::Error { .. } => panic!("Unreachable")
        }
    }

    pub fn generate_asm(&mut self) -> String {
        for node in self.ast.clone() {
            let asm = self.parse_node(&node);
            self.asm.push_str(asm.as_str());
        }  

        self.asm.insert_str(0, "call fmain\nhlt\n");
        self.asm.clone()
    }
}