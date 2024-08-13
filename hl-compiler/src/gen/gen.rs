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
    fn parse_expression(&self, expression: Expression) -> String {

        

        /*  
        match expression {
            Expression::Value(value) => {
                if value.chars().nth(0).expect("Unreachable").is_numeric() {
                    return format!("mov {} r5\n", value.parse::<isize>().expect("Unreachable") as usize);
                }
                if let Some(ram_location) = self.ram.get(&value) {
                    return format!("mov {} r0\nlram r5\n", ram_location);
                }
                panic!("Non initialized variable: {}", value);
            },
            Expression::Function(identifier, arguments) => {
                let function_call_asm = self.parse_function_call(&identifier, &arguments);
                format!("{}mov 0 r0\nlram r5\n", function_call_asm)
            },
            Expression::Operation { left, right, raw } => {
                let operator = match raw.as_str() {
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
                return format!("{}mov r5 r2\n{}{} r2 r5 r5\n", self.parse_expression(*right), self.parse_expression(*left), operator);
            }
        }
        */
        String::new()
    }

    fn parse_variable_assignment(&mut self, identifier: &String, value: &Expression) -> String {

        let ram_location: usize;
        if let Some(location) = self.ram.get(identifier) {
            ram_location = *location;
        }
        else {
            ram_location = self.ram.allocate_next(identifier);
        }

        format!("{}mov {} r0\nsram r5\n", self.parse_expression(value.clone()), ram_location)
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
            Node::Error { ty, row, collumn } => panic!("Unreachable")
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