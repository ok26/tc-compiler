use std::collections::HashMap;

use crate::{ast::{ast::{Node, NodeType}, expressions::Expression}, utils::get_ram_identifier};
use crate::gen::{ram::Ram, expressions::ExpressionGen};

pub struct Gen {
    ast: Vec<Node>,
    ram: Ram,
    expression_parser: ExpressionGen,
    current_label: usize,
    current_function: String,
    functions: HashMap<String, Vec<String>>,
    asm: String
}

impl Gen {
    pub fn new(ast: Vec<Node>, functions: HashMap<String, Vec<String>>) -> Gen {
        Gen {
            ast,
            ram: Ram::new(4096),
            expression_parser: ExpressionGen::new(),
            current_label: 0,
            current_function: String::new(),
            functions,
            asm: String::new()
        }
    }

    fn parse_variable_assignment(&mut self, identifier: &String, value: &Expression) -> String {
        self.expression_parser.parse_expression(value, &mut self.ram, Some(identifier.clone()), &self.current_function, &self.functions)
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
        let expression_asm = self.expression_parser.parse_expression(condition, &mut self.ram, None, &self.current_function, &self.functions);

        let out: String;
        if let Some(else_body) = else_body {
            out = format!("{}jt r3 l{}\njf r3 l{}\nl{}:\n",
                expression_asm,
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
            out = format!("{}jt r3 l{}\nl{}:\n", 
                expression_asm,
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
        
        let expression_asm = self.expression_parser.parse_expression(condition, &mut self.ram, None, &self.current_function, &self.functions);
        let out = format!("l{}:\n{}jt r3 l{}\n", 
            jump_back, 
            expression_asm,  
            jump_to
        );
        out
    }

    fn parse_for_loop(&mut self, variable: &Box<Option<Node>>, condition: &Expression, loop_increment: &Box<Option<Node>>, body: &Vec<Node>) -> String {
        let jump_to = self.current_label;
        let jump_back = self.current_label + 1;
        self.current_label += 2;
        let variable_asm: String;

        if let Some(variable_assinment) = &**variable { match &variable_assinment.ty {
            NodeType::VariableDeclaration { identifier, value } => variable_asm = self.parse_variable_assignment(identifier, value),
            _ => panic!("Unreachable")
        }}
        else { variable_asm = String::new(); }

        let mut body_asm = self.parse_body(body, jump_to.to_string(), Some(jump_back.to_string()));
        if let Some(loop_increment) = &**loop_increment { match &loop_increment.ty {
            NodeType::VariableAssignment { identifier, value } => {

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

        let expression_asm = self.expression_parser.parse_expression(condition, &mut self.ram, None, &self.current_function, &self.functions);
        let out = format!("{}l{}:\n{}jt r3 l{}\n",
            variable_asm,
            jump_back,
            expression_asm,
            jump_to            
        );
        out
    }

    fn parse_function_definition(&mut self, identifier: &String, body: &Vec<Node>) -> String {
        let mut out = format!("f{}:\n", identifier);

        self.current_function = identifier.clone();
        for argument in self.functions.get(&self.current_function).expect("Unreachable") {
            self.ram.allocate_next(&get_ram_identifier(&self.current_function, argument));
        }

        for node in body {
            out.push_str(self.parse_node(node).as_str());
        }

        // Some functions do not return values
        out.push_str("ret\n");
        self.asm.insert_str(0, out.as_str());
        String::new()
    }

    fn parse_return(&mut self, value: &Option<Expression>) -> String {

        let mut out = String::new();
        if let Some(value) = value {
            out = self.expression_parser.parse_expression(value, &mut self.ram, Some("return".to_string()), &self.current_function, &self.functions);
        }
        out.push_str("ret\n");
        out
    }

    fn parse_out_call(&mut self, value: &Expression) -> String {
        let expression_asm = self.expression_parser.parse_expression(value, &mut self.ram, None, &self.current_function, &self.functions);
        format!("{}\nout r3\n", expression_asm)
    }

    fn parse_node(&mut self, node: &Node) -> String {

        return match &node.ty {
            NodeType::VariableAssignment { identifier, value } => self.parse_variable_assignment(identifier, value),
            NodeType::VariableDeclaration { identifier, value } => self.parse_variable_assignment(identifier, value),
            NodeType::If { condition, body } => self.parse_if_statement(condition, body, None),
            NodeType::IfElse { condition, body, else_body } => self.parse_if_statement(condition, body, Some(else_body)),
            NodeType::While { condition, body } => self.parse_while_loop(condition, body),
            NodeType::For { variable, condition, loop_increment, body } => self.parse_for_loop(variable, condition, loop_increment, body),
            NodeType::Function { identifier, arguments: _, body } => self.parse_function_definition(identifier, body),
            NodeType::Return { value } => self.parse_return(value),
            NodeType::Out { value } => self.parse_out_call(value),
            NodeType::Error { .. } => panic!("Unreachable")
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