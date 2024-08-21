use std::collections::{HashMap, HashSet};

use crate::ast::{Expression, Node};

#[derive(Clone)]
pub enum SemanticsError {
    UnwrappedCode,
    FunctionDefinitionInFunction,
    MissingMainFunction,
    FunctionDefinedTwice(String),
    UninitializedVariable(String),
    UninitializedFunction(String),
    IncorrectFunctionArguments(String, usize, usize)
}

pub struct SemanticAnalysis {
    vars_in_scope: HashSet<String>,
    functions: HashMap<String, Vec<String>>,
    errors: Vec<SemanticsError>
}

impl SemanticAnalysis {
    pub fn new(functions: HashMap<String, Vec<String>>) -> SemanticAnalysis {
        SemanticAnalysis {
            vars_in_scope: HashSet::new(),
            functions,
            errors: vec![]
        }
    }

    fn analyse_for_loop(&mut self, variable_declaration: Option<Node>, condition: Expression, loop_increment: Option<Node>, body: Vec<Node>) {

        if let Some(variable_declaration) = variable_declaration { match variable_declaration {
            Node::VariableDeclaration { identifier, value } => self.analyse_variable_declaration(identifier, value),
            _ => panic!("Unreachable")
        }}

        self.analyse_expression(condition);

        if let Some(loop_increment) = loop_increment { match loop_increment {
            Node::VariableAssignment { identifier: _, value } => self.analyse_expression(value),
            _ => panic!("Unreachable")
        }}

        let vars_in_scope = self.vars_in_scope.clone();
        self.analyse_body(body);
        self.vars_in_scope = vars_in_scope;
    }
    
    fn analyse_if(&mut self, condition: Expression, body: Vec<Node>, else_body: Option<Vec<Node>>) {
        
        self.analyse_expression(condition);

        let vars_in_scope = self.vars_in_scope.clone();
        self.analyse_body(body);
        self.vars_in_scope = vars_in_scope;
        if let Some(else_body) = else_body {
            let vars_in_scope = self.vars_in_scope.clone();
            self.analyse_body(else_body);
            self.vars_in_scope = vars_in_scope;
        }
    }
    
    fn analyse_expression(&mut self, expression: Expression) {
        match expression {
            Expression::Value(value) => {
                if !value.chars().nth(0).expect("Unreachable").is_numeric() && !self.vars_in_scope.contains(&value) {
                    self.errors.push(SemanticsError::UninitializedVariable(value));
                }
            },
            Expression::Function(identifier, arguments) => {
                if let Some(expected_arguments) = self.functions.get(&identifier) {
                    if expected_arguments.len() != arguments.len() {
                        self.errors.push(SemanticsError::IncorrectFunctionArguments(identifier, expected_arguments.len(), arguments.len()));
                    }
                }
                else {  
                    self.errors.push(SemanticsError::UninitializedFunction(identifier));
                }
            },
            Expression::Block(block) => {
                for expression in block {
                    self.analyse_expression(expression);
                }
            },
            Expression::Operator(_) => {},
            Expression::ParenthesisOpen(_) | Expression::ParenthesisClose(_) => panic!("Unreachable")
        }
    }
    
    fn analyse_variable_assignment(&mut self, identifier: String, value: Expression) {
        if !self.vars_in_scope.contains(&identifier) {
            self.errors.push(SemanticsError::UninitializedVariable(identifier));
        }

        self.analyse_expression(value);
    }
    
    fn analyse_variable_declaration(&mut self, identifier: String, value: Expression) {
        self.analyse_expression(value);
        self.vars_in_scope.insert(identifier);
    }
    
    fn analyse_while_loop(&mut self, condition: Expression, body: Vec<Node>) {
        self.analyse_expression(condition);
        let vars_in_scope = self.vars_in_scope.clone();
        self.analyse_body(body);
        self.vars_in_scope = vars_in_scope;
    }

    fn analyse_body(&mut self, nodes: Vec<Node>) {

        for node in nodes {
            match node {
                Node::For { variable, condition, loop_increment, body } => self.analyse_for_loop(*variable, condition, *loop_increment, body),
                Node::Function { .. } => self.errors.push(SemanticsError::FunctionDefinitionInFunction),
                Node::If { condition, body } => self.analyse_if(condition, body, None),
                Node::IfElse { condition, body, else_body } => self.analyse_if(condition, body, Some(else_body)),
                Node::Out { value } => self.analyse_expression(value),
                Node::Return { value } => if let Some(value) = value { self.analyse_expression(value) },
                Node::VariableAssignment { identifier, value } => self.analyse_variable_assignment(identifier, value),
                Node::VariableDeclaration { identifier, value } => self.analyse_variable_declaration(identifier, value),
                Node::While { condition, body } => self.analyse_while_loop(condition, body),
                Node::Error { .. } => panic!("Unreachable")
            };
        }
    }
    
    pub fn analyse(&mut self, nodes: &Vec<Node>) -> Vec<SemanticsError> {

        if !self.functions.contains_key(&"main".to_string()) {
            self.errors.push(SemanticsError::MissingMainFunction);
        }

        // Check for duplicates
        let mut functions = HashSet::new();
        
        for node in nodes {
            match node {
                Node::Function { identifier, arguments, body } => {

                    if !functions.insert(identifier) {
                        self.errors.push(SemanticsError::FunctionDefinedTwice(identifier.clone()));
                    }

                    for argument in arguments {
                        self.vars_in_scope.insert(argument.clone());
                    }
                    self.analyse_body(body.clone());
                    self.vars_in_scope.clear();
                },
                _ => self.errors.push(SemanticsError::UnwrappedCode)
            }
        }
    
        self.errors.clone()
    }
}

impl std::fmt::Display for SemanticsError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {

        let out = match self {
            SemanticsError::UnwrappedCode => "Unwrapped Code".to_string(),
            SemanticsError::FunctionDefinedTwice(identifier) => format!("Function: '{}' defined twice", identifier),
            SemanticsError::FunctionDefinitionInFunction => "Function definitions are not allowed within funcitons".to_string(),
            SemanticsError::UninitializedFunction(identifier) => format!("Uninitialized function: {}", identifier),
            SemanticsError::UninitializedVariable(identifier) => format!("Uninitialized variable: {}", identifier),
            SemanticsError::MissingMainFunction => "Missing main function".to_string(),
            SemanticsError::IncorrectFunctionArguments(identifier, expected_args, args) => 
                format!("Function: '{}' expected {} arguments but only received {}", identifier, expected_args, args)
        };
        write!(f, "{}", out)
    }
}