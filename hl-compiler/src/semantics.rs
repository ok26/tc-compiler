use std::collections::{HashMap, HashSet};

use crate::ast::{expressions::{Expression, ExpressionType}, ast::{Node, NodeType}};

#[derive(Clone)]
pub enum SemanticsErrorType {
    UnwrappedCode,
    FunctionDefinitionInFunction,
    MissingMainFunction,
    FunctionDefinedTwice(String),
    UninitializedVariable(String),
    UninitializedFunction(String),
    IncorrectFunctionArguments(String, usize, usize)
}

#[derive(Clone)]
pub struct SemanticsError {
    pub ty: SemanticsErrorType,
    pub row: usize,
    pub column: usize
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

        if let Some(var_decl) = variable_declaration { match var_decl.ty {
            NodeType::VariableDeclaration { identifier, value } => self.analyse_variable_declaration(identifier, value),
            _ => panic!("Unreachable")
        }}

        self.analyse_expression(condition);

        if let Some(loop_increment) = loop_increment { match loop_increment.ty {
            NodeType::VariableAssignment { identifier: _, value } => self.analyse_expression(value),
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
        match expression.ty {
            ExpressionType::Value(value) => {
                if !value.chars().nth(0).expect("Unreachable").is_numeric() && !self.vars_in_scope.contains(&value) {
                    self.errors.push(SemanticsError {
                        ty: SemanticsErrorType::UninitializedVariable(value),
                        row: expression.row,
                        column: expression.column
                    });
                }
            },
            ExpressionType::Function(identifier, arguments) => {
                if let Some(expected_arguments) = self.functions.get(&identifier) {
                    if expected_arguments.len() != arguments.len() {
                        self.errors.push(SemanticsError {
                            ty: SemanticsErrorType::IncorrectFunctionArguments(identifier, expected_arguments.len(), arguments.len()),
                            row: expression.row,
                            column: expression.column 
                        });
                    }
                }
                else {  
                    self.errors.push(SemanticsError {
                        ty: SemanticsErrorType::UninitializedFunction(identifier),
                        row: expression.row,
                        column: expression.column
                    });
                }
            },
            ExpressionType::Block(block) => {
                for expression in block {
                    self.analyse_expression(expression);
                }
            },
            ExpressionType::Operator(_) => {},
            ExpressionType::ParenthesisOpen(_) | ExpressionType::ParenthesisClose(_) => panic!("Unreachable")
        }
    }
    
    fn analyse_variable_assignment(&mut self, identifier: String, value: Expression, row: usize, column: usize) {
        if !self.vars_in_scope.contains(&identifier) {
            self.errors.push(SemanticsError {
                ty: SemanticsErrorType::UninitializedVariable(identifier),
                row,
                column
            });
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
            match node.ty {
                NodeType::For { variable, condition, loop_increment, body } => self.analyse_for_loop(*variable, condition, *loop_increment, body),
                NodeType::If { condition, body } => self.analyse_if(condition, body, None),
                NodeType::IfElse { condition, body, else_body } => self.analyse_if(condition, body, Some(else_body)),
                NodeType::Out { value } => self.analyse_expression(value),
                NodeType::Return { value } => if let Some(value) = value { self.analyse_expression(value) },
                NodeType::VariableAssignment { identifier, value } => self.analyse_variable_assignment(identifier, value, node.row, node.column),
                NodeType::VariableDeclaration { identifier, value } => self.analyse_variable_declaration(identifier, value),
                NodeType::While { condition, body } => self.analyse_while_loop(condition, body),
                NodeType::Function { .. } => self.errors.push(SemanticsError {
                    ty: SemanticsErrorType::FunctionDefinitionInFunction,
                    row: node.row,
                    column: node.column
                }),
                NodeType::Error { .. } => panic!("Unreachable")
            };
        }
    }
    
    pub fn analyse(&mut self, nodes: &Vec<Node>) -> Vec<SemanticsError> {

        if !self.functions.contains_key(&"main".to_string()) {
            self.errors.push(SemanticsError {
                ty: SemanticsErrorType::MissingMainFunction,
                row: 0,
                column: 0
            });
        }

        // Check for duplicates
        let mut functions = HashSet::new();
        
        for node in nodes {
            match &node.ty {
                NodeType::Function { identifier, arguments, body } => {

                    if !functions.insert(identifier) {
                        self.errors.push(SemanticsError {
                            ty: SemanticsErrorType::FunctionDefinedTwice(identifier.clone()),
                            row: node.row,
                            column: node.column
                        });
                    }

                    for argument in arguments {
                        self.vars_in_scope.insert(argument.clone());
                    }
                    self.analyse_body(body.clone());
                    self.vars_in_scope.clear();
                },
                _ => self.errors.push(SemanticsError {
                    ty: SemanticsErrorType::UnwrappedCode,
                    row: node.row,
                    column: node.column
                })
            }
        }
    
        self.errors.clone()
    }
}

impl std::fmt::Display for SemanticsError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {

        let out = match &self.ty {
            SemanticsErrorType::UnwrappedCode => "Unwrapped Code".to_string(),
            SemanticsErrorType::FunctionDefinedTwice(identifier) => format!("Function: '{}' defined twice", identifier),
            SemanticsErrorType::FunctionDefinitionInFunction => "Function definitions are not allowed within funcitons".to_string(),
            SemanticsErrorType::UninitializedFunction(identifier) => format!("Uninitialized function: {}", identifier),
            SemanticsErrorType::UninitializedVariable(identifier) => format!("Uninitialized variable: {}", identifier),
            SemanticsErrorType::MissingMainFunction => "Missing main function".to_string(),
            SemanticsErrorType::IncorrectFunctionArguments(identifier, expected_args, args) => 
                format!("Function: '{}' expected {} arguments but received {}", identifier, expected_args, args)
        };
        write!(f, "{} on line {}, column: {}", out, self.row, self.column)
    }
}