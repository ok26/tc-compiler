use std::cmp::max;

use crate::lexer::{PunctuationKind, Token, TokenType};

#[derive(Clone, Debug)]
pub enum Expression {
    Value(String),
    Function(String, Vec<Expression>),
    Operator(String),
    ParenthesisOpen(usize),
    ParenthesisClose(usize),
    Block(Vec<Expression>)
}

#[derive(Clone, Debug)]
pub enum AstErrorType {
    ExpectedIdentifier,
    ReservedKeyword,
    ExpectedAssignment,
    ExpectedValue,
    InvalidFunctionArguments,
    ExpectedOperator,
    ExpectedBody,
    ExpectedParenthesis,
    ExpectedIntegerOperator,
    SyntaxError,
    UnevenParenthesis,
    ExpectedExpression
}

#[derive(Clone, Debug)]
pub enum Node {
    If {
        condition: Expression,
        body: Vec<Node>
    },
    IfElse {
        condition: Expression,
        body: Vec<Node>,
        else_body: Vec<Node>,
    },
    While {
        condition: Expression,
        body: Vec<Node>
    },
    For {
        variable: Box<Option<Node>>,
        condition: Expression,
        loop_increment: Box<Option<Node>>,
        body: Vec<Node>
    },
    VariableAssignment {
        identifier: String,
        value: Expression,
    },
    Function {
        identifier: String,
        arguments: Vec<String>,
        body: Vec<Node>
    },
    Return {
        value: Option<Expression>
    },
    FunctionCall {
        identifier: String,
        arguments: Vec<Expression>
    },
    Error {
        ty: AstErrorType,
        row: usize,
        column: usize
    }
}

pub struct Ast {
    tokens: Vec<Token>,
    i: usize
}

impl Ast {
    pub fn new(tokens: Vec<Token>) -> Ast {
        Ast {
            tokens,
            i: 0
        }
    }

    fn is_reserved_keyword(keyword: &String) -> bool {
        match keyword.as_str() {
            "for" | "fn" | "while" | "else" | "if" | "let" | "return" => return true,
            _ => return false
        }
    }

    fn is_int_operator(operator: &String) -> bool {
        match operator.as_str() {
            "+" | "-" | "&" | "|" | "^" | "<<" | ">>" => return true,
            _ => return false
        };
    }

    fn skip_line(&mut self) {
        while self.i < self.tokens.len() && !";}{".contains(self.tokens[self.i].raw.as_str()) { self.i += 1; }
        self.i += 1;
    }

    fn parse_variable_assignment(&mut self, terminator: &str) -> Node {

        let token = &self.tokens[self.i];
        self.i += 1;

        if token.ty != TokenType::Identifier {
            return Node::Error {
                ty: AstErrorType::ExpectedIdentifier,
                row: token.row,
                column: token.column
            };
        }

        if Ast::is_reserved_keyword(&token.raw) {
            return Node::Error {
                ty: AstErrorType::ReservedKeyword,
                row: token.row,
                column: token.column
            };
        }

        let identifier = token.raw.clone();
        let token = &self.tokens[self.i];
        self.i += 1;
        
        if token.raw != String::from("=") {
            return Node::Error {
                ty: AstErrorType::ExpectedAssignment,
                row: token.row,
                column: token.column
            };
        }

        match self.parse_expression(vec![terminator]) {
            Ok(expression) => return Node::VariableAssignment {
                identifier,
                value: expression
            },
            Err(error) => return error
        }
    }

    fn parse_function_arguments(&mut self) -> Option<Vec<String>> {
        let mut arguments = vec![];
        if self.tokens[self.i].raw == ")" {
            self.i += 1;
            return Some(arguments);
        }
        loop {
            let argument = &self.tokens[self.i];
            self.i += 1;
            if argument.ty != TokenType::Identifier && argument.ty != TokenType::Number { return None; }
            arguments.push(argument.raw.clone());
            let seperator = &self.tokens[self.i];
            self.i += 1;
            if seperator.raw == ")" { break; }
            if seperator.raw != "," { return None; }
        }
        Some(arguments)
    }

    fn parse_called_function_arguments(&mut self) -> Result<Vec<Expression>, Node> {
        let mut arguments = vec![];
        if self.tokens[self.i].raw == ")" {
            self.i += 1;
            return Ok(arguments);
        }
        loop {
            match self.parse_expression(vec![")", ","]) {
                Ok(expression) => {
                    arguments.push(expression);
                    if self.tokens[self.i - 1].raw == ")" { return Ok(arguments); }
                },
                Err(error) => return Err(error)
            };
        }
    }

    fn parse_expression(&mut self, terminators: Vec<&str>) -> Result<Expression, Node> {
        let mut expression_block: Vec<Expression> = vec![];
        let mut parenthesis_number = 0;
        let mut max_parenthesis_number = 0;

        loop {
            let value = self.tokens[self.i].clone();
            self.i += 1;
            if value.raw == "(" {
                max_parenthesis_number = max(max_parenthesis_number, parenthesis_number);
                expression_block.push(Expression::ParenthesisOpen(parenthesis_number));
                parenthesis_number += 1;
                continue;
            }

            if value.ty != TokenType::Identifier && value.ty != TokenType::Number {
                return Err(Node::Error {
                    ty: AstErrorType::ExpectedValue,
                    row: value.row,
                    column: value.column
                })
            }

            let mut operator = self.tokens[self.i].clone();
            self.i += 1;

            // Function
            if operator.raw == "(" {

                // Numbers not allowed as function names
                if operator.ty == TokenType::Number {
                    return Err(Node::Error {
                        ty: AstErrorType::ExpectedIdentifier,
                        row: value.row,
                        column: value.column
                    })
                }

                match self.parse_called_function_arguments() {
                    Ok(arguments) => {
                        expression_block.push(Expression::Function(value.raw, arguments));
                        operator = self.tokens[self.i].clone();
                        self.i += 1;
                    },
                    Err(error) => return Err(error)
                }
            }
            else {
                expression_block.push(Expression::Value(value.raw));
            }

            while operator.raw == ")" {

                if terminators.contains(&operator.raw.as_str()) && (operator.raw != ")" || parenthesis_number == 0) { break; }

                if parenthesis_number == 0 {
                    println!("{}, {}", terminators[0], parenthesis_number);
                    return Err(Node::Error {
                        ty: AstErrorType::UnevenParenthesis,
                        row: operator.row,
                        column: operator.column
                    })
                }

                parenthesis_number -= 1;
                expression_block.push(Expression::ParenthesisClose(parenthesis_number));
                operator = self.tokens[self.i].clone();
                self.i += 1;
            }

            if terminators.contains(&operator.raw.as_str()) && (operator.raw != ")" || parenthesis_number == 0) { break; }

            if operator.ty != TokenType::Operator {
                return Err(Node::Error {
                    ty: AstErrorType::ExpectedOperator,
                    row: operator.row,
                    column: operator.column
                })
            }

            if !Ast::is_int_operator(&operator.raw) {
                return Err(Node::Error {
                    ty: AstErrorType::ExpectedIntegerOperator,
                    row: operator.row,
                    column: operator.column
                })
            }

            expression_block.push(Expression::Operator(operator.raw));

        }

        if parenthesis_number != 0 {
            println!("{}", parenthesis_number);
            return Err(Node::Error {
                ty: AstErrorType::UnevenParenthesis,
                row: self.tokens[self.i - 1].row,
                column: self.tokens[self.i - 1].column
            })
        }

        let mut parenthesis_number = max_parenthesis_number;
        loop {
            let mut new_block: Vec<Expression> = vec![];
            let mut i = 0;
            while i < expression_block.len() {
                let expression = &expression_block[i];

                if let Expression::ParenthesisOpen(number) = expression {

                    if *number != parenthesis_number {
                        new_block.push(expression.clone());
                        i = i + 1;
                        continue;
                    }

                    // Compress expression block
                    let mut compressed_block: Vec<Expression> = vec![];
                    i = i + 1;
                    loop {
                        let expression = &expression_block[i];
                        if let Expression::ParenthesisClose(number) = expression { if *number == parenthesis_number { break; } }
                        compressed_block.push(expression.clone());
                        i = i + 1;
                    }
                    new_block.push(Expression::Block(compressed_block));
                }
                else { new_block.push(expression.clone()); }

                i = i + 1;
            }
            expression_block = new_block;

            if parenthesis_number == 0 { break; }
            parenthesis_number -= 1;
        }

        // Easy to add more hierarchy for operations and not just parenthesis in future

        Ok(Expression::Block(expression_block))
    }

    fn parse_boolean_expression(&mut self, terminator: &str) -> Result<Expression, Node> {
        match self.parse_expression(vec!["<", ">", "<=", ">=", "==", "!="]) {
            Ok(left_expression) => {
                let boolean_operator = self.tokens[self.i - 1].clone();
                match self.parse_expression(vec![terminator]) {
                    Ok(right_expression) => return Ok(Expression::Block (
                        vec![left_expression, Expression::Operator(boolean_operator.raw), right_expression]
                    )),
                    Err(error) => return Err(error)
                }
            },
            Err(error) => return Err(error)
        }
    }

    fn parse_function(&mut self) -> Node {
        let token = self.tokens[self.i].clone();
        self.i += 1;

        if token.ty != TokenType::Identifier {
            return Node::Error {
                ty: AstErrorType::ExpectedIdentifier,
                row: token.row,
                column: token.column
            }
        }

        let seperator = self.tokens[self.i].clone();
        self.i += 1;
        if seperator.raw != "(" {
            return Node::Error {
                ty: AstErrorType::InvalidFunctionArguments,
                row: seperator.row,
                column: seperator.column
            }
        }

        if let Some(arguments) = self.parse_function_arguments() {

            let body_open = self.tokens[self.i].clone();
            self.i += 1;
            if let TokenType::Punctuation(PunctuationKind::Open(body_number)) = body_open.ty {

                if body_open.raw == "{" {
                    return Node::Function {
                        identifier: token.raw.clone(),
                        arguments,
                        body: self.parse_body(body_number)
                    }
                }
            }
            
            return Node::Error {
                ty: AstErrorType::ExpectedBody,
                row: body_open.row,
                column: body_open.column
            }
        }
        else {
            return Node::Error {
                ty: AstErrorType::InvalidFunctionArguments,
                row: seperator.row,
                column: seperator.column
            }
        }
    }

    fn parse_body(&mut self, body_number: usize) -> Vec<Node> {
        let mut body = vec![];
        loop {

            if let TokenType::Punctuation(PunctuationKind::Close(bracket_number)) = self.tokens[self.i].ty {
                if self.tokens[self.i].raw == "}" && bracket_number == body_number { self.i += 1; break; }
            }

            if let Some(node) = self.parse_line() { body.push(node); }
            else { break; }
        }
        body
    }

    fn parse_return_keyword(&mut self) -> Node {
        let token = self.tokens[self.i].clone();
        if token.raw == ";" {
            self. i += 1;
            return Node::Return { value: None }
        }   
        match self.parse_expression(vec![";"]) {
            Ok(expression) => return Node::Return { value: Some(expression) },
            Err(error) => return error
        }
    }

    fn parse_if_statment(&mut self) -> Node {
        let token = self.tokens[self.i].clone();
        self.i += 1;
        if token.raw != "(" {
            return Node::Error {
                ty: AstErrorType::ExpectedParenthesis,
                row: token.row,
                column: token.column
            }
        }

        match self.parse_boolean_expression(")") {
            Ok(expression) => {

                let body_open = self.tokens[self.i].clone();
                self.i += 1;
                if let TokenType::Punctuation(PunctuationKind::Open(bracket_number)) = body_open.ty {

                    if body_open.raw == "{" {

                        let body = self.parse_body(bracket_number);
                        let token = self.tokens[self.i].clone();
                        if token.raw == "else" {

                            self.i += 1;
                            let body_open = self.tokens[self.i].clone();
                            self.i += 1;
                            if let TokenType::Punctuation(PunctuationKind::Open(bracket_number)) = body_open.ty {

                                if body_open.raw == "{" {
                                    return Node::IfElse {
                                        condition: expression,
                                        body,
                                        else_body: self.parse_body(bracket_number)
                                    }
                                }
                            }

                            return Node::Error {
                                ty: AstErrorType::ExpectedBody,
                                row: body_open.row,
                                column: body_open.column
                            }
                        }

                        return Node::If {
                            condition: expression,
                            body
                        }
                    }
                }
                
                return Node::Error {
                    ty: AstErrorType::ExpectedBody,
                    row: body_open.row,
                    column: body_open.column
                };
            },
            Err(error) => return error
        }
    }

    fn parse_while_loop(&mut self) -> Node {
        let token = self.tokens[self.i].clone();
        self.i += 1;
        if token.raw != "(" {
            return Node::Error {
                ty: AstErrorType::ExpectedParenthesis,
                row: token.row,
                column: token.column
            }
        }

        match self.parse_boolean_expression(")") {
            Ok(expression) => {
                let body_open = self.tokens[self.i].clone();
                self.i += 1;
                if let TokenType::Punctuation(PunctuationKind::Open(bracket_number)) = body_open.ty {

                    if body_open.raw == "{" {
                        return Node::While {
                            condition: expression,
                            body: self.parse_body(bracket_number)
                        }
                    }
                }

                return Node::Error {
                    ty: AstErrorType::ExpectedBody,
                    row: body_open.row,
                    column: body_open.column
                }
            },
            Err(error) => return error
        }
    }

    fn parse_for_loop(&mut self) -> Node {

        let token = self.tokens[self.i].clone();
        self.i += 1;
        if token.raw != "(" {
            return Node::Error {
                ty: AstErrorType::ExpectedParenthesis,
                row: token.row,
                column: token.column
            }
        }

        let variable_assignment: Option<Node>;
        let token = self.tokens[self.i].clone();
        self.i += 1;

        if token.raw == "let" {
            variable_assignment = Some(self.parse_variable_assignment(";"));
            match variable_assignment {
                Some(Node::Error { .. }) => return variable_assignment.expect("Unreachable"),
                _ => {}
            }
        }
        else if token.raw == ";" { variable_assignment = None; }
        else {
            return Node::Error {
                ty: AstErrorType::SyntaxError,
                row: token.row,
                column: token.column
            }
        }

        let token = self.tokens[self.i].clone();
        let expression: Expression;

        if token.raw == ";" { 
            return Node::Error {
                ty: AstErrorType::ExpectedExpression,
                row: token.row,
                column: token.column
            }
        }
        else {
            match self.parse_boolean_expression(";") {
                Ok(boolean_expression) => { expression = boolean_expression; },
                Err(error) => return error
            }
        }

        let token = self.tokens[self.i].clone();
        let loop_increment: Option<Node>;

        if token.raw == ")" {
            loop_increment = None;
            self.i += 1;
        }
        else {
            loop_increment = Some(self.parse_variable_assignment(")"));
            match loop_increment {
                Some(Node::Error { .. }) => { return loop_increment.expect("Unreachable"); }
                _ => {}
            }
        }

        let body_open = self.tokens[self.i].clone();
        self.i += 1;
        if let TokenType::Punctuation(PunctuationKind::Open(bracket_number)) = body_open.ty {

            if body_open.raw == "{" {
                return Node::For {
                    variable: Box::new(variable_assignment),
                    condition: expression,
                    loop_increment: Box::new(loop_increment),
                    body: self.parse_body(bracket_number)
                }
            }
        }

        return Node::Error {
            ty: AstErrorType::ExpectedBody,
            row: body_open.row,
            column: body_open.column
        }
    }

    fn parse_line(&mut self) -> Option<Node> {
        
        let token = &self.tokens[self.i.min(self.tokens.len() - 1)];
        if token.ty == TokenType::EOF { return None; }
        self.i += 1;

        if token.ty != TokenType::Identifier {
            return Some(Node::Error {
                ty: AstErrorType::ExpectedIdentifier,
                row: token.row,
                column: token.column
            });
        }

        let ret = match token.raw.as_str() {
            "let" => self.parse_variable_assignment(";"),
            "for" => self.parse_for_loop(),
            "fn" => self.parse_function(),
            "if" => self.parse_if_statment(),
            "while" => self.parse_while_loop(), 
            "return" => self.parse_return_keyword(), 
            _ => {
                self.i -= 1;
                self.parse_variable_assignment(";")
            }
        };

        match ret {
            Node::Error { .. } => self.skip_line(),
            _ => {}
        };

        return Some(ret);
    }

    pub fn generate_ast(&mut self) -> Vec<Node> {
        let mut ast: Vec<Node> = vec![];
        loop {
            if let Some(node) = self.parse_line() { ast.push(node); }
            else { break; }
        }
        ast
    }
}

fn convert_expression_to_string(expression: &Expression) -> String {
    return match expression {
        Expression::Value(value) => format!("{} ", value.clone()),
        Expression::Function(identifier, arguments) => {

            let mut out = format!("{}(", identifier);
            for (i, argument) in arguments.iter().enumerate() {

                if i != arguments.len() - 1 { out.push_str(format!("{}, ", argument).as_str()); }
                else { out.push_str(format!("{}) ", argument).as_str()); }
            }
            out
        },
        Expression::Operator ( operator ) => format!("{} ", operator.clone()),
        Expression::Block(block) => {
            let mut out = String::new();
            for expression in block {
                out.push_str(&convert_expression_to_string(expression).as_str());
            }
            out
        },
        Expression::ParenthesisOpen(_) | Expression::ParenthesisClose(_) => panic!("Unreachable"),
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", convert_expression_to_string(self))
    }
}

impl std::fmt::Display for AstErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let out = match self {
            AstErrorType::ExpectedAssignment => "Expected Assignment",
            AstErrorType::ExpectedBody => "Expected Body",
            AstErrorType::ExpectedIdentifier => "Expected Identifier",
            AstErrorType::ExpectedIntegerOperator => "Expected Integer Operation",
            AstErrorType::ExpectedOperator => "Expected Operator",
            AstErrorType::ExpectedParenthesis => "Expected Parenthesis",
            AstErrorType::InvalidFunctionArguments => "Invalid Function Arguments",
            AstErrorType::ExpectedValue => "Expected Value",
            AstErrorType::ReservedKeyword => "Reserved Keyword",
            AstErrorType::SyntaxError => "Syntax Error",
            AstErrorType::UnevenParenthesis => "Uneven Parenthesis",
            AstErrorType::ExpectedExpression => "Expected Expression"
        };
        write!(f, "{}", out)
    }
}

fn convert_node_to_string(node: &Node, inc: usize) -> String {
    return match node {
        Node::VariableAssignment { identifier, value } => format!("{}Variable assignment: {} = {}\n", "\t".to_string().repeat(inc), identifier, value),
        Node::Error { ty, row, column } => format!("{}Error: {} on line {}, column: {}\n", "\t".to_string().repeat(inc), ty, row, column),
        Node::Return { value } => {
            if let Some(value) = value { format!("{}Return {}\n", "\t".to_string().repeat(inc), value) }
            else { format!("{}Return\n", "\t".to_string().repeat(inc)) }
        },
        Node::If { condition, body } => {
            let mut out = format!("{}If: Condition: {}\n", "\t".to_string().repeat(inc), condition);
            for part in body {
                out.push_str(format!("{}", convert_node_to_string(part, inc + 1)).as_str());
            }
            out
        },
        Node::IfElse { condition, body, else_body } => {
            let mut out = format!("{}If: Condition {}\n", "\t".to_string().repeat(inc), condition);
            for part in body {
                out.push_str(format!("{}", convert_node_to_string(part, inc + 1)).as_str());
            }
            out.push_str(format!("{}Else:\n", "\t".to_string().repeat(inc)).as_str());
            for part in else_body {
                out.push_str(format!("{}", convert_node_to_string(part, inc + 1)).as_str());
            }
            out
        },
        Node::While { condition, body } => {
            let mut out = format!("{}While: Condition: {}\n", "\t".to_string().repeat(inc), condition);
            for part in body {
                out.push_str(format!("{}", convert_node_to_string(part, inc + 1)).as_str());
            }
            out
        },
        Node::For { variable, condition, loop_increment, body } => {

            let mut variable_out = match &**variable {
                Some(variable) => format!("{}", variable),
                None => String::new()
            };
            variable_out.pop();
            let mut loop_increment_out = match &**loop_increment {
                Some(loop_increment) => format!("{}", loop_increment),
                None => String::new()
            };
            loop_increment_out.pop();
            let mut out = format!("{}For: {}, Condition: {}, Loop increment: {}\n", "\t".to_string().repeat(inc), variable_out, condition, loop_increment_out);
            for part in body {
                out.push_str(format!("{}", convert_node_to_string(part, inc + 1)).as_str());
            }
            out
        },
        Node::Function { identifier, arguments, body } => {
            let mut out = format!("{}Function: {}, Arguments: (", "\t".to_string().repeat(inc), identifier);
            for (i, argument) in arguments.iter().enumerate() {
                if i != arguments.len() - 1 { out.push_str(format!("{}, ", argument).as_str()); }
                else { out.push_str(format!("{})\n", argument).as_str()); }
            }
            for part in body {
                out.push_str(format!("{}", convert_node_to_string(part, inc + 1)).as_str());
            }
            out
        },
        Node::FunctionCall { identifier, arguments } => {
            let mut out = format!("{}{}(", "\t".to_string().repeat(inc), identifier);
            for (i, argument) in arguments.iter().enumerate() {

                if i != arguments.len() - 1 { out.push_str(format!("{}, ", argument).as_str()); }
                else { out.push_str(format!("{})\n", argument).as_str()); }
            }
            out
        }
    }
}

impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", convert_node_to_string(self, 0))
    }
}