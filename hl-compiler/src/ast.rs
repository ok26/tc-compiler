use std::cmp::max;

use crate::lexer::{PunctuationKind, Token, TokenType};

#[derive(Clone)]
pub enum ExpressionType {
    Value(String),
    Function(String, Vec<Expression>),
    Operator(String),
    ParenthesisOpen(usize),
    ParenthesisClose(usize),
    Block(Vec<Expression>)
}

#[derive(Clone)]
pub struct Expression {
    pub ty: ExpressionType,
    pub row: usize,
    pub column: usize
}

#[derive(Clone)]
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
    ExpectedExpression,
    ExpectedEndOfLine
}

#[derive(Clone)]
pub enum NodeType {
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
    VariableDeclaration {
        identifier: String,
        value: Expression
    },
    Function {
        identifier: String,
        arguments: Vec<String>,
        body: Vec<Node>
    },
    Return {
        value: Option<Expression>
    },
    Out {
        value: Expression
    },
    Error (AstErrorType)
}

#[derive(Clone)]
pub struct Node {
    pub ty: NodeType,
    pub row: usize,
    pub column: usize
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
            "for" | "fn" | "while" | "else" | "if" | "let" | "return" | "out" => return true,
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

    fn parse_variable_assignment(&mut self, terminator: &str, is_declaration: bool) -> Node {

        let token = &self.tokens[self.i];
        self.i += 1;

        if token.ty != TokenType::Identifier {
            return Node {
                ty: NodeType::Error(AstErrorType::ExpectedIdentifier),
                row: token.row,
                column: token.column
            };
        }

        if Ast::is_reserved_keyword(&token.raw) {
            return Node {
                ty: NodeType::Error(AstErrorType::ReservedKeyword),
                row: token.row,
                column: token.column
            };
        }

        let identifier = token.raw.clone();
        let token = self.tokens[self.i].clone();
        self.i += 1;
        
        if token.raw != String::from("=") {
            return Node {
                ty: NodeType::Error(AstErrorType::ExpectedAssignment),
                row: token.row,
                column: token.column
            };
        }

        let value = match self.parse_expression(vec![terminator]) {
            Ok(expression) => expression,
            Err(error) => return error
        };

        if is_declaration {
            return Node {
                ty: NodeType::VariableDeclaration {
                    identifier,
                    value
                },
                row: token.row,
                column: token.column
            };
        }
        else {
            return Node {
                ty: NodeType::VariableAssignment {
                    identifier,
                    value
                },
                row: token.row,
                column: token.column
            };
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
                expression_block.push(Expression {
                    ty: ExpressionType::ParenthesisOpen(parenthesis_number),
                    row: value.row,
                    column: value.column
                });
                parenthesis_number += 1;
                continue;
            }

            if value.ty != TokenType::Identifier && value.ty != TokenType::Number {
                return Err(Node {
                    ty: NodeType::Error(AstErrorType::ExpectedValue),
                    row: value.row,
                    column: value.column
                });
            }

            let mut operator = self.tokens[self.i].clone();
            self.i += 1;

            // Function
            if operator.raw == "(" {

                // Numbers not allowed as function names
                if value.ty == TokenType::Number {
                    return Err(Node {
                        ty: NodeType::Error(AstErrorType::ExpectedIdentifier),
                        row: value.row,
                        column: value.column
                    });
                }

                match self.parse_called_function_arguments() {
                    Ok(arguments) => {
                        expression_block.push(Expression {
                            ty: ExpressionType::Function(value.raw, arguments),
                            row: value.row,
                            column: value.column
                        });
                        operator = self.tokens[self.i].clone();
                        self.i += 1;
                    },
                    Err(error) => return Err(error)
                }
            }
            else {
                expression_block.push(Expression {
                    ty: ExpressionType::Value(value.raw),
                    row: value.row,
                    column: value.column
                });
            }

            while operator.raw == ")" {

                if terminators.contains(&operator.raw.as_str()) && (operator.raw != ")" || parenthesis_number == 0) { break; }

                if parenthesis_number == 0 {
                    return Err(Node {
                        ty: NodeType::Error(AstErrorType::UnevenParenthesis),
                        row: operator.row,
                        column: operator.column
                    });
                }

                parenthesis_number -= 1;
                expression_block.push(Expression {
                    ty: ExpressionType::ParenthesisClose(parenthesis_number),
                    row: operator.row,
                    column: operator.column
                });
                operator = self.tokens[self.i].clone();
                self.i += 1;
            }

            if terminators.contains(&operator.raw.as_str()) && (operator.raw != ")" || parenthesis_number == 0) { break; }

            if operator.ty != TokenType::Operator {
                return Err(Node {
                    ty: NodeType::Error(AstErrorType::ExpectedOperator),
                    row: operator.row,
                    column: operator.column
                });
            }

            if !Ast::is_int_operator(&operator.raw) {
                return Err(Node {
                    ty: NodeType::Error(AstErrorType::ExpectedIntegerOperator),
                    row: operator.row,
                    column: operator.column
                });
            }

            expression_block.push(Expression {
                ty: ExpressionType::Operator(operator.raw),
                row: operator.row,
                column: operator.column
            });
        }

        if parenthesis_number != 0 {
            return Err(Node {
                ty: NodeType::Error(AstErrorType::UnevenParenthesis),
                row: self.tokens[self.i - 1].row,
                column: self.tokens[self.i - 1].column
            });
        }

        let mut parenthesis_number = max_parenthesis_number;
        loop {
            let mut new_block: Vec<Expression> = vec![];
            let mut i = 0;
            while i < expression_block.len() {
                let expression = &expression_block[i];
                
                if let ExpressionType::ParenthesisOpen(number) = expression.ty {

                    if number != parenthesis_number {
                        new_block.push(expression.clone());
                        i = i + 1;
                        continue;
                    }

                    // Compress expression block
                    let mut compressed_block: Vec<Expression> = vec![];
                    i = i + 1;
                    loop {
                        let expression = &expression_block[i];
                        if let ExpressionType::ParenthesisClose(number) = expression.ty { if number == parenthesis_number { break; } }
                        compressed_block.push(expression.clone());
                        i = i + 1;
                    }
                    new_block.push(Expression {
                        ty: ExpressionType::Block(compressed_block),
                        row: 0, // This will never really be read
                        column: 0 // Same here
                    });
                }
                else { new_block.push(expression.clone()); }

                i = i + 1;
            }
            expression_block = new_block;

            if parenthesis_number == 0 { break; }
            parenthesis_number -= 1;
        }

        // Easy to add more hierarchy for operations and not just parenthesis in future

        Ok(Expression {
            ty: ExpressionType::Block(expression_block),
            row: 0,
            column: 0
        })
    }

    fn parse_boolean_expression(&mut self, terminator: &str) -> Result<Expression, Node> {
        match self.parse_expression(vec!["<", ">", "<=", ">=", "==", "!="]) {
            Ok(left_expression) => {
                let boolean_operator = self.tokens[self.i - 1].clone();
                let boolean_operator_exp = Expression {
                    ty: ExpressionType::Operator(boolean_operator.raw),
                    row: boolean_operator.row,
                    column: boolean_operator.column
                };
                match self.parse_expression(vec![terminator]) {
                    Ok(right_expression) => return Ok(Expression {
                        ty: ExpressionType::Block(vec![left_expression, boolean_operator_exp, right_expression]),
                        row: 0,
                        column: 0
                    }),
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
            return Node {
                ty: NodeType::Error(AstErrorType::ExpectedIdentifier),
                row: token.row,
                column: token.column
            };
        }

        let seperator = self.tokens[self.i].clone();
        self.i += 1;
        if seperator.raw != "(" {
            return Node {
                ty: NodeType::Error(AstErrorType::InvalidFunctionArguments),
                row: seperator.row,
                column: seperator.column
            };
        }

        if let Some(arguments) = self.parse_function_arguments() {

            let body_open = self.tokens[self.i].clone();
            self.i += 1;
            if let TokenType::Punctuation(PunctuationKind::Open(body_number)) = body_open.ty {

                if body_open.raw == "{" {
                    return Node {
                        ty: NodeType::Function {
                            identifier: token.raw.clone(),
                            arguments,
                            body: self.parse_body(body_number)
                        },
                        row: seperator.row,
                        column: seperator.column
                    };
                }
            }

            return Node {
                ty: NodeType::Error(AstErrorType::ExpectedBody),
                row: body_open.row,
                column: body_open.column
            };
        }
        else {
            return Node {
                ty: NodeType::Error(AstErrorType::InvalidFunctionArguments),
                row: seperator.row,
                column: seperator.column
            };
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
            return Node {
                ty: NodeType::Return { value: None },
                row: token.row,
                column: token.column
            };
        }   
        match self.parse_expression(vec![";"]) {
            Ok(expression) => return Node {
                ty: NodeType::Return { value: Some(expression) },
                row: token.row,
                column: token.column
            },
            Err(error) => return error
        }
    }

    fn parse_if_statment(&mut self) -> Node {
        let token = self.tokens[self.i].clone();
        self.i += 1;
        if token.raw != "(" {
            return Node {
                ty: NodeType::Error(AstErrorType::ExpectedParenthesis),
                row: token.row,
                column: token.column
            };
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
                                    return Node {
                                        ty: NodeType::IfElse {
                                            condition: expression,
                                            body,
                                            else_body: self.parse_body(bracket_number)
                                        },
                                        row: token.row,
                                        column: token.column
                                    };
                                }
                            }

                            return Node {
                                ty: NodeType::Error(AstErrorType::ExpectedBody),
                                row: body_open.row,
                                column: body_open.column
                            };
                        }

                        return Node {
                            ty: NodeType::If {
                                condition: expression,
                                body
                            },
                            row: token.row,
                            column: token.column
                        };
                    }
                }

                return Node {
                    ty: NodeType::Error(AstErrorType::ExpectedBody),
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
            return Node {
                ty: NodeType::Error(AstErrorType::ExpectedParenthesis),
                row: token.row,
                column: token.column
            };
        }

        match self.parse_boolean_expression(")") {
            Ok(expression) => {
                let body_open = self.tokens[self.i].clone();
                self.i += 1;
                if let TokenType::Punctuation(PunctuationKind::Open(bracket_number)) = body_open.ty {

                    if body_open.raw == "{" {
                        return Node {
                            ty: NodeType::While {
                                condition: expression,
                                body: self.parse_body(bracket_number)
                            },
                            row: body_open.row,
                            column: body_open.column
                        };
                    }
                }

                return Node {
                    ty: NodeType::Error(AstErrorType::ExpectedBody),
                    row: body_open.row,
                    column: body_open.column
                };
            },
            Err(error) => return error
        }
    }

    fn parse_for_loop(&mut self) -> Node {

        let token = self.tokens[self.i].clone();
        self.i += 1;
        if token.raw != "(" {
            return Node {
                ty: NodeType::Error(AstErrorType::ExpectedParenthesis),
                row: token.row,
                column: token.column
            };
        }

        let variable_declaration: Option<Node>;
        let token = self.tokens[self.i].clone();
        self.i += 1;

        if token.raw == "let" {
            variable_declaration = Some(self.parse_variable_assignment(";", true));
            match &variable_declaration {
                Some(Node { ty, row: _, column: _ }) => match ty {
                    NodeType::Error(_) => return variable_declaration.expect("Unreachable"),
                    _ => {}
                },
                _ => {}
            }
        }
        else if token.raw == ";" { variable_declaration = None; }
        else {
            return Node {
                ty: NodeType::Error(AstErrorType::SyntaxError),
                row: token.row,
                column: token.column
            };
        }

        let token = self.tokens[self.i].clone();
        let expression: Expression;

        if token.raw == ";" { 
            return Node {
                ty: NodeType::Error(AstErrorType::ExpectedExpression),
                row: token.row,
                column: token.column
            };
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
            loop_increment = Some(self.parse_variable_assignment(")", false));
            match &loop_increment {
                Some(Node { ty, row: _, column: _ }) => match ty {
                    NodeType::Error(_) => return loop_increment.expect("Unreachable"),
                    _ => {}
                },
                _ => {}
            }
        }

        let body_open = self.tokens[self.i].clone();
        self.i += 1;
        if let TokenType::Punctuation(PunctuationKind::Open(bracket_number)) = body_open.ty {

            if body_open.raw == "{" {
                return Node {
                    ty: NodeType::For {
                        variable: Box::new(variable_declaration),
                        condition: expression,
                        loop_increment: Box::new(loop_increment),
                        body: self.parse_body(bracket_number)
                    },
                    row: body_open.row,
                    column: body_open.column
                };
            }
        }

        return Node {
            ty: NodeType::Error(AstErrorType::ExpectedBody),
            row: body_open.row,
            column: body_open.column
        };
    }

    fn parse_out_keyword(&mut self) -> Node {
        let token = self.tokens[self.i].clone();
        self.i += 1;

        if token.raw != "(" {
            return Node {
                ty: NodeType::Error(AstErrorType::ExpectedParenthesis),
                row: token.row,
                column: token.column
            };
        }

        let node = match self.parse_expression(vec![")"]) {
            Ok(value) => Node {
                ty: NodeType::Out { value },
                row: token.row,
                column: token.column
            },
            Err(error) => return error
        };

        let token = &self.tokens[self.i];
        self.i += 1;

        if token.raw != ";" {
            return Node {
                ty: NodeType::Error(AstErrorType::ExpectedEndOfLine),
                row: token.row,
                column: token.column
            };
        }

        return node;
    }

    fn parse_line(&mut self) -> Option<Node> {
        
        let token = &self.tokens[self.i.min(self.tokens.len() - 1)];
        if token.ty == TokenType::EOF { return None; }
        self.i += 1;

        if token.ty != TokenType::Identifier {
            return Some(Node {
                ty: NodeType::Error(AstErrorType::ExpectedIdentifier),
                row: token.row,
                column: token.column
            });
        }

        let ret = match token.raw.as_str() {
            "let" => self.parse_variable_assignment(";", true),
            "for" => self.parse_for_loop(),
            "fn" => self.parse_function(),
            "if" => self.parse_if_statment(),
            "while" => self.parse_while_loop(), 
            "return" => self.parse_return_keyword(), 
            "out" => self.parse_out_keyword(),
            _ => {
                self.i -= 1;
                self.parse_variable_assignment(";", false)
            }
        };

        match ret.ty {
            NodeType::Error { .. } => self.skip_line(),
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
    return match &expression.ty {
        ExpressionType::Value(value) => format!("{} ", value.clone()),
        ExpressionType::Function(identifier, arguments) => {

            let mut out = format!("{}(", identifier);
            for (i, argument) in arguments.iter().enumerate() {

                if i != arguments.len() - 1 { out.push_str(format!("{}, ", argument).as_str()); }
                else { out.push_str(format!("{}) ", argument).as_str()); }
            }
            out
        },
        ExpressionType::Operator ( operator ) => format!("{} ", operator.clone()),
        ExpressionType::Block(block) => {
            let mut out = String::new();
            for expression in block {
                out.push_str(&convert_expression_to_string(expression).as_str());
            }
            out
        },
        ExpressionType::ParenthesisOpen(_) | ExpressionType::ParenthesisClose(_) => panic!("Unreachable"),
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
            AstErrorType::ExpectedExpression => "Expected Expression",
            AstErrorType::ExpectedEndOfLine => "Expected End of Line"
        };
        write!(f, "{}", out)
    }
}

fn convert_node_to_string(node: &Node, inc: usize) -> String {
    return match &node.ty {
        NodeType::VariableAssignment { identifier, value } => format!("{}Variable assignment: {} = {}\n", "\t".to_string().repeat(inc), identifier, value),
        NodeType::VariableDeclaration { identifier, value } => format!("{}Variable assignment: {} = {}\n", "\t".to_string().repeat(inc), identifier, value),
        NodeType::Error(ty) => format!("{}Error: {} on line {}, column: {}", "\t".to_string().repeat(inc), ty, node.row, node.column),
        NodeType::Return { value } => {
            if let Some(value) = value { format!("{}Return {}\n", "\t".to_string().repeat(inc), value) }
            else { format!("{}Return\n", "\t".to_string().repeat(inc)) }
        },
        NodeType::If { condition, body } => {
            let mut out = format!("{}If: Condition: {}\n", "\t".to_string().repeat(inc), condition);
            for part in body {
                out.push_str(format!("{}", convert_node_to_string(&part, inc + 1)).as_str());
            }
            out
        },
        NodeType::IfElse { condition, body, else_body } => {
            let mut out = format!("{}If: Condition {}\n", "\t".to_string().repeat(inc), condition);
            for part in body {
                out.push_str(format!("{}", convert_node_to_string(&part, inc + 1)).as_str());
            }
            out.push_str(format!("{}Else:\n", "\t".to_string().repeat(inc)).as_str());
            for part in else_body {
                out.push_str(format!("{}", convert_node_to_string(&part, inc + 1)).as_str());
            }
            out
        },
        NodeType::While { condition, body } => {
            let mut out = format!("{}While: Condition: {}\n", "\t".to_string().repeat(inc), condition);
            for part in body {
                out.push_str(format!("{}", convert_node_to_string(&part, inc + 1)).as_str());
            }
            out
        },
        NodeType::For { variable, condition, loop_increment, body } => {

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
                out.push_str(format!("{}", convert_node_to_string(&part, inc + 1)).as_str());
            }
            out
        },
        NodeType::Function { identifier, arguments, body } => {
            let mut out = format!("{}Function: {}, Arguments: (", "\t".to_string().repeat(inc), identifier);
            for (i, argument) in arguments.iter().enumerate() {
                if i != arguments.len() - 1 { out.push_str(format!("{}, ", argument).as_str()); }
                else { out.push_str(format!("{})\n", argument).as_str()); }
            }
            for part in body {
                out.push_str(format!("{}", convert_node_to_string(&part, inc + 1)).as_str());
            }
            out
        },
        NodeType::Out { value } => format!("{}Out: {}\n", "\t".to_string().repeat(inc), value)
    }
}

impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", convert_node_to_string(self, 0))
    }
}