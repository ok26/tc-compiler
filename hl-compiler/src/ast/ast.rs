use crate::{lexer::{PunctuationKind, Token, TokenType}, utils::is_reserved_keyword};

use super::{errors::AstErrorType, expressions::{parse_boolean_expression, parse_expression, Expression, ExpressionType}};

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
        offset: Option<Expression>,                      // Memory offset for lists
        value: Expression,
    },
    VariableDeclaration {
        identifier: String,
        offset: Option<Expression>,                      // Memory offset for lists
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
    Asm {
        asm_code: String            // Unsafe to use, does not check asm-code and immediately replaces the asm-call with the asm-code
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

        if is_reserved_keyword(&token.raw) {
            return Node {
                ty: NodeType::Error(AstErrorType::ReservedKeyword),
                row: token.row,
                column: token.column
            };
        }

        let identifier = token.raw.clone();
        let mut token = self.tokens[self.i].clone();
        self.i += 1;

        let mut offset: Option<Expression> = None;

        // List
        if token.raw == "[" {

            offset = match parse_expression(&self.tokens, &mut self.i, vec!["]"]) {
                Ok(value) => Some(value),
                Err(error) => return error
            };

            if is_declaration {
                let token = self.tokens[self.i].clone();
                self.i += 1;
                if token.raw != ";" {
                    return Node {
                        ty: NodeType::Error(AstErrorType::ExpectedEndOfLine),
                        row: token.row,
                        column: token.column
                    }
                }

                return Node {
                    ty: NodeType::VariableDeclaration {
                        identifier,
                        offset,
                        value: Expression {
                            ty: ExpressionType::Block(vec![]),
                            row: 0,
                            column: 0
                        }
                    },
                    row: 0,
                    column: 0
                }
            }

            token = self.tokens[self.i].clone();
            self.i += 1;
        }
        
        if token.raw != String::from("=") {
            return Node {
                ty: NodeType::Error(AstErrorType::ExpectedAssignment),
                row: token.row,
                column: token.column
            };
        }

        let value = match parse_expression(&self.tokens, &mut self.i, vec![terminator]) {
            Ok(expression) => expression,
            Err(error) => return error
        };

        if is_declaration {
            return Node {
                ty: NodeType::VariableDeclaration {
                    identifier,
                    offset,
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
                    offset,
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
        match parse_expression(&self.tokens, &mut self.i, vec![";"]) {
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

        match parse_boolean_expression(&self.tokens, &mut self.i, ")") {
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

        match parse_boolean_expression(&self.tokens, &mut self.i, ")") {
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
                    NodeType::Error(_) => return variable_declaration.unwrap(),
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
            match parse_boolean_expression(&self.tokens, &mut self.i, ";") {
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
                    NodeType::Error(_) => return loop_increment.unwrap(),
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

        let node = match parse_expression(&self.tokens, &mut self.i, vec![")"]) {
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

    fn parse_asm_keyword(&mut self) -> Node {
        let token = self.tokens[self.i].clone();
        self.i += 1;

        if let TokenType::Punctuation(PunctuationKind::Open(open_nmr)) = token.ty {
            if token.raw == "(" {
                
                let mut asm_code = String::new();
                loop {
                    let token = self.tokens[self.i].clone();
                    self.i += 1;

                    if let TokenType::Punctuation(PunctuationKind::Close(close_nmr)) = token.ty {
                        if token.raw == ")" && open_nmr == close_nmr {

                            let token = self.tokens[self.i].clone();
                            self.i += 1;
                            
                            if token.raw != ";" { return Node {
                                ty: NodeType::Error(AstErrorType::ExpectedEndOfLine),
                                row: token.row,
                                column: token.column
                            }}

                            asm_code.push('\n');
                            return Node {
                                ty: NodeType::Asm { asm_code },
                                row: 0,
                                column: 0
                            }
                        }
                    }

                    if token.raw == "," { asm_code.push('\n'); }
                    else { asm_code.push_str(format!("{} ", token.raw).as_str()); }
                }
            }
        }

        return Node {
            ty: NodeType::Error(AstErrorType::ExpectedParenthesis),
            row: token.row,
            column: token.column
        };
    }

    pub fn parse_line(&mut self) -> Option<Node> {
        
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
            "asm" => self.parse_asm_keyword(),
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
}

fn convert_node_to_string(node: &Node, inc: usize) -> String {
    return match &node.ty {
        NodeType::VariableAssignment { identifier, offset: _, value } => format!("{}Variable assignment: {} = {}\n", "\t".to_string().repeat(inc), identifier, value),
        NodeType::VariableDeclaration { identifier, offset: _, value } => format!("{}Variable assignment: {} = {}\n", "\t".to_string().repeat(inc), identifier, value),
        NodeType::Asm { asm_code } => format!("{}Asm-code: {}", "\t".to_string().repeat(inc), asm_code),
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