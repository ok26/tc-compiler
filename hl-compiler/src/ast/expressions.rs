use std::cmp::max;

use crate::{lexer::{Token, TokenType}, utils::is_int_operator};

use super::{ast::{Node, NodeType}, errors::AstErrorType};

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

fn parse_called_function_arguments(tokens: &Vec<Token>, i: &mut usize) -> Result<Vec<Expression>, Node> {
    let mut arguments = vec![];
    if tokens[*i].raw == ")" {
        *i += 1;
        return Ok(arguments);
    }
    loop {
        match parse_expression(tokens, i, vec![")", ","]) {
            Ok(expression) => {
                arguments.push(expression);
                if tokens[*i - 1].raw == ")" { return Ok(arguments); }
            },
            Err(error) => return Err(error)
        };
    }
}
pub fn parse_boolean_expression(tokens: &Vec<Token>, i: &mut usize, terminator: &str) -> Result<Expression, Node> {
    match parse_expression(tokens, i, vec!["<", ">", "<=", ">=", "==", "!="]) {
        Ok(left_expression) => {
            let boolean_operator = tokens[*i - 1].clone();
            let boolean_operator_exp = Expression {
                ty: ExpressionType::Operator(boolean_operator.raw),
                row: boolean_operator.row,
                column: boolean_operator.column
            };
            match parse_expression(tokens, i, vec![terminator]) {
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

// Only multiplication so far
fn compress_mul_blocks(expression_block: Vec<Expression>) -> Vec<Expression> {
    let mut new_block: Vec<Expression> = vec![];
    let mut i = 0;
    while i < expression_block.len() {
        
        let expression = &expression_block[i];
        match &expression.ty {
            ExpressionType::Operator(raw) => {

                if raw != "*" {
                    new_block.push(expression.clone());
                    i += 1;
                    continue;
                }
    
                new_block.pop();
                i -= 1;
    
                // Compress expression block
                let mut compressed_block: Vec<Expression> = vec![];
                loop {
                    if i == expression_block.len() {
                        new_block.push(Expression {
                            ty: ExpressionType::Block(compressed_block),
                            row: 0,
                            column: 0
                        });
                        break;
                    }
    
                    let expression = &expression_block[i];
                    if let ExpressionType::Operator(raw) = &expression.ty { if raw != "*" { 
                        new_block.push(Expression {
                            ty: ExpressionType::Block(compressed_block),
                            row: 0,
                            column: 0
                        });
                        new_block.push(expression.clone()); 
                        break;
                    }}
                    compressed_block.push(expression.clone());
                    i += 1;
                }
            },
            ExpressionType::Block(block) => {
                new_block.push(Expression {
                    ty: ExpressionType::Block(compress_mul_blocks(block.clone())),
                    row: 0,
                    column: 0
                });
            },
            _ => new_block.push(expression.clone())
        }
        i += 1;
    }
    new_block
}

pub fn parse_expression(tokens: &Vec<Token>, i: &mut usize, terminators: Vec<&str>) -> Result<Expression, Node> {
    let mut expression_block: Vec<Expression> = vec![];
    let mut parenthesis_number = 0;
    let mut max_parenthesis_number = 0;

    loop {
        let value = tokens[*i].clone();
        *i += 1;
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

        let mut operator = tokens[*i].clone();
        *i += 1;

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

            match parse_called_function_arguments(tokens, i) {
                Ok(arguments) => {
                    expression_block.push(Expression {
                        ty: ExpressionType::Function(value.raw, arguments),
                        row: value.row,
                        column: value.column
                    });
                    operator = tokens[*i].clone();
                    *i += 1;
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
            operator = tokens[*i].clone();
            *i += 1;
        }

        if terminators.contains(&operator.raw.as_str()) && (operator.raw != ")" || parenthesis_number == 0) { break; }

        if operator.ty != TokenType::Operator {
            return Err(Node {
                ty: NodeType::Error(AstErrorType::ExpectedOperator),
                row: operator.row,
                column: operator.column
            });
        }

        if !is_int_operator(&operator.raw) {
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
            row: tokens[*i - 1].row,
            column: tokens[*i - 1].column
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
                    i += 1;
                    continue;
                }

                // Compress expression block
                let mut compressed_block: Vec<Expression> = vec![];
                i += 1;
                loop {
                    let expression = &expression_block[i];
                    if let ExpressionType::ParenthesisClose(number) = expression.ty { if number == parenthesis_number { break; } }
                    compressed_block.push(expression.clone());
                    i += 1;
                }
                new_block.push(Expression {
                    ty: ExpressionType::Block(compressed_block),
                    row: 0,
                    column: 0
                });
            }
            else { new_block.push(expression.clone()); }

            i += 1;
        }
        expression_block = new_block;

        if parenthesis_number == 0 { break; }
        parenthesis_number -= 1;
    }

    Ok(Expression {
        ty: ExpressionType::Block(compress_mul_blocks(expression_block)),
        row: 0,
        column: 0
    })
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