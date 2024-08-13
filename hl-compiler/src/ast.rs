use crate::lexer::{PunctuationKind, Token, TokenType};

#[derive(Clone, Debug)]
pub enum Expression {
    Operation {
        left: Box<Expression>,
        right: Box<Expression>,
        raw: String
    },
    Value(String),
    Function(String, Vec<String>)
}

#[derive(Clone, Debug)]
pub enum AstErrorType {
    ExpectedIdentifier,
    ReservedKeyword,
    ExpectedAssignment,
    MissingValue,
    InvalidFunctionArguments,
    ExpectedOperator,
    ExpectedBody,
    ExpectedParentheses,
    ExpectedIntegerOperator,
    SyntaxError
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
        condition: Option<Expression>,
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
    Error {
        ty: AstErrorType,
        row: usize,
        collumn: usize
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
                collumn: token.collumn
            };
        }

        if Ast::is_reserved_keyword(&token.raw) {
            return Node::Error {
                ty: AstErrorType::ReservedKeyword,
                row: token.row,
                collumn: token.collumn
            };
        }

        let identifier = token.raw.clone();
        let token = &self.tokens[self.i];
        self.i += 1;
        
        if token.raw != String::from("=") {
            return Node::Error {
                ty: AstErrorType::ExpectedAssignment,
                row: token.row,
                collumn: token.collumn
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

    fn parse_expression(&mut self, terminators: Vec<&str>) -> Result<Expression, Node> {
        let token = self.tokens[self.i].clone();
        self.i += 1;
        let mut operator = self.tokens[self.i].clone();
        self.i += 1;

        match token.ty {
            TokenType::Number | TokenType::Identifier => {},
            _ => return Err(Node::Error {
                ty: AstErrorType::MissingValue,
                row: token.row,
                collumn: token.collumn
            }) 
        }

        if Ast::is_reserved_keyword(&token.raw) {
            return Err(Node::Error {
                ty: AstErrorType::ReservedKeyword,
                row: token.row,
                collumn: token.collumn
            })
        }

        let left_value;
        if operator.raw == "(" {
            if let Some(arguments) = self.parse_function_arguments() {
                left_value = Expression::Function(token.raw.clone(), arguments);
                operator = self.tokens[self.i].clone();
                self.i += 1;
            }
            else {
                return Err(Node::Error {
                    ty: AstErrorType::InvalidFunctionArguments,
                    row: operator.row,
                    collumn: operator.collumn
                })
            }
        }
        else { left_value = Expression::Value(token.raw.clone()); }

        if terminators.contains(&operator.raw.as_str()) {
            return Ok(left_value);
        }

        if operator.ty != TokenType::Operator {
            return Err(Node::Error {
                ty: AstErrorType::ExpectedOperator,
                row: operator.row,
                collumn: operator.collumn
            })
        }

        if !Ast::is_int_operator(&operator.raw) {
            return Err(Node::Error {
                ty: AstErrorType::ExpectedIntegerOperator,
                row: operator.row,
                collumn: operator.collumn
            })
        }

        match self.parse_expression(terminators) {
            Ok(expression) => return Ok(Expression::Operation {
                left: Box::new(left_value),
                right: Box::new(expression),
                raw: operator.raw.clone()
            }),
            Err(error) => return Err(error)
        }
    }

    fn parse_boolean_expression(&mut self, terminator: &str) -> Result<Expression, Node> {
        match self.parse_expression(vec!["<", ">", "<=", ">=", "==", "!="]) {
            Ok(left_expression) => {
                let boolean_operator = self.tokens[self.i - 1].clone();
                match self.parse_expression(vec![terminator]) {
                    Ok(right_expression) => return Ok(Expression::Operation {
                        left: Box::new(left_expression),
                        right: Box::new(right_expression),
                        raw: boolean_operator.raw
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
            return Node::Error {
                ty: AstErrorType::ExpectedIdentifier,
                row: token.row,
                collumn: token.collumn
            }
        }

        let seperator = self.tokens[self.i].clone();
        self.i += 1;
        if seperator.raw != "(" {
            return Node::Error {
                ty: AstErrorType::InvalidFunctionArguments,
                row: seperator.row,
                collumn: seperator.collumn
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
                collumn: body_open.collumn
            }
        }
        else {
            return Node::Error {
                ty: AstErrorType::InvalidFunctionArguments,
                row: seperator.row,
                collumn: seperator.collumn
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
                ty: AstErrorType::ExpectedParentheses,
                row: token.row,
                collumn: token.collumn
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
                                collumn: body_open.collumn
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
                    collumn: body_open.collumn
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
                ty: AstErrorType::ExpectedParentheses,
                row: token.row,
                collumn: token.collumn
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
                    collumn: body_open.collumn
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
                ty: AstErrorType::ExpectedParentheses,
                row: token.row,
                collumn: token.collumn
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
                collumn: token.collumn
            }
        }

        let token = self.tokens[self.i].clone();
        let expression: Option<Expression>;

        if token.raw == ";" { 
            expression = None;
            self.i += 1; 
        }
        else {
            match self.parse_boolean_expression(";") {
                Ok(boolean_expression) => { expression = Some(boolean_expression); },
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
            collumn: body_open.collumn
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
                collumn: token.collumn
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
        Expression::Value(value) => value.clone(),
        Expression::Function(identifier, arguments) => {

            let mut out = format!("{}(", identifier);
            for (i, argument) in arguments.iter().enumerate() {

                if i != arguments.len() - 1 { out.push_str(format!("{}, ", argument).as_str()); }
                else { out.push_str(format!("{})", argument).as_str()); }
            }
            out
        },
        Expression::Operation { left, right, raw } => format!("{} {} {}", left, raw, right)
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
            AstErrorType::ExpectedParentheses => "Expected Parentheses",
            AstErrorType::InvalidFunctionArguments => "Invalid Function Arguments",
            AstErrorType::MissingValue => "Missing Value",
            AstErrorType::ReservedKeyword => "Reserved Keyword",
            AstErrorType::SyntaxError => "Syntax Error"
        };
        write!(f, "{}", out)
    }
}

fn convert_node_to_string(node: &Node, inc: usize) -> String {
    return match node {
        Node::VariableAssignment { identifier, value } => format!("{}Variable assignment: {} = {}\n", "\t".to_string().repeat(inc), identifier, value),
        Node::Error { ty, row, collumn } => format!("{}Error: {} on line {}, collumn: {}\n", "\t".to_string().repeat(inc), ty, row, collumn),
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
            let condition_out = match &*condition {
                Some(condition) => format!("{}", condition),
                None => String::new()
            };
            let mut loop_increment_out = match &**loop_increment {
                Some(loop_increment) => format!("{}", loop_increment),
                None => String::new()
            };
            loop_increment_out.pop();
            let mut out = format!("{}For: {}, Condition: {}, Loop increment: {}\n", "\t".to_string().repeat(inc), variable_out, condition_out, loop_increment_out);
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
        }
    }
}

impl std::fmt::Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", convert_node_to_string(self, 0))
    }
}