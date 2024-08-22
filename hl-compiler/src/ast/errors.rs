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