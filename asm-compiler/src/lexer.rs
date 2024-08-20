#[derive(Clone)]
pub struct Token {
    pub ty: TokenType,
    pub row: usize,
    pub column: usize
}

#[derive(Clone)]
pub enum TokenType {
    Add,
    Sub,
    And,
    Or,
    Not,
    Xor,
    Shl,
    Shr,
    Eq,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Sram,
    Lram,
    Push,
    Pop,
    Call,
    Return,
    Mov,
    Jt,
    Jf,
    Jeq,
    Jneq,
    Jl,
    Jg,
    Jge,
    Jle,
    Jmp,
    Halt,
    Out,
    Reg(u32),
    Label(String),
    Value(i32)
}

fn parse_value_or_label(word: &str) -> TokenType {
    if word.chars().nth(0).unwrap().is_digit(10) {
        let value: Result<i32, _> = word.parse(); 
        match value {
            Ok(value) => return TokenType::Value(value),
            Err(_) => panic!("Unknown keyword: {}", word)
        }
    }
    else {
        return TokenType::Label(word.to_string());
    }
    
}

pub fn tokenize(code: String) -> Vec<Token> {
    let mut tokens: Vec<Token> = vec![];

    let lines = code.lines();
    for (row, line) in lines.enumerate() {

        if line.ends_with(":") {
            tokens.push(Token {
                ty: TokenType::Label(line[..line.len() - 1].to_string()),
                row,
                column: 1
            });
            continue;
        }

        let words = line.split(' ');
        for (column, word) in words.enumerate() {
            if word.len() == 0 { continue; }
            let token_type = match word {
                "add" => TokenType::Add,
                "sub" => TokenType::Sub,
                "and" => TokenType::And,
                "or" => TokenType::Or,
                "not" => TokenType::Not,
                "xor" => TokenType::Xor,
                "shl" => TokenType::Shl,
                "shr" => TokenType::Shr,
                "eq" => TokenType::Eq,
                "neq" => TokenType::NotEq,
                "lt" => TokenType::Less,
                "lte" => TokenType::LessEq,
                "gt" => TokenType::Greater,
                "gte" => TokenType::GreaterEq,
                "sram" => TokenType::Sram,
                "lram" => TokenType::Lram,
                "push" => TokenType::Push,
                "pop" => TokenType::Pop,
                "call" => TokenType::Call,
                "ret" => TokenType::Return,
                "mov" => TokenType::Mov,
                "jt" => TokenType::Jt,
                "jf" => TokenType::Jf,
                "jeq" => TokenType::Jeq,
                "jne" => TokenType::Jneq,
                "jl" => TokenType::Jl,
                "jg" => TokenType::Jg,
                "jge" => TokenType::Jge,
                "jle" => TokenType::Jle,
                "hlt" => TokenType::Halt,
                "out" => TokenType::Out,
                "jmp" => TokenType::Jmp,
                "r0" | "r1" | "r2" | "r3" | "r4" | "r5" => TokenType::Reg(word.chars().last().unwrap() as u32 - '0' as u32),
                "io" => TokenType::Reg(7),
                _ => parse_value_or_label(word)
            };
            tokens.push(Token {
                ty: token_type,
                row: row + 1,
                column: column + 1
            });
        }
    }
    tokens
}