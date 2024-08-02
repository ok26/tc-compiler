#[derive(Clone)]
pub enum Token {
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
    Halt,
    Reg(u32),
    Label(String),
    Value(i32)
}

fn parse_value_or_label(word: &str) -> Token {
    if word.chars().nth(0).unwrap().is_digit(10) {
        let value: Result<i32, _> = word.parse(); 
        match value {
            Ok(value) => return Token::Value(value),
            Err(_) => panic!("Unknown keyword: {}", word)
        }
    }
    else {
        return Token::Label(word.to_string());
    }
    
}

pub fn tokenize(code: String) -> Vec<Token> {
    let mut tokens: Vec<Token> = vec![];

    let lines = code.lines();
    for line in lines {

        if line.ends_with(":") {
            tokens.push(Token::Label(line[..line.len() - 1].to_string()));
            continue;
        }

        let words = line.split(' ');
        for word in words {
            if word.len() == 0 { continue; }
            tokens.push( match word {
                "add" => Token::Add,
                "sub" => Token::Sub,
                "and" => Token::And,
                "or" => Token::Or,
                "not" => Token::Not,
                "xor" => Token::Xor,
                "shl" => Token::Shl,
                "shr" => Token::Shr,
                "eq" => Token::Eq,
                "neq" => Token::NotEq,
                "lt" => Token::Less,
                "lte" => Token::LessEq,
                "gt" => Token::Greater,
                "gte" => Token::GreaterEq,
                "sram" => Token::Sram,
                "lram" => Token::Lram,
                "push" => Token::Push,
                "pop" => Token::Pop,
                "call" => Token::Call,
                "ret" => Token::Return,
                "mov" => Token::Mov,
                "jt" => Token::Jt,
                "jf" => Token::Jf,
                "jeq" => Token::Jeq,
                "jne" => Token::Jneq,
                "jl" => Token::Jl,
                "jg" => Token::Jge,
                "jge" => Token::Jge,
                "jle" => Token::Jle,
                "hlt" => Token::Halt,
                "r0" | "r1" | "r2" | "r3" | "r4" | "r5" => Token::Reg(word.chars().last().unwrap() as u32 - '0' as u32),
                "io" => Token::Reg(7),
                _ => parse_value_or_label(word)
            })
        }
    }
    tokens
}