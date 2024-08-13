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
    Reg(u32),
    Label(String),
    Value(i32)
}

fn parse_value_or_label(word: &str, row: usize, column: usize) -> Token {
    if word.chars().nth(0).unwrap().is_digit(10) {
        let value: Result<i32, _> = word.parse(); 
        match value {
            Ok(value) => return Token { ty: TokenType::Value(value), row, column },
            Err(_) => panic!("Unknown keyword: {}", word)
        }
    }
    else {
        return Token { ty: TokenType::Label(word.to_string()), row, column };
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
            let mut token = match word {
                "add" => Token { ty: TokenType::Add, row, column },
                "sub" => Token { ty: TokenType::Sub, row, column },
                "and" => Token { ty: TokenType::And, row, column },
                "or" => Token { ty: TokenType::Or, row, column },
                "not" => Token { ty: TokenType::Not, row, column },
                "xor" => Token { ty: TokenType::Xor, row, column },
                "shl" => Token { ty: TokenType::Shl, row, column },
                "shr" => Token { ty: TokenType::Shr, row, column },
                "eq" => Token { ty: TokenType::Eq, row, column },
                "neq" => Token { ty: TokenType::NotEq, row, column },
                "lt" => Token { ty: TokenType::Less, row, column },
                "lte" => Token { ty: TokenType::LessEq, row, column },
                "gt" => Token { ty: TokenType::Greater, row, column },
                "gte" => Token { ty: TokenType::GreaterEq, row, column },
                "sram" => Token { ty: TokenType::Sram, row, column },
                "lram" => Token { ty: TokenType::Lram, row, column },
                "push" => Token { ty: TokenType::Push, row, column },
                "pop" => Token { ty: TokenType::Pop, row, column },
                "call" => Token { ty: TokenType::Call, row, column },
                "ret" => Token { ty: TokenType::Return, row, column },
                "mov" => Token { ty: TokenType::Mov, row, column },
                "jt" => Token { ty: TokenType::Jt, row, column },
                "jf" => Token { ty: TokenType::Jf, row, column },
                "jeq" => Token { ty: TokenType::Jeq, row, column },
                "jne" => Token { ty: TokenType::Jneq, row, column },
                "jl" => Token { ty: TokenType::Jl, row, column },
                "jg" => Token { ty: TokenType::Jg, row, column },
                "jge" => Token { ty: TokenType::Jge, row, column },
                "jle" => Token { ty: TokenType::Jle, row, column },
                "hlt" => Token { ty: TokenType::Halt, row, column },
                "jmp" => Token { ty: TokenType::Jmp, row, column },
                "r0" | "r1" | "r2" | "r3" | "r4" | "r5" => Token { ty: TokenType::Reg(word.chars().last().unwrap() as u32 - '0' as u32), row, column },
                "io" => Token { ty: TokenType::Reg(7), row, column },
                _ => parse_value_or_label(word, row, column)
            };
            token.row += 1;
            token.column += 1;
            tokens.push(token);
        }
    }
    tokens
}