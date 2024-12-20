#[derive(PartialEq, Eq, Clone)]
pub enum PunctuationKind {
    Open(usize),
    Close(usize),
    Seperator
}

#[derive(PartialEq, Eq, Clone)]
pub enum TokenType {
    Punctuation(PunctuationKind),
    Operator,
    Identifier,
    Number,
    EOF
}

#[derive(Clone)]
pub struct Token {
    pub ty: TokenType,
    pub raw: String,
    pub row: usize,
    pub column: usize
}

#[derive(PartialEq, Eq, Clone)]
pub enum LexerErrorType {
    UnevenPunctuation,
    UnevenFilePunctuation,
    InvalidNumber,
    UnknownCharachter
}

#[derive(Clone)]
pub struct LexerError {
    pub ty: LexerErrorType,
    row: usize,
    column: usize
}

pub struct Lexer<'a> {
    chars: std::iter::Peekable<std::str::Chars<'a>>,
    row: usize,
    column: usize,
    current_punctuation: [isize; 3] // [Parenthesis, Brackets, Curly Brackets]
}

impl<'a> Lexer<'a> {
    pub fn new(code: &'a str) -> Lexer<'a> {
        Lexer {
            chars: code.chars().peekable(),
            row: 1,
            column: 0,
            current_punctuation: [0; 3]
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            if let Some(c) = self.chars.peek() {
                if c.is_whitespace() {
                    self.column += 1;
                    if *c == '\n' {
                        self.row += 1;
                        self.column = 0;
                    }
                    self.chars.next();
                }
                else { return; }
            } 
            else { return; }
        }
    }

    fn skip_comment(&mut self) {
        loop {
            if let Some(c) = self.chars.peek() {
                if *c == '\n' { return; }
                self.chars.next();
            }
            else { return; }
        }
    }

    fn is_identifier(c: char) -> bool { 
        match c {
            'A'..='Z' | 'a'..='z' | '_' => true, 
            _ => false 
        }
    }

    fn is_punctuation(c: char)      -> bool { "(){}[],;".contains(c) }
    fn is_seperator(c: char)        -> bool { ",;".contains(c) }
    fn is_operator(c: char)         -> bool { "+=-*/><!&|^".contains(c) }

    fn update_punctuation(&mut self, c: char) -> isize {
        let ty = match c {
            '(' | ')' => 0,
            '[' | ']' => 1,
            '{' | '}' => 2,
            _ => panic!("Unreachable")
        };

        let ret;
        if "([{".contains(c) { 
            ret = self.current_punctuation[ty]; 
            self.current_punctuation[ty] += 1; 
        }
        else { 
            self.current_punctuation[ty] -= 1;
            ret = self.current_punctuation[ty];
        }

        return ret;
    }

    fn parse_identifier(&mut self) -> Token {
        let mut raw = String::new();
        loop {
            if let Some(c) = self.chars.peek() {
                if Lexer::is_identifier(*c) || (raw.len() > 0 && c.is_numeric()) {
                    raw.push(*c);
                    self.chars.next();
                    self.column += 1;
                }
                else { break; }
            }
            else { break; }
        }
        return Token {
            ty: TokenType::Identifier,
            raw: raw,
            row: self.row,
            column: self.column
        }
    }

    fn parse_number(&mut self) -> Result<Token, LexerError> {
        let mut raw = String::new();
        loop {
            if let Some(c) = self.chars.peek() {
                if c.is_numeric() {
                    raw.push(*c);
                    self.chars.next();
                    self.column += 1;
                }
                else if Lexer::is_identifier(*c) {
                    return Err(LexerError {
                        ty: LexerErrorType::InvalidNumber,
                        row: self.row,
                        column: self.column
                    })
               }
                else { break; }
            }
            else { break; }
        }
        return Ok(Token {
            ty: TokenType::Number,
            raw: raw,
            row: self.row,
            column: self.column
        })
    }

    fn parse_punctuation(&mut self) -> Result<Token, LexerError> {
        
        let c = self.chars.next().unwrap();
        self.column += 1;
        if Lexer::is_seperator(c) {
            return Ok(Token {
                ty: TokenType::Punctuation(PunctuationKind::Seperator),
                raw: c.to_string(),
                row: self.row,
                column: self.column
            })
        }

        let current_punctuation = self.update_punctuation(c);
        if current_punctuation < 0 {
            return Err(LexerError {
                ty: LexerErrorType::UnevenPunctuation,
                row: self.row,
                column: self.column
            });
        }

        let ty = match c {
            '(' | '[' | '{' => TokenType::Punctuation(PunctuationKind::Open(current_punctuation as usize)),
            ')' | ']' | '}' => TokenType::Punctuation(PunctuationKind::Close(current_punctuation as usize)),
            _ => panic!("Unreachable")
        };

        return Ok(Token {
            ty: ty,
            raw: c.to_string(),
            row: self.row,
            column: self.column
        });
    }

    fn parse_operator(&mut self) -> Option<Token> {
        let mut raw = self.chars.next().expect("Always exists").to_string();
        self.column += 1;
        if let Some(nxt) = self.chars.peek() {
            if Lexer::is_operator(*nxt) {
                raw.push(*nxt);
                self.chars.next();
                self.column += 1;
            }
        }

        if raw == "//" { 
            self.skip_comment();
            return None;
        }
        return Some(Token {
            ty: TokenType::Operator,
            raw: raw,
            row: self.row,
            column: self.column
        })
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {

        self.skip_whitespace();

        loop {
            if let Some(c) = self.chars.peek() {

                if Lexer::is_identifier(*c) { return Ok(self.parse_identifier()); }
                if c.is_numeric() { return self.parse_number(); }  
                if Lexer::is_punctuation(*c) { return self.parse_punctuation(); }
                if Lexer::is_operator(*c) {
                    if let Some(token) = self.parse_operator() { return Ok(token); }
                    else { self.skip_whitespace(); continue; }
                }

                self.chars.next();
                return Err(LexerError {
                    ty: LexerErrorType::UnknownCharachter,
                    row: self.row,
                    column: self.column
                })
            }
            else { break; }
        }

        if self.current_punctuation[0] != 0 || self.current_punctuation[1] != 0 || self.current_punctuation[2] != 0 {
            return Err(LexerError {
                ty: LexerErrorType::UnevenFilePunctuation,
                row: self.row,
                column: self.column
            })
        }

        return Ok(Token {
            ty: TokenType::EOF,
            raw: String::new(),
            row: self.row,
            column: self.column
        })
    }
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let out = match self {
            TokenType::EOF => "EOF".to_string(),
            TokenType::Identifier => "Identifier".to_string(),
            TokenType::Number => "Number".to_string(),
            TokenType::Operator => "Operator".to_string(),
            TokenType::Punctuation(PunctuationKind::Open(nmr)) => format!("Open({})", nmr),
            TokenType::Punctuation(PunctuationKind::Close(nmr)) => format!("Close({})", nmr),
            TokenType::Punctuation(PunctuationKind::Seperator) => "Seperator".to_string()
        };
        write!(f, "{}", out)
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Token ( Type: {}, Raw: {}, Row: {}, column: {} )", self.ty, self.raw, self.row, self.column)
    }
}

impl std::fmt::Display for LexerErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let out = match self {
            LexerErrorType::InvalidNumber => "Invalid number",
            LexerErrorType::UnevenPunctuation => "Uneven punctuation",
            LexerErrorType::UnknownCharachter => "Unknown charachter",
            LexerErrorType::UnevenFilePunctuation => "Uneven Punctuation in File"
        };
        write!(f, "{}", out)
    }
}

impl std::fmt::Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "LexerError ( Type: {}, Row: {}, column: {} )", self.ty, self.row, self.column)
    }
}