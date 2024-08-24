pub fn is_reserved_keyword(keyword: &String) -> bool {
    match keyword.as_str() {
        "for" | "fn" | "while" | "else" | "if" | "let" | "return" | "out" => return true,
        _ => return false
    }
}

pub fn is_int_operator(operator: &String) -> bool {
    match operator.as_str() {
        "+" | "-" | "*" | "&" | "|" | "^" | "<<" | ">>" => return true,
        _ => return false
    };
}