use crate::ast::Ast;
use crate::gen::ram::Ram;

pub struct Gen {
    ast: Ast,
    ram: Ram,
    asm: String
}

impl Gen {
    fn new(ast: Ast) -> Gen {
        Gen {
            ast,
            ram: Ram::new(),
            asm: String::new()
        }
    }

    fn generate_asm(&mut self) -> String {

        

        self.asm.clone()
    }
}