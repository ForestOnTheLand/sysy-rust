use crate::util::Error;
use koopa::ir::Value;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
pub enum Symbol {
    Const(i32),
    Var(Value),
}

pub struct SymbolTable {
    data: Vec<HashMap<String, Symbol>>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable { data: Vec::new() }
    }

    pub fn push(&mut self) {
        self.data.push(HashMap::new());
    }

    pub fn pop(&mut self) -> Result<(), Error> {
        self.data.pop().ok_or(Error::InternalError(
            "symbol table gets empty before it should be".to_string(),
        ))?;
        Ok(())
    }

    pub fn get_const(&self, ident: &String) -> Result<i32, Error> {
        for layer in self.data.iter().rev() {
            match layer.get(ident) {
                Some(Symbol::Const(value)) => {
                    return Ok(*value);
                }
                Some(Symbol::Var(_)) => {
                    return Err(Error::ParseError(format!(
                        "expected '{}' to be a constant, but found an variable",
                        ident
                    )));
                }
                None => {}
            }
        }
        Err(Error::ParseError(format!("identifier '{ident}' undefined")))
    }

    pub fn insert_const(&mut self, ident: &String, value: i32) -> Result<(), Error> {
        let data = self.data.last_mut().unwrap();
        match data.insert(ident.clone(), Symbol::Const(value)) {
            Some(_previous) => Err(Error::ParseError(format!(
                "identifier '{}' redefined",
                ident
            ))),
            None => Ok(()),
        }
    }

    pub fn get_var(&self, ident: &String) -> Result<Value, Error> {
        for layer in self.data.iter().rev() {
            match layer.get(ident) {
                Some(Symbol::Const(_)) => {
                    return Err(Error::ParseError(format!(
                        "expected '{}' to be an variable, but found a constant",
                        ident
                    )));
                }
                Some(Symbol::Var(value)) => {
                    return Ok(*value);
                }
                None => {}
            }
        }
        Err(Error::ParseError(format!("identifier '{ident}' undefined")))
    }

    pub fn insert_var(&mut self, ident: &String, value: Value) -> Result<(), Error> {
        let data = self.data.last_mut().unwrap();
        match data.insert(ident.clone(), Symbol::Var(value)) {
            Some(_previous) => Err(Error::ParseError(format!(
                "identifier '{}' redefined",
                ident
            ))),
            None => Ok(()),
        }
    }

    pub fn get_symbol(&self, ident: &String) -> Result<Symbol, Error> {
        for layer in self.data.iter().rev() {
            match layer.get(ident) {
                Some(symbol) => {
                    return Ok(*symbol);
                }
                None => {}
            }
        }
        Err(Error::ParseError(format!("identifier '{ident}' undefined")))
    }

    pub fn size(&self) -> usize {
        self.data.len()
    }
}
