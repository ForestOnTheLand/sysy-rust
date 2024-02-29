use crate::util::Error;
use koopa::ir::Value;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
pub enum Symbol {
    Const(i32),
    Var(Value),
}

pub struct SymbolTable {
    data: HashMap<String, Symbol>,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            data: HashMap::new(),
        }
    }

    pub fn get_const(&self, ident: &String) -> Result<i32, Error> {
        match self.data.get(ident) {
            Some(Symbol::Const(val)) => Ok(*val),
            Some(Symbol::Var(_)) => Err(Error::ParseError(format!(
                "expected '{}' to be a constant, but found an variable",
                ident
            ))),
            None => Err(Error::ParseError(format!(
                "identifier '{}' undefined",
                ident
            ))),
        }
    }

    pub fn insert_const(&mut self, ident: &String, value: i32) -> Result<(), Error> {
        match self.data.insert(ident.clone(), Symbol::Const(value)) {
            Some(_previous) => Err(Error::ParseError(format!(
                "identifier '{}' redefined",
                ident
            ))),
            None => Ok(()),
        }
    }

    pub fn get_var(&self, ident: &String) -> Result<Value, Error> {
        match self.data.get(ident) {
            Some(Symbol::Const(_)) => Err(Error::ParseError(format!(
                "expected '{}' to be an variable, but found a constant",
                ident
            ))),
            Some(Symbol::Var(value)) => Ok(*value),
            None => Err(Error::ParseError(format!(
                "identifier '{}' undefined",
                ident
            ))),
        }
    }

    pub fn insert_var(&mut self, ident: &String, value: Value) -> Result<(), Error> {
        match self.data.insert(ident.clone(), Symbol::Var(value)) {
            Some(_previous) => Err(Error::ParseError(format!(
                "identifier '{}' redefined",
                ident
            ))),
            None => Ok(()),
        }
    }

    pub fn get_symbol(&self, ident: &String) -> Result<Symbol, Error> {
        if let Some(symbol) = self.data.get(ident) {
            Ok(*symbol)
        } else {
            Err(Error::ParseError(format!(
                "identifier '{}' undefined",
                ident
            )))
        }
    }
}
