//! Symbol table for KoopaIR.

use crate::util::Error;
use koopa::ir::{BasicBlock, Function, Type, Value};
use std::{
    collections::HashMap,
    sync::atomic::{AtomicUsize, Ordering},
};

#[derive(Debug, Clone)]
pub enum Symbol {
    Const(i32),
    Var(Value, Type),
}

/// Symbol table, supporting nested blocks
#[derive(Debug)]
pub struct SymbolTable {
    data: Vec<HashMap<String, Symbol>>,
    loops: Vec<(BasicBlock, BasicBlock)>,
    function: HashMap<String, Function>,
    counter: AtomicUsize,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            data: vec![HashMap::new()],
            loops: Vec::new(),
            function: HashMap::new(),
            counter: AtomicUsize::new(1000),
        }
    }

    pub fn enter_block(&mut self) {
        self.data.push(HashMap::new());
    }

    pub fn quit_block(&mut self) -> Result<(), Error> {
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
                Some(_) => {
                    return Err(Error::ParseError(format!(
                        "expected '{ident}' to be a constant"
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

    pub fn get_var(&self, ident: &String) -> Result<(Value, Type), Error> {
        for layer in self.data.iter().rev() {
            match layer.get(ident) {
                Some(Symbol::Var(value, ty)) => {
                    return Ok((value.clone(), ty.clone()));
                }
                Some(_) => {
                    return Err(Error::ParseError(format!(
                        "expected '{ident}' to be an variable"
                    )));
                }
                None => {}
            }
        }
        Err(Error::ParseError(format!("identifier '{ident}' undefined")))
    }

    pub fn insert_var(&mut self, ident: &String, value: Value, ty: Type) -> Result<(), Error> {
        let data = self.data.last_mut().unwrap();
        match data.insert(ident.clone(), Symbol::Var(value, ty)) {
            Some(_) => Err(Error::ParseError(format!("identifier '{ident}' redefined"))),
            None => Ok(()),
        }
    }

    pub fn get_symbol(&self, ident: &String) -> Result<Symbol, Error> {
        for layer in self.data.iter().rev() {
            match layer.get(ident) {
                Some(symbol) => {
                    return Ok(symbol.clone());
                }
                None => {}
            }
        }
        Err(Error::ParseError(format!("identifier '{ident}' undefined")))
    }

    pub fn insert_function(&mut self, ident: String, function: Function) -> Result<(), Error> {
        if let Some(_) = self.function.insert(ident.clone(), function) {
            Err(Error::ParseError(format!("function '{ident}' redefined")))
        } else {
            Ok(())
        }
    }

    pub fn get_function(&self, ident: &String) -> Result<Function, Error> {
        if let Some(f) = self.function.get(ident) {
            Ok(*f)
        } else {
            Err(Error::ParseError(format!("function '{ident}' undefined")))
        }
    }

    pub fn size(&self) -> usize {
        self.data.len()
    }

    pub fn enter_loop(&mut self, entry: BasicBlock, end: BasicBlock) {
        self.loops.push((entry, end));
    }

    pub fn quit_loop(&mut self) -> Result<(), Error> {
        self.loops.pop().ok_or(Error::InternalError(
            "loop table gets empty before it should be".to_string(),
        ))?;
        Ok(())
    }

    pub fn loop_entry(&self) -> Result<BasicBlock, Error> {
        Ok(self
            .loops
            .last()
            .ok_or(Error::ParseError(
                "invalid `continue` instruction found".to_string(),
            ))?
            .0)
    }

    pub fn loop_end(&self) -> Result<BasicBlock, Error> {
        Ok(self
            .loops
            .last()
            .ok_or(Error::ParseError(
                "invalid `break` instruction found".to_string(),
            ))?
            .1)
    }

    pub fn get_id(&self) -> usize {
        self.counter.fetch_add(1, Ordering::Relaxed)
    }
}
