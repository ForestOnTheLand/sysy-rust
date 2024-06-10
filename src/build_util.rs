//! Utils for generating KoopaIR.

use koopa::ir::{BasicBlock, Function, Type, Value};
use std::{
    collections::HashMap,
    sync::atomic::{AtomicUsize, Ordering},
};

// Symbol, either a constant or a variable
#[derive(Debug, Clone)]
pub enum Symbol {
    Const(i32),
    Var(Value, Type),
}

/// Symbol table, recording:
/// - [`SymbolTable::data`] : a map from identifier to its corresponding symbol
/// - [`SymbolTable::loops`] : the start & end of each loop, in order to continue & break
/// - [`SymbolTable::function`] : a map from function name to its corresponding function
/// - [`SymbolTable::counter`] : an increasing id for blocks
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
            counter: AtomicUsize::new(1),
        }
    }

    pub fn enter_block(&mut self) {
        self.data.push(HashMap::new());
    }

    pub fn quit_block(&mut self) {
        self.data
            .pop()
            .expect("internal error: symbol table should not be empty now");
    }

    pub fn get_const(&self, ident: &String) -> i32 {
        for layer in self.data.iter().rev() {
            match layer.get(ident) {
                Some(Symbol::Const(value)) => return value.clone(),
                Some(_) => panic!("cannot evaluate '{ident}' at compile time"),
                None => {}
            }
        }
        panic!("identifier '{ident}' undefined");
    }

    pub fn insert_const(&mut self, ident: String, value: i32) {
        let data = self.data.last_mut().unwrap();
        if let Some(_) = data.insert(ident.clone(), Symbol::Const(value)) {
            panic!("identifier '{ident}' redefined")
        }
    }

    pub fn get_var(&self, ident: &String) -> (Value, Type) {
        for layer in self.data.iter().rev() {
            match layer.get(ident) {
                Some(Symbol::Var(value, ty)) => return (value.clone(), ty.clone()),
                Some(_) => panic!("expected '{ident}' to be an variable"),
                None => {}
            }
        }
        panic!("identifier '{ident}' undefined")
    }

    pub fn insert_var(&mut self, ident: String, value: Value, ty: Type) {
        let data = self.data.last_mut().unwrap();
        if let Some(_) = data.insert(ident.clone(), Symbol::Var(value, ty)) {
            panic!("identifier '{ident}' redefined")
        }
    }

    pub fn get_symbol(&self, ident: &String) -> Symbol {
        for layer in self.data.iter().rev() {
            if let Some(symbol) = layer.get(ident) {
                return symbol.clone();
            }
        }
        panic!("identifier '{ident}' undefined");
    }

    pub fn insert_function(&mut self, ident: String, function: Function) {
        if let Some(_) = self.function.insert(ident.clone(), function) {
            panic!("function '{ident}' redefined")
        }
    }

    pub fn get_function(&self, ident: &String) -> Function {
        match self.function.get(ident) {
            Some(f) => f.clone(),
            None => panic!("function '{ident}' undefined"),
        }
    }

    pub fn enter_loop(&mut self, entry: BasicBlock, exit: BasicBlock) {
        self.loops.push((entry, exit));
    }

    pub fn quit_loop(&mut self) {
        self.loops
            .pop()
            .expect("internal error: loop table should not be empty now");
    }

    pub fn loop_entry(&self) -> BasicBlock {
        self.loops
            .last()
            .expect("invalid `continue` instruction found")
            .0
    }

    pub fn loop_end(&self) -> BasicBlock {
        self.loops
            .last()
            .expect("invalid `break` instruction found")
            .1
    }

    pub fn get_id(&self) -> usize {
        self.counter.fetch_add(1, Ordering::Relaxed)
    }
}
