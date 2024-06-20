//! Utils for generating KoopaIR.
//!
//! Namely, [`SymbolTable`] is defined here.

use koopa::ir::{BasicBlock, Function, Type, Value};
use std::collections::HashMap;

/// Symbol, either a constant or a variable
#[derive(Debug, Clone)]
pub enum Symbol {
    Const(i32),
    Var(Value, Type),
}

/// Symbol table, recording informations about values, scopes, functions.
/// - [`SymbolTable::data`] : a map from identifier to its corresponding symbol
/// - [`SymbolTable::loops`] : the start & end of each loop, in order to continue & break
/// - [`SymbolTable::function`] : a map from function name to its corresponding function
/// - [`SymbolTable::counter`] : an increasing id for blocks
#[derive(Debug)]
pub struct SymbolTable {
    data: Vec<HashMap<String, Symbol>>,
    loops: Vec<(BasicBlock, BasicBlock)>,
    function: HashMap<String, Function>,
    counter: usize,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            data: vec![HashMap::new()],
            loops: Vec::new(),
            function: HashMap::new(),
            counter: 0,
        }
    }

    /// Call me when you enter a scope!
    pub fn enter_block(&mut self) {
        self.data.push(HashMap::new());
    }

    /// Call me when you quit a scope!
    pub fn quit_block(&mut self) {
        self.data
            .pop()
            .expect("internal error: symbol table should not be empty now");
    }

    /// Search a constant `i32` value by its identifier.
    /// # Panic
    /// When the variable defined by the identifier cannot be evaluated at compile time,
    /// or the variable has not been defined.
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

    /// Insert a constant `i32` value.
    /// # Panic
    /// When the variable has been defined in this scope before.
    pub fn insert_const(&mut self, ident: String, value: i32) {
        let data = self.data.last_mut().unwrap();
        if let Some(_) = data.insert(ident.clone(), Symbol::Const(value)) {
            panic!("identifier '{ident}' redefined")
        }
    }

    /// Search a variable by its identifier.
    /// # Panic
    /// When the variable defined by the identifier is a constant,
    /// or the variable has not been defined.
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

    /// Insert a variable.
    /// # Panic
    /// When the variable has been defined in this scope before.
    pub fn insert_var(&mut self, ident: String, value: Value, ty: Type) {
        let data = self.data.last_mut().unwrap();
        if let Some(_) = data.insert(ident.clone(), Symbol::Var(value, ty)) {
            panic!("identifier '{ident}' redefined")
        }
    }

    /// Search a variable/constant by its identifier.
    /// # Panic
    /// When the variable has not been defined.
    pub fn get_symbol(&self, ident: &String) -> Symbol {
        for layer in self.data.iter().rev() {
            if let Some(symbol) = layer.get(ident) {
                return symbol.clone();
            }
        }
        panic!("identifier '{ident}' undefined");
    }

    /// Insert a function.
    /// # Panic
    /// When the function has been defined in this scope before.
    pub fn insert_function(&mut self, ident: String, function: Function) {
        if let Some(_) = self.function.insert(ident.clone(), function) {
            panic!("function '{ident}' redefined")
        }
    }

    /// Search a function by its identifier.
    /// # Panic
    /// When the function has not been defined.
    pub fn get_function(&self, ident: &String) -> Function {
        match self.function.get(ident) {
            Some(f) => f.clone(),
            None => panic!("function '{ident}' undefined"),
        }
    }

    /// Call me when you enter a `while` loop!
    pub fn enter_loop(&mut self, entry: BasicBlock, exit: BasicBlock) {
        self.loops.push((entry, exit));
    }

    /// Call me when you quit a `while` loop!
    pub fn quit_loop(&mut self) {
        self.loops
            .pop()
            .expect("internal error: loop table should not be empty now");
    }

    /// Get the entry of the current `while` loop (i.e. where you should go when `continue`)
    pub fn loop_entry(&self) -> BasicBlock {
        self.loops
            .last()
            .expect("invalid `continue` instruction found")
            .0
    }

    /// Get the exit of the current `while` loop (i.e. where you should go when `break`)
    pub fn loop_end(&self) -> BasicBlock {
        self.loops
            .last()
            .expect("invalid `break` instruction found")
            .1
    }

    /// Get a unique id for a variable name
    pub fn get_id(&mut self) -> usize {
        self.counter += 1;
        self.counter
    }
}
