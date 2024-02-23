//! In this file, the transformation from AST to KoopaIR is provided.

use crate::ast::{Block, CompUnit, FuncDef, FuncType};
use koopa::back::KoopaGenerator;
use koopa::ir::builder_traits::*;
use koopa::ir::{FunctionData, Program, Type};

// use std::error::Error;
use std::io;

#[derive(Debug)]
pub enum Error {
    NotImplementedError,
}

pub fn output_program(program: &Program, output: impl io::Write) {
    KoopaGenerator::new(output).generate_on(program).unwrap();
}

pub fn build_program(comp_unit: CompUnit) -> Result<Program, Error> {
    let mut program = Program::new();
    build_function(&mut program, comp_unit.func_def)?;
    Ok(program)
}

fn build_function(program: &mut Program, func_def: FuncDef) -> Result<(), Error> {
    let func = FunctionData::new(
        format!("@{}", func_def.ident),
        Vec::new(),
        match func_def.func_type {
            FuncType::Int => Type::get_i32(),
            _ => {
                return Err(Error::NotImplementedError);
            }
        },
    );
    let func = program.new_func(func);
    build_block(program.func_mut(func), func_def.block)?;

    Ok(())
}

fn build_block(func: &mut FunctionData, block: Block) -> Result<(), Error> {
    let stmt = block.stmt;
    let entry = func.dfg_mut().new_bb().basic_block(Some("%entry".into()));
    func.layout_mut().bbs_mut().push_key_back(entry).unwrap();

    let ret_val = func.dfg_mut().new_value().integer(stmt.num);
    let ret = func.dfg_mut().new_value().ret(Some(ret_val));
    func.layout_mut()
        .bb_mut(entry)
        .insts_mut()
        .push_key_back(ret)
        .unwrap();

    Ok(())
}
