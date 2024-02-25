//! In this file, the conversion from AST to KoopaIR is provided.

use crate::ast::{
    AddExp, AddOp, Block, CompUnit, Exp, FuncDef, FuncType, MulExp, MulOp, PrimaryExp, UnaryExp,
    UnaryOp,
};
use crate::util::Error;
use koopa::back::KoopaGenerator;
use koopa::ir::builder_traits::*;
use koopa::ir::{BasicBlock, BinaryOp, FunctionData, Program, Type, Value};

// use std::error::Error;
use std::io;

/// Add an instruction into a function.
/// To avoid a huge amount of meaningless repeated code, I have to use macros.
macro_rules! new_inst {
    ($func:expr, $bb:expr, $inst:expr) => {
        $func
            .layout_mut()
            .bb_mut($bb)
            .insts_mut()
            .push_key_back($inst)
            .unwrap();
    };
}

pub fn output_program(program: &Program, output: impl io::Write) {
    KoopaGenerator::new(output).generate_on(program).unwrap();
}

pub fn build_program(comp_unit: &CompUnit) -> Result<Program, Error> {
    let mut program = Program::new();
    build_function(&mut program, &comp_unit.func_def)?;
    Ok(program)
}

fn build_function(program: &mut Program, func_def: &FuncDef) -> Result<(), Error> {
    let func = FunctionData::new(
        format!("@{}", func_def.ident),
        Vec::new(),
        match func_def.func_type {
            FuncType::Int => Type::get_i32(),
        },
    );
    let func = program.new_func(func);
    build_block(program.func_mut(func), &func_def.block)?;

    Ok(())
}

fn build_block(func: &mut FunctionData, block: &Block) -> Result<(), Error> {
    let stmt = &block.stmt;

    let entry = func.dfg_mut().new_bb().basic_block(Some("%entry".into()));
    func.layout_mut().bbs_mut().push_key_back(entry).unwrap();

    let ret_val = build_exp(func, entry, stmt.exp.as_ref())?;
    let ret = func.dfg_mut().new_value().ret(Some(ret_val));
    new_inst!(func, entry, ret);

    Ok(())
}

fn build_exp(func: &mut FunctionData, bb: BasicBlock, exp: &Exp) -> Result<Value, Error> {
    build_add_exp(func, bb, exp.add_exp.as_ref())
}

fn build_unary_exp(
    func: &mut FunctionData,
    bb: BasicBlock,
    exp: &UnaryExp,
) -> Result<Value, Error> {
    match exp {
        UnaryExp::Single(exp) => build_primary_exp(func, bb, exp),
        UnaryExp::Unary(unary_op, exp) => {
            let value = build_unary_exp(func, bb, exp)?;
            match unary_op {
                UnaryOp::Pos => Ok(value),
                UnaryOp::Neg => {
                    let zero = func.dfg_mut().new_value().integer(0);
                    let neg = func
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Sub, zero, value);
                    new_inst!(func, bb, neg);
                    Ok(neg)
                }
                UnaryOp::Not => {
                    let zero = func.dfg_mut().new_value().integer(0);
                    let not = func.dfg_mut().new_value().binary(BinaryOp::Eq, value, zero);
                    new_inst!(func, bb, not);
                    Ok(not)
                }
            }
        }
    }
}

fn build_primary_exp(
    func: &mut FunctionData,
    bb: BasicBlock,
    exp: &PrimaryExp,
) -> Result<Value, Error> {
    match exp {
        PrimaryExp::Expression(exp) => build_exp(func, bb, exp.as_ref()),
        PrimaryExp::Number(num) => Ok(func.dfg_mut().new_value().integer(*num)),
    }
}

fn build_mul_exp(func: &mut FunctionData, bb: BasicBlock, exp: &MulExp) -> Result<Value, Error> {
    match exp {
        MulExp::Single(exp) => build_unary_exp(func, bb, exp),
        MulExp::Binary(left, op, right) => {
            let left = build_mul_exp(func, bb, left)?;
            let right = build_unary_exp(func, bb, right)?;
            let value = func.dfg_mut().new_value().binary(
                match op {
                    MulOp::Mul => BinaryOp::Mul,
                    MulOp::Div => BinaryOp::Div,
                    MulOp::Mod => BinaryOp::Mod,
                },
                left,
                right,
            );
            new_inst!(func, bb, value);
            Ok(value)
        }
    }
}

fn build_add_exp(func: &mut FunctionData, bb: BasicBlock, exp: &AddExp) -> Result<Value, Error> {
    match exp {
        AddExp::Single(exp) => build_mul_exp(func, bb, exp),
        AddExp::Binary(left, op, right) => {
            let left = build_add_exp(func, bb, left)?;
            let right = build_mul_exp(func, bb, right)?;
            let value = func.dfg_mut().new_value().binary(
                match op {
                    AddOp::Add => BinaryOp::Add,
                    AddOp::Sub => BinaryOp::Sub,
                },
                left,
                right,
            );
            new_inst!(func, bb, value);
            Ok(value)
        }
    }
}
