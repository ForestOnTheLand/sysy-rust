//! In this file, the conversion from AST to KoopaIR is provided.

use crate::ast::*;
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
    build_lor_exp(func, bb, exp.lor_exp.as_ref())
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

fn build_rel_exp(func: &mut FunctionData, bb: BasicBlock, exp: &RelExp) -> Result<Value, Error> {
    match exp {
        RelExp::Single(exp) => build_add_exp(func, bb, exp),
        RelExp::Binary(left, op, right) => {
            let left = build_rel_exp(func, bb, left)?;
            let right = build_add_exp(func, bb, right)?;
            let value = func.dfg_mut().new_value().binary(
                match op {
                    RelOp::Lt => BinaryOp::Lt,
                    RelOp::Le => BinaryOp::Le,
                    RelOp::Gt => BinaryOp::Gt,
                    RelOp::Ge => BinaryOp::Ge,
                },
                left,
                right,
            );
            new_inst!(func, bb, value);
            Ok(value)
        }
    }
}

fn build_eq_exp(func: &mut FunctionData, bb: BasicBlock, exp: &EqExp) -> Result<Value, Error> {
    match exp {
        EqExp::Single(exp) => build_rel_exp(func, bb, exp),
        EqExp::Binary(left, op, right) => {
            let left = build_eq_exp(func, bb, left)?;
            let right = build_rel_exp(func, bb, right)?;
            let value = func.dfg_mut().new_value().binary(
                match op {
                    EqOp::Eq => BinaryOp::Eq,
                    EqOp::Neq => BinaryOp::NotEq,
                },
                left,
                right,
            );
            new_inst!(func, bb, value);
            Ok(value)
        }
    }
}

/// `a && b ` is equivalent to `(a != 0) & (b != 0)`
fn build_land_exp(func: &mut FunctionData, bb: BasicBlock, exp: &LAndExp) -> Result<Value, Error> {
    match exp {
        LAndExp::Single(exp) => build_eq_exp(func, bb, exp),
        LAndExp::Binary(left, right) => {
            let zero = func.dfg_mut().new_value().integer(0);
            let left = build_land_exp(func, bb, left)?;
            let l = func
                .dfg_mut()
                .new_value()
                .binary(BinaryOp::NotEq, left, zero);
            new_inst!(func, bb, l);
            let right = build_eq_exp(func, bb, right)?;
            let r = func
                .dfg_mut()
                .new_value()
                .binary(BinaryOp::NotEq, right, zero);
            new_inst!(func, bb, r);
            let value = func.dfg_mut().new_value().binary(BinaryOp::And, l, r);
            new_inst!(func, bb, value);
            Ok(value)
        }
    }
}

/// `a || b ` is equivalent to `(a | b) != 0`
fn build_lor_exp(func: &mut FunctionData, bb: BasicBlock, exp: &LOrExp) -> Result<Value, Error> {
    match exp {
        LOrExp::Single(exp) => build_land_exp(func, bb, exp),
        LOrExp::Binary(left, right) => {
            let zero = func.dfg_mut().new_value().integer(0);
            let left = build_lor_exp(func, bb, left)?;
            let right = build_land_exp(func, bb, right)?;
            let or = func.dfg_mut().new_value().binary(BinaryOp::Or, left, right);
            new_inst!(func, bb, or);
            let value = func.dfg_mut().new_value().binary(BinaryOp::NotEq, or, zero);
            new_inst!(func, bb, value);
            Ok(value)
        }
    }
}
