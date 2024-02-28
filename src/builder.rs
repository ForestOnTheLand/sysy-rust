//! In this file, the conversion from AST to KoopaIR is provided.

use crate::ast::*;
use crate::symtab::{Symbol, SymbolTable};
use crate::util::Error;
use koopa::back::KoopaGenerator;
use koopa::ir::builder_traits::*;
use koopa::ir::{BasicBlock, BinaryOp, FunctionData, Program, Type, Value};

use std::collections::HashMap;
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
            .unwrap()
    };
}

macro_rules! new_value {
    ($func:expr) => {
        $func.dfg_mut().new_value()
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
    let entry = func.dfg_mut().new_bb().basic_block(Some("%entry".into()));
    func.layout_mut().bbs_mut().push_key_back(entry).unwrap();

    let mut symtab = SymbolTable::new();

    for block_item in block.block_items.iter() {
        build_block_item(func, entry, block_item, &mut symtab)?;
    }

    Ok(())
}

fn build_block_item(
    func: &mut FunctionData,
    bb: BasicBlock,
    item: &BlockItem,
    symtab: &mut SymbolTable,
) -> Result<(), Error> {
    match item {
        BlockItem::Decl(decl) => build_decl(func, bb, &decl, symtab),
        BlockItem::Stmt(stmt) => build_stmt(func, bb, &stmt, symtab),
    }
}

fn build_decl(
    func: &mut FunctionData,
    bb: BasicBlock,
    decl: &Decl,
    symtab: &mut SymbolTable,
) -> Result<(), Error> {
    match decl {
        Decl::Const(decl) => build_const_decl(decl, symtab),
        Decl::Var(decl) => build_var_decl(func, bb, decl, symtab),
    }
}

fn build_var_decl(
    func: &mut FunctionData,
    bb: BasicBlock,
    decl: &VarDecl,
    symtab: &mut SymbolTable,
) -> Result<(), Error> {
    for def in decl.var_defs.iter() {
        build_var_def(func, bb, def, symtab)?;
    }
    Ok(())
}

fn build_var_def(
    func: &mut FunctionData,
    bb: BasicBlock,
    def: &VarDef,
    symtab: &mut SymbolTable,
) -> Result<(), Error> {
    let var = new_value!(func).alloc(Type::get_i32());
    new_inst!(func, bb, var);
    if let Some(init_value) = &def.init_val {
        let value = build_init_value(func, bb, &init_value, symtab)?;
        let store = new_value!(func).store(value, var);
        new_inst!(func, bb, store);
    }
    symtab.insert_var(&def.ident, var)?;
    Ok(())
}

fn build_init_value(
    func: &mut FunctionData,
    bb: BasicBlock,
    val: &InitVal,
    symtab: &mut SymbolTable,
) -> Result<Value, Error> {
    build_exp(func, bb, &val.exp, symtab)
}

fn build_const_decl(decl: &ConstDecl, symtab: &mut SymbolTable) -> Result<(), Error> {
    for def in decl.const_defs.iter() {
        build_const_def(def, symtab)?;
    }
    Ok(())
}

fn build_const_def(decl: &ConstDef, symtab: &mut SymbolTable) -> Result<(), Error> {
    let value = compute_init_value(&decl.const_init_val, symtab)?;
    symtab.insert_const(&decl.ident, value)?;
    Ok(())
}

fn compute_init_value(init: &ConstInitVal, symtab: &SymbolTable) -> Result<i32, Error> {
    compute_exp(&init.const_exp.exp, symtab)
}

fn compute_exp(exp: &Exp, symtab: &SymbolTable) -> Result<i32, Error> {
    compute_lor_exp(&exp.lor_exp, symtab)
}

fn compute_unary_exp(exp: &UnaryExp, symtab: &SymbolTable) -> Result<i32, Error> {
    match exp {
        UnaryExp::Single(exp) => compute_primary_exp(exp, symtab),
        UnaryExp::Unary(op, exp) => {
            let val = compute_unary_exp(exp, symtab)?;
            Ok(match op {
                UnaryOp::Pos => val,
                UnaryOp::Neg => -val,
                UnaryOp::Not => !val,
            })
        }
    }
}

fn compute_primary_exp(exp: &PrimaryExp, symtab: &SymbolTable) -> Result<i32, Error> {
    match exp {
        PrimaryExp::Expression(exp) => compute_exp(exp, symtab),
        PrimaryExp::Number(val) => Ok(*val),
        PrimaryExp::LVal(lval) => symtab.get_const(&lval.ident),
    }
}

fn compute_mul_exp(exp: &MulExp, symtab: &SymbolTable) -> Result<i32, Error> {
    match exp {
        MulExp::Single(exp) => compute_unary_exp(exp, symtab),
        MulExp::Binary(left, op, right) => {
            let left = compute_mul_exp(left, symtab)?;
            let right = compute_unary_exp(right, symtab)?;
            Ok(match op {
                MulOp::Mul => left * right,
                MulOp::Div => left / right,
                MulOp::Mod => left % right,
            })
        }
    }
}

fn compute_add_exp(exp: &AddExp, symtab: &SymbolTable) -> Result<i32, Error> {
    match exp {
        AddExp::Single(exp) => compute_mul_exp(exp, symtab),
        AddExp::Binary(left, op, right) => {
            let left = compute_add_exp(left, symtab)?;
            let right = compute_mul_exp(right, symtab)?;
            Ok(match op {
                AddOp::Add => left + right,
                AddOp::Sub => left - right,
            })
        }
    }
}

fn compute_rel_exp(exp: &RelExp, symtab: &SymbolTable) -> Result<i32, Error> {
    match exp {
        RelExp::Single(exp) => compute_add_exp(exp, symtab),
        RelExp::Binary(left, op, right) => {
            let left = compute_rel_exp(left, symtab)?;
            let right = compute_add_exp(right, symtab)?;
            Ok(match op {
                RelOp::Ge => left >= right,
                RelOp::Gt => left > right,
                RelOp::Le => left <= right,
                RelOp::Lt => left < right,
            } as i32)
        }
    }
}

fn compute_eq_exp(exp: &EqExp, symtab: &SymbolTable) -> Result<i32, Error> {
    match exp {
        EqExp::Single(exp) => compute_rel_exp(exp, symtab),
        EqExp::Binary(left, op, right) => {
            let left = compute_eq_exp(left, symtab)?;
            let right = compute_rel_exp(right, symtab)?;
            Ok(match op {
                EqOp::Eq => left == right,
                EqOp::Neq => left != right,
            } as i32)
        }
    }
}

fn compute_land_exp(exp: &LAndExp, symtab: &SymbolTable) -> Result<i32, Error> {
    match exp {
        LAndExp::Single(exp) => compute_eq_exp(exp, symtab),
        LAndExp::Binary(left, right) => {
            let left = compute_land_exp(left, symtab)?;
            let right = compute_eq_exp(right, symtab)?;
            Ok((left != 0 && right != 0) as i32)
        }
    }
}

fn compute_lor_exp(exp: &LOrExp, symtab: &SymbolTable) -> Result<i32, Error> {
    match exp {
        LOrExp::Single(exp) => compute_land_exp(exp, symtab),
        LOrExp::Binary(left, right) => {
            let left = compute_lor_exp(left, symtab)?;
            let right = compute_land_exp(right, symtab)?;
            Ok((left != 0 || right != 0) as i32)
        }
    }
}

fn build_stmt(
    func: &mut FunctionData,
    bb: BasicBlock,
    stmt: &Stmt,
    symtab: &SymbolTable,
) -> Result<(), Error> {
    match stmt {
        Stmt::Assign(lval, exp) => {
            let value = build_exp(func, bb, exp, symtab)?;
            let store = new_value!(func).store(value, symtab.get_var(&lval.ident)?);
            new_inst!(func, bb, store);
        }
        Stmt::Return(exp) => {
            let ret_val = build_exp(func, bb, exp, symtab)?;
            let ret = new_value!(func).ret(Some(ret_val));
            new_inst!(func, bb, ret);
        }
    };
    Ok(())
}

fn build_exp(
    func: &mut FunctionData,
    bb: BasicBlock,
    exp: &Exp,
    symtab: &SymbolTable,
) -> Result<Value, Error> {
    build_lor_exp(func, bb, &exp.lor_exp, symtab)
}

fn build_unary_exp(
    func: &mut FunctionData,
    bb: BasicBlock,
    exp: &UnaryExp,
    symtab: &SymbolTable,
) -> Result<Value, Error> {
    match exp {
        UnaryExp::Single(exp) => build_primary_exp(func, bb, exp, symtab),
        UnaryExp::Unary(unary_op, exp) => {
            let value = build_unary_exp(func, bb, exp, symtab)?;
            match unary_op {
                UnaryOp::Pos => Ok(value),
                UnaryOp::Neg => {
                    let zero = new_value!(func).integer(0);
                    let neg = new_value!(func).binary(BinaryOp::Sub, zero, value);
                    new_inst!(func, bb, neg);
                    Ok(neg)
                }
                UnaryOp::Not => {
                    let zero = new_value!(func).integer(0);
                    let not = new_value!(func).binary(BinaryOp::Eq, value, zero);
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
    symtab: &SymbolTable,
) -> Result<Value, Error> {
    match exp {
        PrimaryExp::Expression(exp) => build_exp(func, bb, exp.as_ref(), symtab),
        PrimaryExp::Number(num) => Ok(new_value!(func).integer(*num)),
        PrimaryExp::LVal(lval) => match symtab.get_symbol(&lval.ident)? {
            Symbol::Const(val) => Ok(new_value!(func).integer(val)),
            Symbol::Var(value) => {
                let temp = new_value!(func).load(value);
                new_inst!(func, bb, temp);
                Ok(temp)
            }
        },
    }
}

fn build_mul_exp(
    func: &mut FunctionData,
    bb: BasicBlock,
    exp: &MulExp,
    symtab: &SymbolTable,
) -> Result<Value, Error> {
    match exp {
        MulExp::Single(exp) => build_unary_exp(func, bb, exp, symtab),
        MulExp::Binary(left, op, right) => {
            let left = build_mul_exp(func, bb, left, symtab)?;
            let right = build_unary_exp(func, bb, right, symtab)?;
            let value = new_value!(func).binary(
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

fn build_add_exp(
    func: &mut FunctionData,
    bb: BasicBlock,
    exp: &AddExp,
    symtab: &SymbolTable,
) -> Result<Value, Error> {
    match exp {
        AddExp::Single(exp) => build_mul_exp(func, bb, exp, symtab),
        AddExp::Binary(left, op, right) => {
            let left = build_add_exp(func, bb, left, symtab)?;
            let right = build_mul_exp(func, bb, right, symtab)?;
            let value = new_value!(func).binary(
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

fn build_rel_exp(
    func: &mut FunctionData,
    bb: BasicBlock,
    exp: &RelExp,
    symtab: &SymbolTable,
) -> Result<Value, Error> {
    match exp {
        RelExp::Single(exp) => build_add_exp(func, bb, exp, symtab),
        RelExp::Binary(left, op, right) => {
            let left = build_rel_exp(func, bb, left, symtab)?;
            let right = build_add_exp(func, bb, right, symtab)?;
            let value = new_value!(func).binary(
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

fn build_eq_exp(
    func: &mut FunctionData,
    bb: BasicBlock,
    exp: &EqExp,
    symtab: &SymbolTable,
) -> Result<Value, Error> {
    match exp {
        EqExp::Single(exp) => build_rel_exp(func, bb, exp, symtab),
        EqExp::Binary(left, op, right) => {
            let left = build_eq_exp(func, bb, left, symtab)?;
            let right = build_rel_exp(func, bb, right, symtab)?;
            let value = new_value!(func).binary(
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
fn build_land_exp(
    func: &mut FunctionData,
    bb: BasicBlock,
    exp: &LAndExp,
    symtab: &SymbolTable,
) -> Result<Value, Error> {
    match exp {
        LAndExp::Single(exp) => build_eq_exp(func, bb, exp, symtab),
        LAndExp::Binary(left, right) => {
            let zero = new_value!(func).integer(0);
            let left = build_land_exp(func, bb, left, symtab)?;
            let l = new_value!(func).binary(BinaryOp::NotEq, left, zero);
            new_inst!(func, bb, l);
            let right = build_eq_exp(func, bb, right, symtab)?;
            let r = new_value!(func).binary(BinaryOp::NotEq, right, zero);
            new_inst!(func, bb, r);
            let value = new_value!(func).binary(BinaryOp::And, l, r);
            new_inst!(func, bb, value);
            Ok(value)
        }
    }
}

/// `a || b ` is equivalent to `(a | b) != 0`
fn build_lor_exp(
    func: &mut FunctionData,
    bb: BasicBlock,
    exp: &LOrExp,
    symtab: &SymbolTable,
) -> Result<Value, Error> {
    match exp {
        LOrExp::Single(exp) => build_land_exp(func, bb, exp, symtab),
        LOrExp::Binary(left, right) => {
            let zero = new_value!(func).integer(0);
            let left = build_lor_exp(func, bb, left, symtab)?;
            let right = build_land_exp(func, bb, right, symtab)?;
            let or = new_value!(func).binary(BinaryOp::Or, left, right);
            new_inst!(func, bb, or);
            let value = new_value!(func).binary(BinaryOp::NotEq, or, zero);
            new_inst!(func, bb, value);
            Ok(value)
        }
    }
}
