//! In this file, the conversion from AST to KoopaIR is provided.

use crate::ast::*;
use crate::symtab::{Symbol, SymbolTable};
use koopa::back::KoopaGenerator;
use koopa::ir::builder_traits::*;
use koopa::ir::{BasicBlock, BinaryOp, FunctionData, Program, Type, Value};

use std::io;
use std::sync::atomic::{AtomicUsize, Ordering};

// Global counter for if-else statements
static CONDITION_COUNTER: AtomicUsize = AtomicUsize::new(1);
static AND_COUNTER: AtomicUsize = AtomicUsize::new(1);
static OR_COUNTER: AtomicUsize = AtomicUsize::new(1);
static WHILE_COUNTER: AtomicUsize = AtomicUsize::new(1);
static UNUSED_COUNTER: AtomicUsize = AtomicUsize::new(1);

/// Add an instruction into a function.
macro_rules! new_inst {
    ($func:expr, $bb:expr, $inst:expr) => {
        if !is_unused_block($func, $bb) {
            $func
                .layout_mut()
                .bb_mut($bb)
                .insts_mut()
                .push_key_back($inst)
                .unwrap()
        }
    };
}

/// Add a new value into a function.
macro_rules! new_value {
    ($func:expr) => {
        $func.dfg_mut().new_value()
    };
}

/// Add a new basic block into a function.
macro_rules! new_bb {
    ($func:expr) => {
        $func.dfg_mut().new_bb()
    };
}

macro_rules! add_bb {
    ($func_data:expr, $entry:expr) => {
        $func_data
            .layout_mut()
            .bbs_mut()
            .push_key_back($entry)
            .unwrap()
    };
}

pub fn output_program(program: &Program, output: impl io::Write) {
    KoopaGenerator::new(output).generate_on(program).unwrap();
}

pub fn build_program(comp_unit: &CompUnit) -> Program {
    let mut program = Program::new();
    build_function(&mut program, &comp_unit.func_def);
    program
}

fn build_function(program: &mut Program, func_def: &FuncDef) {
    let func = FunctionData::new(
        format!("@{}", func_def.ident),
        Vec::new(),
        match func_def.func_type {
            FuncType::Int => Type::get_i32(),
            FuncType::Void => Type::get_unit(),
        },
    );
    let func = program.new_func(func);
    let mut symtab = SymbolTable::new();

    let func_data = program.func_mut(func);
    let bb = new_bb!(func_data).basic_block(Some("%entry".into()));
    add_bb!(func_data, bb);

    build_block(func_data, bb, &func_def.block, &mut symtab);
}

fn is_unused_block(func: &mut FunctionData, bb: BasicBlock) -> bool {
    func.dfg()
        .bb(bb)
        .name()
        .as_ref()
        .expect("basic blocks should have a non-default name")
        .starts_with("%unused")
}

fn build_block(
    func: &mut FunctionData,
    bb: BasicBlock,
    block: &Block,
    symtab: &mut SymbolTable,
) -> BasicBlock {
    symtab.enter_block();
    let mut next_bb = bb;
    for block_item in block.block_items.iter() {
        next_bb = build_block_item(func, next_bb, block_item, symtab);
    }
    symtab.quit_block().unwrap();
    next_bb
}

fn build_block_item(
    func: &mut FunctionData,
    bb: BasicBlock,
    item: &BlockItem,
    symtab: &mut SymbolTable,
) -> BasicBlock {
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
) -> BasicBlock {
    match decl {
        Decl::Const(decl) => {
            build_const_decl(decl, symtab);
            bb
        }
        Decl::Var(decl) => build_var_decl(func, bb, decl, symtab),
    }
}

fn build_var_decl(
    func: &mut FunctionData,
    bb: BasicBlock,
    decl: &VarDecl,
    symtab: &mut SymbolTable,
) -> BasicBlock {
    let mut bb = bb;
    for def in decl.var_defs.iter() {
        bb = build_var_def(func, bb, def, symtab);
    }
    bb
}

fn build_var_def(
    func: &mut FunctionData,
    bb: BasicBlock,
    def: &VarDef,
    symtab: &mut SymbolTable,
) -> BasicBlock {
    if is_unused_block(func, bb) {
        return bb;
    }
    let var = new_value!(func).alloc(Type::get_i32());
    func.dfg_mut()
        .set_value_name(var, Some(format!("@{}_{}", def.ident, symtab.size())));
    new_inst!(func, bb, var);
    symtab.insert_var(&def.ident, var).unwrap();
    if let Some(init_value) = &def.init_val {
        let (value, bb) = build_init_value(func, bb, &init_value, symtab);
        let store = new_value!(func).store(value, var);
        new_inst!(func, bb, store);
        bb
    } else {
        bb
    }
}

fn build_init_value(
    func: &mut FunctionData,
    bb: BasicBlock,
    val: &InitVal,
    symtab: &mut SymbolTable,
) -> (Value, BasicBlock) {
    build_exp(func, bb, &val.exp, symtab)
}

fn build_const_decl(decl: &ConstDecl, symtab: &mut SymbolTable) {
    for def in decl.const_defs.iter() {
        build_const_def(def, symtab);
    }
}

fn build_const_def(decl: &ConstDef, symtab: &mut SymbolTable) {
    let value = compute_init_value(&decl.const_init_val, symtab);
    symtab.insert_const(&decl.ident, value).unwrap();
}

fn compute_init_value(init: &ConstInitVal, symtab: &SymbolTable) -> i32 {
    compute_exp(&init.const_exp.exp, symtab)
}

fn compute_exp(exp: &Exp, symtab: &SymbolTable) -> i32 {
    compute_lor_exp(&exp.lor_exp, symtab)
}

fn compute_unary_exp(exp: &UnaryExp, symtab: &SymbolTable) -> i32 {
    match exp {
        UnaryExp::Single(exp) => compute_primary_exp(exp, symtab),
        UnaryExp::Unary(op, exp) => {
            let val = compute_unary_exp(exp, symtab);
            match op {
                UnaryOp::Pos => val,
                UnaryOp::Neg => -val,
                UnaryOp::Not => !val,
            }
        }
        UnaryExp::Call(ident, params) => {
            panic!("cannot evaluate function at compile time");
        }
    }
}

fn compute_primary_exp(exp: &PrimaryExp, symtab: &SymbolTable) -> i32 {
    match exp {
        PrimaryExp::Expression(exp) => compute_exp(exp, symtab),
        PrimaryExp::Number(val) => *val,
        PrimaryExp::LVal(lval) => symtab.get_const(&lval.ident).unwrap(),
    }
}

fn compute_mul_exp(exp: &MulExp, symtab: &SymbolTable) -> i32 {
    match exp {
        MulExp::Single(exp) => compute_unary_exp(exp, symtab),
        MulExp::Binary(left, op, right) => {
            let left = compute_mul_exp(left, symtab);
            let right = compute_unary_exp(right, symtab);
            match op {
                MulOp::Mul => left * right,
                MulOp::Div => left / right,
                MulOp::Mod => left % right,
            }
        }
    }
}

fn compute_add_exp(exp: &AddExp, symtab: &SymbolTable) -> i32 {
    match exp {
        AddExp::Single(exp) => compute_mul_exp(exp, symtab),
        AddExp::Binary(left, op, right) => {
            let left = compute_add_exp(left, symtab);
            let right = compute_mul_exp(right, symtab);
            match op {
                AddOp::Add => left + right,
                AddOp::Sub => left - right,
            }
        }
    }
}

fn compute_rel_exp(exp: &RelExp, symtab: &SymbolTable) -> i32 {
    match exp {
        RelExp::Single(exp) => compute_add_exp(exp, symtab),
        RelExp::Binary(left, op, right) => {
            let left = compute_rel_exp(left, symtab);
            let right = compute_add_exp(right, symtab);
            let result = match op {
                RelOp::Ge => left >= right,
                RelOp::Gt => left > right,
                RelOp::Le => left <= right,
                RelOp::Lt => left < right,
            };
            result as i32
        }
    }
}

fn compute_eq_exp(exp: &EqExp, symtab: &SymbolTable) -> i32 {
    match exp {
        EqExp::Single(exp) => compute_rel_exp(exp, symtab),
        EqExp::Binary(left, op, right) => {
            let left = compute_eq_exp(left, symtab);
            let right = compute_rel_exp(right, symtab);
            let result = match op {
                EqOp::Eq => left == right,
                EqOp::Neq => left != right,
            };
            result as i32
        }
    }
}

fn compute_land_exp(exp: &LAndExp, symtab: &SymbolTable) -> i32 {
    match exp {
        LAndExp::Single(exp) => compute_eq_exp(exp, symtab),
        LAndExp::Binary(left, right) => {
            let left = compute_land_exp(left, symtab);
            let right = compute_eq_exp(right, symtab);
            (left != 0 && right != 0) as i32
        }
    }
}

fn compute_lor_exp(exp: &LOrExp, symtab: &SymbolTable) -> i32 {
    match exp {
        LOrExp::Single(exp) => compute_land_exp(exp, symtab),
        LOrExp::Binary(left, right) => {
            let left = compute_lor_exp(left, symtab);
            let right = compute_land_exp(right, symtab);
            (left != 0 || right != 0) as i32
        }
    }
}

fn build_stmt(
    func: &mut FunctionData,
    bb: BasicBlock,
    stmt: &Stmt,
    symtab: &mut SymbolTable,
) -> BasicBlock {
    if is_unused_block(func, bb) {
        return bb;
    }

    match stmt {
        Stmt::Assign(lval, exp) => {
            let (value, bb) = build_exp(func, bb, exp, symtab);
            let store = new_value!(func).store(value, symtab.get_var(&lval.ident).unwrap());
            new_inst!(func, bb, store);
            bb
        }
        Stmt::Return(exp) => {
            let (ret_val, bb) = build_exp(func, bb, exp, symtab);
            let ret = new_value!(func).ret(Some(ret_val));
            new_inst!(func, bb, ret);
            let id = UNUSED_COUNTER.fetch_add(1, Ordering::Relaxed);
            let end_bb = new_bb!(func).basic_block(Some(format!("%unused_{id}")));
            // add_bb!(func, end_bb);
            end_bb
        }
        Stmt::Exp(exp) => {
            if let Some(exp) = exp {
                build_exp(func, bb, exp, symtab);
            }
            bb
        }
        Stmt::Block(block) => {
            let next_bb = build_block(func, bb, block, symtab);
            next_bb
        }
        Stmt::Condition(cond, true_branch, false_branch) => {
            let id = CONDITION_COUNTER.fetch_add(1, Ordering::Relaxed);
            let (cond, bb) = build_exp(func, bb, cond, symtab);
            let end_bb = new_bb!(func).basic_block(Some(format!("%endif_{id}")));

            let true_bb = new_bb!(func).basic_block(Some(format!("%then_{id}")));
            add_bb!(func, true_bb);
            let true_end = build_stmt(func, true_bb, true_branch, symtab);
            let true_jmp = new_value!(func).jump(end_bb);
            new_inst!(func, true_end, true_jmp);

            let else_bb = {
                if let Some(false_branch) = false_branch {
                    let false_bb = new_bb!(func).basic_block(Some(format!("%else_{id}")));
                    add_bb!(func, false_bb);
                    let false_end = build_stmt(func, false_bb, false_branch, symtab);
                    add_bb!(func, end_bb);
                    let false_jmp = new_value!(func).jump(end_bb);
                    new_inst!(func, false_end, false_jmp);
                    false_bb
                } else {
                    add_bb!(func, end_bb);
                    end_bb
                }
            };
            let branch = new_value!(func).branch(cond, true_bb, else_bb);
            new_inst!(func, bb, branch);
            end_bb
        }

        Stmt::While(exp, stmt) => {
            let id = WHILE_COUNTER.fetch_add(1, Ordering::Relaxed);
            let entry = new_bb!(func).basic_block(Some(format!("%while_entry_{id}")));
            add_bb!(func, entry);
            let end = new_bb!(func).basic_block(Some(format!("%while_end_{id}")));

            symtab.enter_loop(entry, end);

            let enter = new_value!(func).jump(entry);
            new_inst!(func, bb, enter);
            let (cond, end_entry) = build_exp(func, entry, exp, symtab);

            let body = new_bb!(func).basic_block(Some(format!("%while_body_{id}")));
            add_bb!(func, body);
            let end_body = build_stmt(func, body, stmt, symtab);
            let jump = new_value!(func).jump(entry);
            new_inst!(func, end_body, jump);

            add_bb!(func, end);

            let branch = new_value!(func).branch(cond, body, end);
            new_inst!(func, end_entry, branch);

            symtab.quit_loop().unwrap();

            end
        }

        Stmt::Break => {
            let id = UNUSED_COUNTER.fetch_add(1, Ordering::Relaxed);
            let target = symtab.loop_end().unwrap();
            let jump = new_value!(func).jump(target);
            new_inst!(func, bb, jump);
            let end_bb = new_bb!(func).basic_block(Some(format!("%unused_{id}")));
            end_bb
        }

        Stmt::Continue => {
            let id = UNUSED_COUNTER.fetch_add(1, Ordering::Relaxed);
            let target = symtab.loop_entry().unwrap();
            let jump = new_value!(func).jump(target);
            new_inst!(func, bb, jump);
            let end_bb = new_bb!(func).basic_block(Some(format!("%unused_{id}")));
            end_bb
        }
    }
}

fn build_exp(
    func: &mut FunctionData,
    bb: BasicBlock,
    exp: &Exp,
    symtab: &SymbolTable,
) -> (Value, BasicBlock) {
    build_lor_exp(func, bb, &exp.lor_exp, symtab)
}

fn build_unary_exp(
    func: &mut FunctionData,
    bb: BasicBlock,
    exp: &UnaryExp,
    symtab: &SymbolTable,
) -> (Value, BasicBlock) {
    match exp {
        UnaryExp::Single(exp) => build_primary_exp(func, bb, exp, symtab),
        UnaryExp::Unary(unary_op, exp) => {
            let (value, bb) = build_unary_exp(func, bb, exp, symtab);
            match unary_op {
                UnaryOp::Pos => (value, bb),
                UnaryOp::Neg => {
                    let zero = new_value!(func).integer(0);
                    let neg = new_value!(func).binary(BinaryOp::Sub, zero, value);
                    new_inst!(func, bb, neg);
                    (neg, bb)
                }
                UnaryOp::Not => {
                    let zero = new_value!(func).integer(0);
                    let not = new_value!(func).binary(BinaryOp::Eq, value, zero);
                    new_inst!(func, bb, not);
                    (not, bb)
                }
            }
        }
        UnaryExp::Call(ident, params) => unimplemented!(),
    }
}

fn build_primary_exp(
    func: &mut FunctionData,
    bb: BasicBlock,
    exp: &PrimaryExp,
    symtab: &SymbolTable,
) -> (Value, BasicBlock) {
    match exp {
        PrimaryExp::Expression(exp) => build_exp(func, bb, exp.as_ref(), symtab),
        PrimaryExp::Number(num) => (new_value!(func).integer(*num), bb),
        PrimaryExp::LVal(lval) => match symtab.get_symbol(&lval.ident).unwrap() {
            Symbol::Const(val) => (new_value!(func).integer(val), bb),
            Symbol::Var(value) => {
                let temp = new_value!(func).load(value);
                new_inst!(func, bb, temp);
                (temp, bb)
            }
        },
    }
}

fn build_mul_exp(
    func: &mut FunctionData,
    bb: BasicBlock,
    exp: &MulExp,
    symtab: &SymbolTable,
) -> (Value, BasicBlock) {
    match exp {
        MulExp::Single(exp) => build_unary_exp(func, bb, exp, symtab),
        MulExp::Binary(left, op, right) => {
            let (left, bb) = build_mul_exp(func, bb, left, symtab);
            let (right, bb) = build_unary_exp(func, bb, right, symtab);
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
            (value, bb)
        }
    }
}

fn build_add_exp(
    func: &mut FunctionData,
    bb: BasicBlock,
    exp: &AddExp,
    symtab: &SymbolTable,
) -> (Value, BasicBlock) {
    match exp {
        AddExp::Single(exp) => build_mul_exp(func, bb, exp, symtab),
        AddExp::Binary(left, op, right) => {
            let (left, bb) = build_add_exp(func, bb, left, symtab);
            let (right, bb) = build_mul_exp(func, bb, right, symtab);
            let value = new_value!(func).binary(
                match op {
                    AddOp::Add => BinaryOp::Add,
                    AddOp::Sub => BinaryOp::Sub,
                },
                left,
                right,
            );
            new_inst!(func, bb, value);
            (value, bb)
        }
    }
}

fn build_rel_exp(
    func: &mut FunctionData,
    bb: BasicBlock,
    exp: &RelExp,
    symtab: &SymbolTable,
) -> (Value, BasicBlock) {
    match exp {
        RelExp::Single(exp) => build_add_exp(func, bb, exp, symtab),
        RelExp::Binary(left, op, right) => {
            let (left, bb) = build_rel_exp(func, bb, left, symtab);
            let (right, bb) = build_add_exp(func, bb, right, symtab);
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
            (value, bb)
        }
    }
}

fn build_eq_exp(
    func: &mut FunctionData,
    bb: BasicBlock,
    exp: &EqExp,
    symtab: &SymbolTable,
) -> (Value, BasicBlock) {
    match exp {
        EqExp::Single(exp) => build_rel_exp(func, bb, exp, symtab),
        EqExp::Binary(left, op, right) => {
            let (left, bb) = build_eq_exp(func, bb, left, symtab);
            let (right, bb) = build_rel_exp(func, bb, right, symtab);
            let value = new_value!(func).binary(
                match op {
                    EqOp::Eq => BinaryOp::Eq,
                    EqOp::Neq => BinaryOp::NotEq,
                },
                left,
                right,
            );
            new_inst!(func, bb, value);
            (value, bb)
        }
    }
}

/// `a && b ` is equivalent to
/// ```c
/// result = 0;
/// if (a) {
///     result = (b != 0);
/// }
/// ```
fn build_land_exp(
    func: &mut FunctionData,
    bb: BasicBlock,
    exp: &LAndExp,
    symtab: &SymbolTable,
) -> (Value, BasicBlock) {
    match exp {
        LAndExp::Single(exp) => build_eq_exp(func, bb, exp, symtab),
        LAndExp::Binary(left, right) => {
            let result = new_value!(func).alloc(Type::get_i32());
            new_inst!(func, bb, result);
            let zero = new_value!(func).integer(0);
            let assign = new_value!(func).store(zero, result);
            new_inst!(func, bb, assign);

            let (left, bb) = build_land_exp(func, bb, left, symtab);

            let id = AND_COUNTER.fetch_add(1, Ordering::Relaxed);
            let and = new_bb!(func).basic_block(Some(format!("%and_{id}")));
            add_bb!(func, and);
            let end_and = new_bb!(func).basic_block(Some(format!("%endand_{id}")));
            add_bb!(func, end_and);

            let branch = new_value!(func).branch(left, and, end_and);
            new_inst!(func, bb, branch);

            let (right, bb) = build_eq_exp(func, and, right, symtab);
            let zero = new_value!(func).integer(0);
            let right = new_value!(func).binary(BinaryOp::NotEq, right, zero);
            new_inst!(func, bb, right);
            let assign = new_value!(func).store(right, result);
            new_inst!(func, bb, assign);
            let jmp = new_value!(func).jump(end_and);
            new_inst!(func, bb, jmp);

            let result = new_value!(func).load(result);
            new_inst!(func, end_and, result);
            (result, end_and)
        }
    }
}

/// `a || b ` is equivalent to
/// ```c
/// result = 1;
/// if (!a) {
///     result = (b != 0);
/// }
/// ```
fn build_lor_exp(
    func: &mut FunctionData,
    bb: BasicBlock,
    exp: &LOrExp,
    symtab: &SymbolTable,
) -> (Value, BasicBlock) {
    match exp {
        LOrExp::Single(exp) => build_land_exp(func, bb, exp, symtab),
        LOrExp::Binary(left, right) => {
            let zero = new_value!(func).integer(0);
            let one = new_value!(func).integer(1);

            let result = new_value!(func).alloc(Type::get_i32());
            new_inst!(func, bb, result);
            let assign = new_value!(func).store(one, result);
            new_inst!(func, bb, assign);

            let (left, bb) = build_lor_exp(func, bb, left, symtab);

            let id = OR_COUNTER.fetch_add(1, Ordering::Relaxed);
            let or = new_bb!(func).basic_block(Some(format!("%or_{id}")));
            add_bb!(func, or);
            let end_or = new_bb!(func).basic_block(Some(format!("%endor_{id}")));
            add_bb!(func, end_or);

            let branch = new_value!(func).branch(left, end_or, or);
            new_inst!(func, bb, branch);

            let (right, bb) = build_land_exp(func, or, right, symtab);
            let right = new_value!(func).binary(BinaryOp::NotEq, right, zero);
            new_inst!(func, bb, right);
            let assign = new_value!(func).store(right, result);
            new_inst!(func, bb, assign);
            let jmp = new_value!(func).jump(end_or);
            new_inst!(func, bb, jmp);

            let result = new_value!(func).load(result);
            new_inst!(func, end_or, result);
            (result, end_or)
        }
    }
}
