//! In this file, the conversion from AST to KoopaIR is provided.
//!
//! The core function [`build_program`] converts a
//! [`CompUnit`] AST into a Koopa [`Program`], and
//! [`output_program`] just simply output a Koopa [`Program`].

use crate::ast::*;
use crate::build_util::{Symbol, SymbolTable};
use koopa::back::KoopaGenerator;
use koopa::ir::{builder::LocalBuilder, builder_traits::*, TypeKind, ValueKind};
use koopa::ir::{BasicBlock, BinaryOp, Function, FunctionData, Program, Type, Value};

use std::convert::Into;
use std::io;

/// One of the core features. Output a KoopaIR program into output, by using the given API.
pub fn output_program(program: &Program, output: impl io::Write) {
    KoopaGenerator::new(output).generate_on(program).unwrap();
}

/// One of the core features. Parse an AST into a KoopaIR Program.
pub fn build_program(comp_unit: &CompUnit) -> Program {
    let mut program = Program::new();
    let mut symtab = SymbolTable::new();
    declare_builtins(&mut program, &mut symtab);
    build_comp_unit(&mut program, comp_unit, &mut symtab);
    program
}

/// Declare Sysy builtin functions.
fn declare_builtins(program: &mut Program, symtab: &mut SymbolTable) {
    let builtin_functions: Vec<(&'static str, Vec<Type>, Type)> = vec![
        // decl @getint(): i32
        ("getint", vec![], Type::get_i32()),
        // decl @getch(): i32
        ("getch", vec![], Type::get_i32()),
        // decl @getarray(*i32): i32
        (
            "getarray",
            vec![Type::get_pointer(Type::get_i32())],
            Type::get_i32(),
        ),
        // decl @putint(i32)
        ("putint", vec![Type::get_i32()], Type::get_unit()),
        // decl @putch(i32)
        ("putch", vec![Type::get_i32()], Type::get_unit()),
        // decl @putarray(i32, *i32)
        (
            "putarray",
            vec![Type::get_i32(), Type::get_pointer(Type::get_i32())],
            Type::get_unit(),
        ),
        // decl @starttime()
        ("starttime", vec![], Type::get_unit()),
        // decl @stoptime()
        ("stoptime", vec![], Type::get_unit()),
    ];

    for (name, params_ty, ret_ty) in builtin_functions {
        let data = FunctionData::new_decl(format!("@{name}"), params_ty, ret_ty);
        let function = program.new_func(data);
        symtab.insert_function(name.to_string(), function);
    }
}

/// Write a [`CompUnit`] into a [`Program`].
fn build_comp_unit(program: &mut Program, comp_unit: &CompUnit, symtab: &mut SymbolTable) {
    for item in comp_unit.items.iter() {
        match item.as_ref() {
            GlobalItem::Decl(decl) => build_global_decl(program, decl, symtab),
            GlobalItem::FuncDef(func_def) => build_function(program, func_def, symtab),
        }
    }
}

/// Write a **global** [`Decl`] into a [`Program`],
/// with functions [`build_global_const_def`], [`build_global_var_def`] used.
fn build_global_decl(program: &mut Program, decl: &Decl, symtab: &mut SymbolTable) {
    assert_eq!(decl.btype, BuiltinType::Int, "Invalid global value type");
    for def in decl.defs.iter() {
        if decl.mutable {
            build_global_var_def(program, def, symtab);
        } else {
            build_global_const_def(program, def, symtab);
        }
    }
}

/// Write a **const** **global** [`Def`] into a [`Program`].
fn build_global_const_def(program: &mut Program, def: &Def, symtab: &mut SymbolTable) {
    let shape = compute_shape(&def.shape, symtab);
    let data = def.init.as_ref().expect("expected an initial value");
    let data = compute_init_value(data, shape.clone(), symtab);

    if shape.is_empty() {
        symtab.insert_const(def.ident.clone(), data[0]);
    } else {
        let data = data
            .into_iter()
            .map(|i| program.new_value().integer(i))
            .collect();
        let value = packing(|e| program.new_value().aggregate(e), data, &shape);
        let array = program.new_value().global_alloc(value);
        program.set_value_name(array, Some(format!("@{}", def.ident)));
        let ty = program.borrow_value(array).ty().clone();
        symtab.insert_var(def.ident.clone(), array, ty);
    }
}

/// Write a **non-const** **global** [`Def`] into a [`Program`].
fn build_global_var_def(program: &mut Program, def: &Def, symtab: &mut SymbolTable) {
    let shape = compute_shape(&def.shape, symtab);
    let data: Value = match &def.init {
        Some(init_val) => {
            let elems = compute_init_value(init_val, shape.clone(), symtab);
            if shape.is_empty() {
                program.new_value().integer(elems[0])
            } else {
                let data = elems
                    .into_iter()
                    .map(|i| program.new_value().integer(i))
                    .collect();
                packing(|e| program.new_value().aggregate(e), data, &shape)
            }
        }
        None => program.new_value().zero_init(get_array_type(&shape)),
    };

    let var = program.new_value().global_alloc(data);
    program.set_value_name(var, Some(format!("@{}", def.ident)));
    let ty = program.borrow_value(var).ty().clone();
    symtab.insert_var(def.ident.clone(), var, ty);
}

/// Write a [`FuncDef`] into a [`Program`].
fn build_function(program: &mut Program, func_def: &FuncDef, symtab: &mut SymbolTable) {
    if func_def.block.is_none() {
        let name = &func_def.ident;
        let params_ty = func_def
            .params
            .iter()
            .map(|p| get_type(p.btype, &p.shape, symtab))
            .collect();
        let ret_ty = get_type(func_def.func_type, &None, symtab);
        let data = FunctionData::new_decl(format!("@{name}"), params_ty, ret_ty);
        let function = program.new_func(data);
        symtab.insert_function(name.to_string(), function);
        return;
    }
    symtab.enter_block();
    let (func, ret) = parse_function(program, func_def, symtab);
    let func_data = program.func_mut(func);
    symtab.insert_function(func_def.ident.clone(), func);

    let bb = new_bb(func_data, "%koopa_builtin_entry".into());
    add_bb(func_data, bb);

    build_params(func_data, bb, &func_def.params, symtab);

    let bb = build_block(func_data, bb, func_def.block.as_ref().unwrap(), symtab);
    if !is_unused_block(func_data, bb) {
        let ret = if ret.is_unit() {
            new_value(func_data).ret(None)
        } else {
            let zero = new_value(func_data).integer(0);
            new_value(func_data).ret(Some(zero))
        };
        add_value(func_data, bb, ret);
    }
    symtab.quit_block();
}

/// Initialize a [`FunctionData`] with a given [`FuncDef`].
/// Returns the handle [`Function`] and its returning [`Type`].
fn parse_function(
    program: &mut Program,
    func_def: &FuncDef,
    symtab: &SymbolTable,
) -> (Function, Type) {
    let params = parse_params(func_def, symtab);
    let ret_ty = match func_def.func_type {
        BuiltinType::Int => Type::get_i32(),
        BuiltinType::Void => Type::get_unit(),
    };
    let f = FunctionData::with_param_names(format!("@{}", func_def.ident), params, ret_ty.clone());
    (program.new_func(f), ret_ty)
}

/// Parsing arguments of a function.
/// Returns a [`Vec`] containing the name and [`Type`] of each argument.
fn parse_params(func_def: &FuncDef, symtab: &SymbolTable) -> Vec<(Option<String>, Type)> {
    let mut params = Vec::new();
    for param in func_def.params.iter() {
        params.push((
            Some(format!("@{}", param.ident)),
            match &param.shape {
                None => Type::get_i32(),
                Some(shape) => {
                    let shape = compute_shape(shape, symtab);
                    Type::get_pointer(get_array_type(&shape))
                }
            },
        ))
    }
    params
}

/// After entering the function block, we need to store the arguments locally.
fn build_params(
    func: &mut FunctionData,
    bb: BasicBlock,
    params: &Vec<Box<FuncFParam>>,
    symtab: &mut SymbolTable,
) {
    for (i, param) in params.iter().enumerate() {
        let value = func.params()[i];
        let ident = format!("%{}", param.ident);
        let ty = func.dfg().value(value).ty().clone();
        let p = new_value(func).alloc(ty);
        func.dfg_mut().set_value_name(p, Some(ident.clone()));
        add_value(func, bb, p);
        let store = new_value(func).store(value, p);
        add_value(func, bb, store);
        let ty = func.dfg().value(p).ty().clone();
        symtab.insert_var(param.ident.clone(), p, ty);
    }
}

/// Builds a [`Block`] into a [`FunctionData`].
fn build_block(
    func: &mut FunctionData,
    bb: BasicBlock,
    block: &Block,
    symtab: &mut SymbolTable,
) -> BasicBlock {
    symtab.enter_block();
    let mut next_bb = bb;
    for block_item in block.items.iter() {
        next_bb = build_block_item(func, next_bb, block_item, symtab);
    }
    symtab.quit_block();
    next_bb
}

/// Builds a [`BlockItem`] into a [`FunctionData`].
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

/// Builds a [`Decl`] into a [`Function`].
fn build_decl(
    func: &mut FunctionData,
    bb: BasicBlock,
    decl: &Decl,
    symtab: &mut SymbolTable,
) -> BasicBlock {
    if decl.mutable {
        let mut bb = bb;
        for def in decl.defs.iter() {
            bb = build_var_def(func, bb, def, symtab);
        }
        bb
    } else {
        for def in decl.defs.iter() {
            build_const_def(func, bb, def, symtab);
        }
        bb
    }
}

/// Builds a [`Def`] into a [`FunctionData`].
fn build_var_def(
    func: &mut FunctionData,
    bb: BasicBlock,
    def: &Def,
    symtab: &mut SymbolTable,
) -> BasicBlock {
    if is_unused_block(func, bb) {
        return bb;
    }

    let shape = compute_shape(&def.shape, symtab);

    let var = new_value(func).alloc(get_array_type(&shape));
    func.dfg_mut()
        .set_value_name(var, Some(format!("@{}_{}", def.ident, symtab.get_id())));
    add_value(func, bb, var);
    let ty = func.dfg().value(var).ty().clone();
    symtab.insert_var(def.ident.clone(), var, ty);
    if let Some(init_value) = &def.init {
        let (values, bb) = build_init_value(func, bb, &init_value, shape.clone(), symtab);
        let value = packing(|e| new_value(func).aggregate(e), values, &shape);
        let store = new_value(func).store(value, var);
        add_value(func, bb, store);
        bb
    } else {
        bb
    }
}

/// Builds a **const** [`Def`] into a [`FunctionData`].
fn build_const_def(func: &mut FunctionData, bb: BasicBlock, def: &Def, symtab: &mut SymbolTable) {
    let shape: Vec<usize> = def
        .shape
        .iter()
        .map(|exp| compute_exp(exp, symtab) as usize)
        .collect();
    let data = def.init.as_ref().expect("expected an initial value");
    let data = compute_init_value(data, shape.clone(), symtab);

    if shape.is_empty() {
        symtab.insert_const(def.ident.clone(), data[0]);
    } else {
        let values = data.iter().map(|&i| new_value(func).integer(i)).collect();
        let value = packing(|elems| new_value(func).aggregate(elems), values, &shape);
        let ty = func.dfg().value(value).ty().clone();
        let array = new_value(func).alloc(ty);
        add_value(func, bb, array);
        let store = new_value(func).store(value, array);
        add_value(func, bb, store);
        func.dfg_mut()
            .set_value_name(array, Some(format!("@{}_{}", &def.ident, symtab.get_id())));
        let ty = func.dfg().value(array).ty().clone();
        symtab.insert_var(def.ident.clone(), array, ty);
    }
}

/// Compute the value of an [`Exp`] at compile time.
fn compute_exp(exp: &Exp, symtab: &SymbolTable) -> i32 {
    match exp {
        Exp::Unary(op, exp) => op.compute(compute_exp(exp, symtab)),
        Exp::Binary(left, op, right) => {
            op.compute(compute_exp(left, symtab), compute_exp(right, symtab))
        }
        Exp::Call(_, _) => panic!("cannot evaluate function at compile time"),
        Exp::LVal(lval) => symtab.get_const(&lval.ident),
        Exp::Number(val) => val.clone(),
    }
}

/// Builds a [`LVal`] into a [`FunctionData`].
fn build_lval(
    func: &mut FunctionData,
    bb: BasicBlock,
    lval: &LVal,
    symtab: &mut SymbolTable,
) -> (Value, BasicBlock, Type) {
    let mut bb = bb;
    let (array, ty) = symtab.get_var(&lval.ident);
    let mut current_type = match ty.kind() {
        TypeKind::Pointer(ty) => ty.clone(),
        _ => unreachable!(),
    };
    let mut pointer = array;
    for index in lval.index.iter() {
        let (index, next_bb) = build_exp(func, bb, index, symtab);
        bb = next_bb;
        match current_type.kind() {
            TypeKind::Array(next_ty, _) => {
                current_type = next_ty.clone();
                pointer = new_value(func).get_elem_ptr(pointer, index);
                add_value(func, bb, pointer);
            }
            TypeKind::Pointer(next_ty) => {
                current_type = next_ty.clone();
                pointer = new_value(func).load(pointer);
                add_value(func, bb, pointer);
                pointer = new_value(func).get_ptr(pointer, index);
                add_value(func, bb, pointer);
            }
            _ => unimplemented!(),
        };
    }
    (pointer, bb, current_type)
}

/// Builds a [`Stmt`] into a [`FunctionData`].
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
            let (pos, bb, _) = build_lval(func, bb, lval, symtab);
            let store = new_value(func).store(value, pos);
            add_value(func, bb, store);
            bb
        }

        Stmt::Return(exp) => {
            if let Some(exp) = exp {
                let (ret_val, bb) = build_exp(func, bb, exp, symtab);
                let ret = new_value(func).ret(Some(ret_val));
                add_value(func, bb, ret);
            } else {
                let ret = new_value(func).ret(None);
                add_value(func, bb, ret);
            }
            let id = symtab.get_id();
            let end_bb = new_bb(func, format!("%koopa_builtin_unused_{id}"));
            // add_bb(func, end_bb);
            end_bb
        }
        Stmt::Exp(exp) => {
            if let Some(exp) = exp {
                build_exp(func, bb, exp, symtab).1
            } else {
                bb
            }
        }
        Stmt::Block(block) => {
            let next_bb = build_block(func, bb, block, symtab);
            next_bb
        }
        Stmt::Condition(cond, true_branch, false_branch) => {
            let id = symtab.get_id();
            let then_bb = new_bb(func, format!("%koopa_builtin_then_{id}"));
            let end_bb = new_bb(func, format!("%koopa_builtin_end_if_{id}"));
            let else_bb = if false_branch.is_some() {
                new_bb(func, format!("%koopa_builtin_else_{id}"))
            } else {
                end_bb
            };
            build_logical_exp(func, bb, then_bb, else_bb, cond, symtab);
            add_bb(func, then_bb);
            let end_then = build_stmt(func, then_bb, true_branch, symtab);
            let jmp = new_value(func).jump(end_bb);
            add_value(func, end_then, jmp);
            if let Some(false_branch) = false_branch {
                add_bb(func, else_bb);
                let end_else = build_stmt(func, else_bb, false_branch, symtab);
                let jmp = new_value(func).jump(end_bb);
                add_value(func, end_else, jmp);
            }
            add_bb(func, end_bb);
            end_bb
        }

        Stmt::While(exp, stmt) => {
            let id = symtab.get_id();
            let entry = new_bb(func, format!("%koopa_builtin_while_entry_{id}"));
            let exit = new_bb(func, format!("%koopa_builtin_while_end_{id}"));
            let body = new_bb(func, format!("%koopa_builtin_while_body_{id}"));
            symtab.enter_loop(entry, exit);
            add_bb(func, entry);
            let enter = new_value(func).jump(entry);
            add_value(func, bb, enter);
            build_logical_exp(func, entry, body, exit, exp, symtab);
            add_bb(func, body);
            let end_body = build_stmt(func, body, stmt, symtab);
            let enter = new_value(func).jump(entry);
            add_value(func, end_body, enter);
            add_bb(func, exit);
            symtab.quit_loop();
            exit
        }

        Stmt::Break => {
            let id = symtab.get_id();
            let target = symtab.loop_end();
            let jump = new_value(func).jump(target);
            add_value(func, bb, jump);
            let end_bb = new_bb(func, format!("%koopa_builtin_unused_{id}"));
            end_bb
        }

        Stmt::Continue => {
            let id = symtab.get_id();
            let target = symtab.loop_entry();
            let jump = new_value(func).jump(target);
            add_value(func, bb, jump);
            let end_bb = new_bb(func, format!("%koopa_builtin_unused_{id}"));
            end_bb
        }
    }
}

/// Builds an [`Exp`] into a [`FunctionData`].
fn build_exp(
    func: &mut FunctionData,
    bb: BasicBlock,
    exp: &Exp,
    symtab: &mut SymbolTable,
) -> (Value, BasicBlock) {
    match exp {
        // Build a single number
        Exp::Number(num) => (new_value(func).integer(*num), bb),
        // Build a left value
        Exp::LVal(lval) => match symtab.get_symbol(&lval.ident) {
            Symbol::Const(val) => (new_value(func).integer(val), bb),
            _ => {
                let (pos, bb, ty) = build_lval(func, bb, lval, symtab);
                if matches!(ty.kind(), TypeKind::Array(_, _)) {
                    let zero = new_value(func).integer(0);
                    let pos = new_value(func).get_elem_ptr(pos, zero);
                    add_value(func, bb, pos);
                    (pos, bb)
                } else {
                    let load = new_value(func).load(pos);
                    add_value(func, bb, load);
                    (load, bb)
                }
            }
        },
        // Build unary expression
        Exp::Unary(op, exp) => {
            let (value, bb) = build_exp(func, bb, exp, symtab);
            if let Some(val) = get_integer(func, value) {
                return (new_value(func).integer(op.compute(val)), bb);
            }
            match op {
                UnaryOperator::Pos => (value, bb),
                UnaryOperator::Neg => {
                    let zero = new_value(func).integer(0);
                    let neg = new_value(func).binary(BinaryOp::Sub, zero, value);
                    add_value(func, bb, neg);
                    (neg, bb)
                }
                UnaryOperator::Not => {
                    let zero = new_value(func).integer(0);
                    let not = new_value(func).binary(BinaryOp::Eq, value, zero);
                    add_value(func, bb, not);
                    (not, bb)
                }
            }
        }
        // Build a function call
        Exp::Call(ident, params) => {
            let mut bb = bb;
            let mut args = Vec::new();
            for param in params.iter() {
                let (value, next_bb) = build_exp(func, bb, param, symtab);
                bb = next_bb;
                args.push(value);
            }
            let result = new_value(func).call(symtab.get_function(ident), args);
            add_value(func, bb, result);
            (result, bb)
        }
        // Build a binary expression
        Exp::Binary(_, BinaryOperator::And | BinaryOperator::Or, _) => {
            let one = new_value(func).integer(1);
            let zero = new_value(func).integer(0);
            let id = symtab.get_id();
            let true_bb = new_bb(func, format!("%koopa_builtin_true_{id}"));
            let false_bb = new_bb(func, format!("%koopa_builtin_false_{id}"));
            let exit_bb = new_bb(func, format!("%koopa_builtin_end_{id}"));
            let result = new_value(func).alloc(Type::get_i32());
            add_value(func, bb, result);
            build_logical_exp(func, bb, true_bb, false_bb, exp, symtab);
            add_bb(func, true_bb);
            add_bb(func, false_bb);
            let assign = new_value(func).store(one, result);
            add_value(func, true_bb, assign);
            let assign = new_value(func).store(zero, result);
            add_value(func, false_bb, assign);
            let jump = new_value(func).jump(exit_bb);
            add_value(func, true_bb, jump);
            add_value(func, false_bb, jump);
            add_bb(func, exit_bb);
            let result = new_value(func).load(result);
            add_value(func, exit_bb, result);
            (result, exit_bb)
        }
        // Default
        Exp::Binary(left, op, right) => {
            let (left, bb) = build_exp(func, bb, left, symtab);
            let (right, bb) = build_exp(func, bb, right, symtab);
            if let Some(left_value) = get_integer(func, left) {
                if let Some(right_value) = get_integer(func, right) {
                    return (
                        new_value(func).integer(op.compute(left_value, right_value)),
                        bb,
                    );
                }
            }
            let value = new_value(func).binary(op.clone().into(), left, right);
            add_value(func, bb, value);
            (value, bb)
        }
    }
}

/// Builds a **logical** [`Exp`] into a [`FunctionData`].
/// The jump or branch instructions is dealed within this function.
/// It will jump to `true_bb` when the `exp` is true, and to `false_bb` otherwise.
fn build_logical_exp(
    func: &mut FunctionData,
    bb: BasicBlock,
    true_bb: BasicBlock,
    false_bb: BasicBlock,
    exp: &Exp,
    symtab: &mut SymbolTable,
) {
    match exp {
        // Build a single number
        Exp::Number(num) => {
            let jmp = new_value(func).jump(if *num != 0 { true_bb } else { false_bb });
            add_value(func, bb, jmp);
        }
        // Build unary expression
        Exp::Unary(op, exp) => {
            if matches!(op, UnaryOperator::Not) {
                build_logical_exp(func, bb, false_bb, true_bb, exp, symtab)
            } else {
                build_logical_exp(func, bb, true_bb, false_bb, exp, symtab)
            }
        }
        // Build a binary expression
        Exp::Binary(left, BinaryOperator::And, right) => {
            let temp_bb = new_bb(func, format!("%koopa_builtin_and_{}", symtab.get_id()));
            build_logical_exp(func, bb, temp_bb, false_bb, left, symtab);
            add_bb(func, temp_bb);
            build_logical_exp(func, temp_bb, true_bb, false_bb, right, symtab);
        }
        Exp::Binary(left, BinaryOperator::Or, right) => {
            let temp_bb = new_bb(func, format!("%koopa_builtin_or_{}", symtab.get_id()));
            build_logical_exp(func, bb, true_bb, temp_bb, left, symtab);
            add_bb(func, temp_bb);
            build_logical_exp(func, temp_bb, true_bb, false_bb, right, symtab);
        }
        // Others, just evaluate and jump
        _ => {
            let (value, bb) = build_exp(func, bb, exp, symtab);
            let branch = new_value(func).branch(value, true_bb, false_bb);
            add_value(func, bb, branch);
        }
    }
}

// Helper functions

/// Judge whether a block is marked with unused.
fn is_unused_block(func: &mut FunctionData, bb: BasicBlock) -> bool {
    let name = func.dfg().bb(bb).name().as_ref();
    name.expect("basic blocks should have a non-default name")
        .starts_with("%koopa_builtin_unused")
}

/// Add a new [`Value`] into a [`BasicBlock`] of a [`FunctionData`].
fn add_value(func: &mut FunctionData, bb: BasicBlock, inst: Value) {
    if !is_unused_block(func, bb) {
        func.layout_mut()
            .bb_mut(bb)
            .insts_mut()
            .push_key_back(inst)
            .unwrap();
    }
}

/// Create a new [`Value`] within a [`FunctionData`].
fn new_value(func: &mut FunctionData) -> LocalBuilder {
    func.dfg_mut().new_value()
}

/// Add a new [`BasicBlock`] into a [`FunctionData`].
fn add_bb(func: &mut FunctionData, bb: BasicBlock) {
    func.layout_mut().bbs_mut().push_key_back(bb).unwrap()
}

/// Create a new [`BasicBlock`] within a [`FunctionData`].
fn new_bb(func: &mut FunctionData, name: String) -> BasicBlock {
    func.dfg_mut().new_bb().basic_block(Some(name))
}

fn get_subshape(shape: &Vec<usize>, offset: usize) -> Vec<usize> {
    let mut size = shape.last().unwrap().clone();
    assert!(offset % size == 0, "array not aligned");
    let mut index = 1;
    for i in (1..(shape.len() - 1)).rev() {
        size *= shape[i];
        if offset % size != 0 {
            index = i + 1;
            break;
        }
    }
    shape[index..].to_vec()
}

fn packing<F>(mut packer: F, data: Vec<Value>, shape: &Vec<usize>) -> Value
where
    F: FnMut(Vec<Value>) -> Value,
{
    assert_eq!(data.len(), shape.iter().product());
    let mut data = data;
    for &length in shape.iter().rev() {
        let mut next_data = Vec::new();
        let mut pack = Vec::new();
        for value in data {
            pack.push(value);
            if pack.len() == length {
                next_data.push(packer(pack));
                pack = Vec::new();
            }
        }
        data = next_data
    }
    data[0]
}

fn compute_init_value(init: &InitVal, shape: Vec<usize>, symtab: &SymbolTable) -> Vec<i32> {
    fn _compute_init_value(
        init: &InitVal,
        shape: &Vec<usize>,
        symtab: &SymbolTable,
        data: &mut Vec<i32>,
        offset: usize,
    ) -> usize {
        match init {
            InitVal::Single(exp) => {
                data[offset] = compute_exp(exp, symtab);
                offset + 1
            }
            InitVal::Array(exps) => {
                let s = get_subshape(&shape, offset);
                let end_offset = offset + s.iter().product::<usize>();
                let mut offset = offset;
                for exp in exps.iter() {
                    offset = _compute_init_value(exp, &s, symtab, data, offset);
                }
                end_offset
            }
        }
    }

    let size = shape.iter().product();
    let mut data = Vec::new();
    data.resize(size, 0);
    match init {
        InitVal::Single(exp) => {
            data[0] = compute_exp(exp, symtab);
        }
        InitVal::Array(exps) => {
            let mut offset = 0;
            for exp in exps.iter() {
                offset = _compute_init_value(exp, &shape, symtab, &mut data, offset);
            }
        }
    }
    data
}

fn build_init_value(
    func: &mut FunctionData,
    bb: BasicBlock,
    val: &InitVal,
    shape: Vec<usize>,
    symtab: &mut SymbolTable,
) -> (Vec<Value>, BasicBlock) {
    fn _build_init_array_value(
        func: &mut FunctionData,
        bb: BasicBlock,
        val: &InitVal,
        shape: &Vec<usize>,
        symtab: &mut SymbolTable,
        data: &mut Vec<Value>,
        offset: usize,
    ) -> (usize, BasicBlock) {
        match val {
            InitVal::Single(exp) => {
                let (value, next_bb) = build_exp(func, bb, exp, symtab);
                data[offset] = value;
                (offset + 1, next_bb)
            }
            InitVal::Array(exps) => {
                let s = get_subshape(&shape, offset);
                let end_offset = offset + s.iter().product::<usize>();
                let mut offset = offset;
                let mut bb = bb;
                for exp in exps.iter() {
                    let (next_offset, next_bb) =
                        _build_init_array_value(func, bb, exp, &s, symtab, data, offset);
                    offset = next_offset;
                    bb = next_bb;
                }
                (end_offset, bb)
            }
        }
    }

    let size = shape.iter().product();
    let mut data = Vec::new();
    data.resize(size, new_value(func).integer(0));
    match val {
        InitVal::Single(exp) => {
            let (value, next_bb) = build_exp(func, bb, exp, symtab);
            data[0] = value;
            (data, next_bb)
        }
        InitVal::Array(exps) => {
            let mut offset = 0;
            let mut bb = bb;
            for exp in exps.iter() {
                let (next_offset, next_bb) =
                    _build_init_array_value(func, bb, exp, &shape, symtab, &mut data, offset);
                offset = next_offset;
                bb = next_bb;
            }
            (data, bb)
        }
    }
}

fn get_array_type(shape: &Vec<usize>) -> Type {
    let mut ty = Type::get_i32();
    for &length in shape.iter().rev() {
        ty = Type::get_array(ty, length);
    }
    ty
}

fn compute_shape(shape: &Vec<Box<Exp>>, symtab: &SymbolTable) -> Vec<usize> {
    shape
        .iter()
        .map(|e| compute_exp(e, symtab) as usize)
        .collect()
}

fn get_integer(func: &FunctionData, val: Value) -> Option<i32> {
    match func.dfg().value(val).kind() {
        ValueKind::Integer(i) => Some(i.value()),
        _ => None,
    }
}

fn get_type(btype: BuiltinType, shape: &Option<Vec<Box<Exp>>>, symtab: &SymbolTable) -> Type {
    match btype {
        BuiltinType::Void => Type::get_unit(),
        BuiltinType::Int => match shape {
            None => Type::get_i32(),
            Some(shape) => Type::get_pointer(get_array_type(&compute_shape(shape, symtab))),
        },
    }
}
