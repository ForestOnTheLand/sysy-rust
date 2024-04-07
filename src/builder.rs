//! In this file, the conversion from AST to KoopaIR is provided.

use crate::ast::*;
use crate::build_util::{Symbol, SymbolTable};
use koopa::back::KoopaGenerator;
use koopa::ir::{builder_traits::*, TypeKind, ValueKind};
use koopa::ir::{BasicBlock, BinaryOp, Function, FunctionData, Program, Type, Value};

use std::io;

/// Add a new value into a function.
macro_rules! new_value {
    ($func:expr) => {
        $func.dfg_mut().new_value()
    };
}

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
    let builtin_functions: Vec<(String, Vec<Type>, Type)> = vec![
        // decl @getint(): i32
        ("getint".to_string(), vec![], Type::get_i32()),
        // decl @getch(): i32
        ("getch".to_string(), vec![], Type::get_i32()),
        // decl @getarray(*i32): i32
        (
            "getarray".to_string(),
            vec![Type::get_pointer(Type::get_i32())],
            Type::get_i32(),
        ),
        // decl @putint(i32)
        (
            "putint".to_string(),
            vec![Type::get_i32()],
            Type::get_unit(),
        ),
        // decl @putch(i32)
        ("putch".to_string(), vec![Type::get_i32()], Type::get_unit()),
        // decl @putarray(i32, *i32)
        (
            "putarray".to_string(),
            vec![Type::get_i32(), Type::get_pointer(Type::get_i32())],
            Type::get_unit(),
        ),
        // decl @starttime()
        ("starttime".to_string(), vec![], Type::get_unit()),
        // decl @stoptime()
        ("stoptime".to_string(), vec![], Type::get_unit()),
    ];

    for (name, params_ty, ret_ty) in builtin_functions {
        let data = FunctionData::new_decl(format!("@{name}"), params_ty, ret_ty);
        let function = program.new_func(data);
        symtab.insert_function(name, function);
    }
}

/// Write a [`CompUnit`] into a program.
fn build_comp_unit(program: &mut Program, comp_unit: &CompUnit, symtab: &mut SymbolTable) {
    for item in comp_unit.items.iter() {
        match item.as_ref() {
            GlobalItem::Decl(decl) => build_global_decl(program, decl, symtab),
            GlobalItem::FuncDef(func_def) => build_function(program, func_def, symtab),
        }
    }
}

/// Write a **global** [`GlobalDecl`] into a program,
/// with functions [`build_global_const_decl`], [`build_global_var_decl`] used.
fn build_global_decl(program: &mut Program, decl: &GlobalDecl, symtab: &mut SymbolTable) {
    match decl {
        GlobalDecl::Const(decl) => build_global_const_decl(program, decl, symtab),
        GlobalDecl::Var(decl) => build_global_var_decl(program, decl, symtab),
    };
}

/// Write a **global** [`ConstDecl`] into a program,
/// with function [`build_global_const_def`] used.
fn build_global_const_decl(program: &mut Program, decl: &ConstDecl, symtab: &mut SymbolTable) {
    assert_eq!(decl.btype, BuiltinType::Int);
    for def in decl.const_defs.iter() {
        build_global_const_def(program, def, symtab);
    }
}

/// Write a **global** [`ConstDef`] into a program,
/// with function [`compute_init_value`] used.
fn build_global_const_def(program: &mut Program, def: &ConstDef, symtab: &mut SymbolTable) {
    let shape = compute_shape(&def.shape, symtab);
    let data = compute_init_value(&def.const_init_val, shape.clone(), symtab);

    if shape.is_empty() {
        symtab.insert_const(def.ident.clone(), data[0]);
    } else {
        let value = global_packing(program, &data, &shape);
        let array = program.new_value().global_alloc(value);
        program.set_value_name(array, Some(format!("@{}", def.ident)));
        let ty = program.borrow_value(array).ty().clone();
        symtab.insert_var(def.ident.clone(), array, ty);
    }
}

/// Write a **global** [`GlobalVarDecl`] into a program,
/// with function [`build_global_var_def`] used.
fn build_global_var_decl(program: &mut Program, decl: &GlobalVarDecl, symtab: &mut SymbolTable) {
    assert_eq!(decl.btype, BuiltinType::Int);
    for def in decl.var_defs.iter() {
        build_global_var_def(program, def, symtab);
    }
}

/// Write a **global** [`GlobalVarDef`] into a program,
/// with function [`build_global_var_def`] used.
fn build_global_var_def(program: &mut Program, def: &GlobalVarDef, symtab: &mut SymbolTable) {
    let shape = compute_shape(&def.shape, symtab);
    let data: Value = match &def.init_val {
        Some(init_val) => {
            let elems = compute_init_value(init_val, shape.clone(), symtab);
            if shape.is_empty() {
                program.new_value().integer(elems[0])
            } else {
                global_packing(program, &elems, &shape)
            }
        }
        None => program.new_value().zero_init(get_array_type(&shape)),
    };

    let var = program.new_value().global_alloc(data);
    program.set_value_name(var, Some(format!("@{}", def.ident)));
    let ty = program.borrow_value(var).ty().clone();
    symtab.insert_var(def.ident.clone(), var, ty);
}

/// Write a [`FuncDef`] into a program.
fn build_function(program: &mut Program, func_def: &FuncDef, symtab: &mut SymbolTable) {
    symtab.enter_block();
    let (func, ret) = parse_function(program, func_def, symtab);
    let func_data = program.func_mut(func);
    symtab.insert_function(func_def.ident.clone(), func);

    let bb = new_bb(func_data, "%koopa_builtin_entry".into());
    add_bb(func_data, bb);

    if let Some(params) = func_def.params.as_ref() {
        build_params(func_data, bb, params, symtab);
    }

    let bb = build_block(func_data, bb, &func_def.block, symtab);
    if !is_unused_block(func_data, bb) {
        if ret.is_unit() {
            let ret = new_value!(func_data).ret(None);
            add_value(func_data, bb, ret);
        } else {
            let zero = new_value!(func_data).integer(0);
            let ret = new_value!(func_data).ret(Some(zero));
            add_value(func_data, bb, ret);
        }
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
    if let Some(p) = &func_def.params {
        for param in p.params.iter() {
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
    }
    params
}

/// After entering the function block, we need to store the arguments locally.
fn build_params(
    func: &mut FunctionData,
    bb: BasicBlock,
    params: &FuncFParams,
    symtab: &mut SymbolTable,
) {
    for i in 0..params.params.len() {
        let value = func.params()[i];
        let ident = format!("%{}", params.params[i].ident);
        let ty = func.dfg().value(value).ty().clone();
        let p = new_value!(func).alloc(ty);
        func.dfg_mut().set_value_name(p, Some(ident.clone()));
        add_value(func, bb, p);
        let store = new_value!(func).store(value, p);
        add_value(func, bb, store);
        let ty = func.dfg().value(p).ty().clone();
        symtab.insert_var(params.params[i].ident.clone(), p, ty);
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
    for block_item in block.block_items.iter() {
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

/// Builds a declaration into a function.
fn build_decl(
    func: &mut FunctionData,
    bb: BasicBlock,
    decl: &Decl,
    symtab: &mut SymbolTable,
) -> BasicBlock {
    match decl {
        Decl::Const(decl) => {
            build_const_decl(func, bb, decl, symtab);
            bb
        }
        Decl::Var(decl) => build_var_decl(func, bb, decl, symtab),
    }
}

/// Builds a [`VarDecl`] into a [`FunctionData`].
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

/// Builds a [`VarDef`] into a [`FunctionData`].
fn build_var_def(
    func: &mut FunctionData,
    bb: BasicBlock,
    def: &VarDef,
    symtab: &mut SymbolTable,
) -> BasicBlock {
    if is_unused_block(func, bb) {
        return bb;
    }

    let shape = compute_shape(&def.shape, symtab);

    let var = new_value!(func).alloc(get_array_type(&shape));
    func.dfg_mut()
        .set_value_name(var, Some(format!("@{}_{}", def.ident, symtab.layer_id())));
    add_value(func, bb, var);
    let ty = func.dfg().value(var).ty().clone();
    symtab.insert_var(def.ident.clone(), var, ty);
    if let Some(init_value) = &def.init_val {
        let (values, bb) = build_init_value(func, bb, &init_value, shape.clone(), symtab);
        let value = local_packing(func, values, &shape);
        let store = new_value!(func).store(value, var);
        add_value(func, bb, store);
        bb
    } else {
        bb
    }
}

fn build_init_value(
    func: &mut FunctionData,
    bb: BasicBlock,
    val: &InitVal,
    shape: Vec<usize>,
    symtab: &mut SymbolTable,
) -> (Vec<Value>, BasicBlock) {
    let size = shape.iter().product();
    let mut data = Vec::new();
    data.resize(size, new_value!(func).integer(0));
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

fn build_const_decl(
    func: &mut FunctionData,
    bb: BasicBlock,
    decl: &ConstDecl,
    symtab: &mut SymbolTable,
) {
    for def in decl.const_defs.iter() {
        build_const_def(func, bb, def, symtab);
    }
}

fn build_const_def(
    func: &mut FunctionData,
    bb: BasicBlock,
    def: &ConstDef,
    symtab: &mut SymbolTable,
) {
    let shape: Vec<usize> = def
        .shape
        .iter()
        .map(|exp| compute_const_exp(exp, symtab) as usize)
        .collect();
    let data = compute_init_value(&def.const_init_val, shape.clone(), symtab);

    if shape.is_empty() {
        symtab.insert_const(def.ident.clone(), data[0]);
    } else {
        let values = data.iter().map(|&i| new_value!(func).integer(i)).collect();
        let value = local_packing(func, values, &shape);
        let ty = func.dfg().value(value).ty().clone();
        let array = new_value!(func).alloc(ty);
        add_value(func, bb, array);
        let store = new_value!(func).store(value, array);
        add_value(func, bb, store);
        func.dfg_mut().set_value_name(
            array,
            Some(format!("@{}_{}", &def.ident, symtab.layer_id())),
        );
        let ty = func.dfg().value(array).ty().clone();
        symtab.insert_var(def.ident.clone(), array, ty);
    }
}

fn compute_init_value(init: &ConstInitVal, shape: Vec<usize>, symtab: &SymbolTable) -> Vec<i32> {
    let size = shape.iter().product();
    let mut data = Vec::new();
    data.resize(size, 0);
    match init {
        ConstInitVal::Single(exp) => {
            data[0] = compute_const_exp(exp, symtab);
        }
        ConstInitVal::Array(exps) => {
            let mut offset = 0;
            for exp in exps.iter() {
                offset = _compute_init_value(exp, &shape, symtab, &mut data, offset);
            }
        }
    }
    data
}

fn _compute_init_value(
    init: &ConstInitVal,
    shape: &Vec<usize>,
    symtab: &SymbolTable,
    data: &mut Vec<i32>,
    offset: usize,
) -> usize {
    match init {
        ConstInitVal::Single(exp) => {
            data[offset] = compute_const_exp(exp, symtab);
            offset + 1
        }
        ConstInitVal::Array(exps) => {
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

fn compute_const_exp(exp: &ConstExp, symtab: &SymbolTable) -> i32 {
    compute_exp(&exp.exp, symtab)
}

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

fn build_lval(
    func: &mut FunctionData,
    bb: BasicBlock,
    lval: &LVal,
    symtab: &SymbolTable,
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
                pointer = new_value!(func).get_elem_ptr(pointer, index);
                add_value(func, bb, pointer);
            }
            TypeKind::Pointer(next_ty) => {
                current_type = next_ty.clone();
                pointer = new_value!(func).load(pointer);
                add_value(func, bb, pointer);
                pointer = new_value!(func).get_ptr(pointer, index);
                add_value(func, bb, pointer);
            }
            _ => unimplemented!(),
        };
    }
    (pointer, bb, current_type)
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
            let (pos, bb, _) = build_lval(func, bb, lval, symtab);
            let store = new_value!(func).store(value, pos);
            add_value(func, bb, store);
            bb
        }

        Stmt::Return(exp) => {
            if let Some(exp) = exp {
                let (ret_val, bb) = build_exp(func, bb, exp, symtab);
                let ret = new_value!(func).ret(Some(ret_val));
                add_value(func, bb, ret);
            } else {
                let ret = new_value!(func).ret(None);
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
            let (cond, bb) = build_exp(func, bb, cond, symtab);
            let end_bb = new_bb(func, format!("%koopa_builtin_end_if_{id}"));

            let true_bb = new_bb(func, format!("%koopa_builtin_then_{id}"));
            add_bb(func, true_bb);
            let true_end = build_stmt(func, true_bb, true_branch, symtab);
            let true_jmp = new_value!(func).jump(end_bb);
            add_value(func, true_end, true_jmp);

            let else_bb = {
                if let Some(false_branch) = false_branch {
                    let false_bb = new_bb(func, format!("%koopa_builtin_else_{id}"));
                    add_bb(func, false_bb);
                    let false_end = build_stmt(func, false_bb, false_branch, symtab);
                    add_bb(func, end_bb);
                    let false_jmp = new_value!(func).jump(end_bb);
                    add_value(func, false_end, false_jmp);
                    false_bb
                } else {
                    add_bb(func, end_bb);
                    end_bb
                }
            };
            let branch = new_value!(func).branch(cond, true_bb, else_bb);
            add_value(func, bb, branch);
            end_bb
        }

        Stmt::While(exp, stmt) => {
            let id = symtab.get_id();
            let entry = new_bb(func, format!("%koopa_builtin_while_entry_{id}"));
            add_bb(func, entry);
            let end = new_bb(func, format!("%koopa_builtin_while_end_{id}"));

            symtab.enter_loop(entry, end);

            let enter = new_value!(func).jump(entry);
            add_value(func, bb, enter);
            let (cond, end_entry) = build_exp(func, entry, exp, symtab);

            let body = new_bb(func, format!("%koopa_builtin_while_body_{id}"));
            add_bb(func, body);
            let end_body = build_stmt(func, body, stmt, symtab);
            let jump = new_value!(func).jump(entry);
            add_value(func, end_body, jump);

            add_bb(func, end);

            let branch = new_value!(func).branch(cond, body, end);
            add_value(func, end_entry, branch);

            symtab.quit_loop();

            end
        }

        Stmt::Break => {
            let id = symtab.get_id();
            let target = symtab.loop_end();
            let jump = new_value!(func).jump(target);
            add_value(func, bb, jump);
            let end_bb = new_bb(func, format!("%koopa_builtin_unused_{id}"));
            end_bb
        }

        Stmt::Continue => {
            let id = symtab.get_id();
            let target = symtab.loop_entry();
            let jump = new_value!(func).jump(target);
            add_value(func, bb, jump);
            let end_bb = new_bb(func, format!("%koopa_builtin_unused_{id}"));
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
    match exp {
        // Build a single number
        Exp::Number(num) => (new_value!(func).integer(*num), bb),
        // Build a left value
        Exp::LVal(lval) => match symtab.get_symbol(&lval.ident) {
            Symbol::Const(val) => (new_value!(func).integer(val), bb),
            _ => {
                let (pos, bb, ty) = build_lval(func, bb, lval, symtab);
                if matches!(ty.kind(), TypeKind::Array(_, _)) {
                    let zero = new_value!(func).integer(0);
                    let pos = new_value!(func).get_elem_ptr(pos, zero);
                    add_value(func, bb, pos);
                    (pos, bb)
                } else {
                    let load = new_value!(func).load(pos);
                    add_value(func, bb, load);
                    (load, bb)
                }
            }
        },
        // Build unary expression
        Exp::Unary(op, exp) => {
            let (value, bb) = build_exp(func, bb, exp, symtab);
            if let Some(val) = get_integer(func, value) {
                return (new_value!(func).integer(op.compute(val)), bb);
            }
            match op {
                UnaryOperator::Pos => (value, bb),
                UnaryOperator::Neg => {
                    let zero = new_value!(func).integer(0);
                    let neg = new_value!(func).binary(BinaryOp::Sub, zero, value);
                    add_value(func, bb, neg);
                    (neg, bb)
                }
                UnaryOperator::Not => {
                    let zero = new_value!(func).integer(0);
                    let not = new_value!(func).binary(BinaryOp::Eq, value, zero);
                    add_value(func, bb, not);
                    (not, bb)
                }
            }
        }
        // Build a function call
        Exp::Call(ident, params) => {
            let mut bb = bb;
            let mut args = Vec::new();
            if let Some(params) = params {
                for param in params.exps.iter() {
                    let (value, next_bb) = build_exp(func, bb, param, symtab);
                    bb = next_bb;
                    args.push(value);
                }
            }
            let result = new_value!(func).call(symtab.get_function(ident), args);
            add_value(func, bb, result);
            (result, bb)
        }
        // Build a binary expression
        Exp::Binary(left, op, right) => match op {
            BinaryOperator::And => {
                let zero = new_value!(func).integer(0);

                let (left, bb) = build_exp(func, bb, left, symtab);

                if let Some(left_value) = get_integer(func, left) {
                    if left_value == 0 {
                        return (zero, bb);
                    } else {
                        let (right, bb) = build_exp(func, bb, right, symtab);
                        if let Some(right_value) = get_integer(func, right) {
                            return (new_value!(func).integer((right_value != 0) as i32), bb);
                        }
                        let right = new_value!(func).binary(BinaryOp::NotEq, right, zero);
                        add_value(func, bb, right);
                        return (right, bb);
                    }
                }

                let result = new_value!(func).alloc(Type::get_i32());
                add_value(func, bb, result);
                let assign = new_value!(func).store(zero, result);
                add_value(func, bb, assign);

                let id = symtab.get_id();
                let and = new_bb(func, format!("%koopa_builtin_and_{id}"));
                add_bb(func, and);
                let end_and = new_bb(func, format!("%koopa_builtin_end_and_{id}"));

                let branch = new_value!(func).branch(left, and, end_and);
                add_value(func, bb, branch);

                let (right, bb) = build_exp(func, and, right, symtab);
                let right = if let Some(right_value) = get_integer(func, right) {
                    new_value!(func).integer((right_value != 0) as i32)
                } else {
                    let right = new_value!(func).binary(BinaryOp::NotEq, right, zero);
                    add_value(func, bb, right);
                    right
                };
                let assign = new_value!(func).store(right, result);
                add_value(func, bb, assign);
                let jmp = new_value!(func).jump(end_and);
                add_value(func, bb, jmp);

                add_bb(func, end_and);
                let result = new_value!(func).load(result);
                add_value(func, end_and, result);
                (result, end_and)
            }
            BinaryOperator::Or => {
                let zero = new_value!(func).integer(0);
                let one = new_value!(func).integer(1);

                let (left, bb) = build_exp(func, bb, left, symtab);

                if let Some(left_value) = get_integer(func, left) {
                    if left_value != 0 {
                        return (one, bb);
                    }
                    let (right, bb) = build_exp(func, bb, right, symtab);
                    if let Some(right_value) = get_integer(func, right) {
                        return (if right_value == 0 { zero } else { one }, bb);
                    }
                    let right = new_value!(func).binary(BinaryOp::NotEq, right, zero);
                    add_value(func, bb, right);
                    return (right, bb);
                }

                let result = new_value!(func).alloc(Type::get_i32());
                add_value(func, bb, result);
                let assign = new_value!(func).store(one, result);
                add_value(func, bb, assign);

                let id = symtab.get_id();
                let or = new_bb(func, format!("%koopa_builtin_or_{id}"));
                add_bb(func, or);
                let end_or = new_bb(func, format!("%koopa_builtin_end_or_{id}"));

                let branch = new_value!(func).branch(left, end_or, or);
                add_value(func, bb, branch);

                let (right, bb) = build_exp(func, or, right, symtab);
                let right = if let Some(right_value) = get_integer(func, right) {
                    new_value!(func).integer((right_value != 0) as i32)
                } else {
                    let right = new_value!(func).binary(BinaryOp::NotEq, right, zero);
                    add_value(func, bb, right);
                    right
                };
                let assign = new_value!(func).store(right, result);
                add_value(func, bb, assign);
                let jmp = new_value!(func).jump(end_or);
                add_value(func, bb, jmp);

                add_bb(func, end_or);
                let result = new_value!(func).load(result);
                add_value(func, end_or, result);
                (result, end_or)
            }
            // Default
            _ => {
                let (left, bb) = build_exp(func, bb, left, symtab);
                let (right, bb) = build_exp(func, bb, right, symtab);
                if let Some(left_value) = get_integer(func, left) {
                    if let Some(right_value) = get_integer(func, right) {
                        return (
                            new_value!(func).integer(op.compute(left_value, right_value)),
                            bb,
                        );
                    }
                }
                let value = new_value!(func).binary(op.clone().into(), left, right);
                add_value(func, bb, value);
                (value, bb)
            }
        },
    }
}

// Helper functions

/// Judge whether a block is marked with unused
fn is_unused_block(func: &mut FunctionData, bb: BasicBlock) -> bool {
    func.dfg()
        .bb(bb)
        .name()
        .as_ref()
        .expect("basic blocks should have a non-default name")
        .starts_with("%koopa_builtin_unused")
}

/// Add a new instruction into a basic block.
fn add_value(func: &mut FunctionData, bb: BasicBlock, inst: Value) {
    if !is_unused_block(func, bb) {
        func.layout_mut()
            .bb_mut(bb)
            .insts_mut()
            .push_key_back(inst)
            .unwrap()
    }
}

/// Add a new basic block into a function.
fn add_bb(func: &mut FunctionData, bb: BasicBlock) {
    func.layout_mut().bbs_mut().push_key_back(bb).unwrap()
}

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

fn global_packing(program: &mut Program, data: &Vec<i32>, shape: &Vec<usize>) -> Value {
    assert_eq!(data.len(), shape.iter().product());
    let mut data: Vec<Value> = data
        .iter()
        .map(|&i| program.new_value().integer(i))
        .collect();
    for &length in shape.iter().rev() {
        let mut next_data = Vec::new();
        let mut pack = Vec::new();
        for value in data {
            pack.push(value);
            if pack.len() == length {
                next_data.push(program.new_value().aggregate(pack));
                pack = Vec::new();
            }
        }
        data = next_data
    }
    data[0]
}

fn local_packing(func: &mut FunctionData, data: Vec<Value>, shape: &Vec<usize>) -> Value {
    assert_eq!(data.len(), shape.iter().product());
    let mut data = data;
    for &length in shape.iter().rev() {
        let mut next_data = Vec::new();
        let mut pack = Vec::new();
        for value in data {
            pack.push(value);
            if pack.len() == length {
                next_data.push(new_value!(func).aggregate(pack));
                pack = Vec::new();
            }
        }
        data = next_data
    }
    data[0]
}

fn get_array_type(shape: &Vec<usize>) -> Type {
    let mut ty = Type::get_i32();
    for &length in shape.iter().rev() {
        ty = Type::get_array(ty, length);
    }
    ty
}

fn compute_shape(shape: &Vec<Box<ConstExp>>, symtab: &SymbolTable) -> Vec<usize> {
    shape
        .iter()
        .map(|e| compute_const_exp(e, symtab) as usize)
        .collect()
}

fn get_integer(func: &FunctionData, val: Value) -> Option<i32> {
    match func.dfg().value(val).kind() {
        ValueKind::Integer(i) => Some(i.value()),
        _ => None,
    }
}
