//! In this file, the conversion from Koopa to RISCV is provided.
//! Instead of implementing a `trait` as is described in the writeup,
//! the core of the file is implemented in the function [`translate_program`].

use crate::translate_util::*;
use crate::util::Error;
use koopa::ir::{
    entities::ValueData, values::Aggregate, BasicBlock, BinaryOp, FunctionData, Program, TypeKind,
    Value, ValueKind,
};
use std::{cmp::max, io};

/// The core part of this module. Translate a KoopaIR program `program` into RISCV
/// assembly, which is written into the `output`.
///
/// # Panic
///
/// When encountering invalid KoopaIR program or unimplemented functions
///
pub fn translate_program(program: &Program, output: &mut impl io::Write) {
    // Global variables
    for &value in program.inst_layout() {
        translate_global_value(program, value, output);
    }

    // Functions
    for &func in program.func_layout() {
        let func_data = program.func(func);
        translate_function(program, func_data, output);
    }
}

/// Parsing global variables. For example:
///
/// ```riscv
///   .data
///   .globl x
/// x:
///   .zero 4
/// ```
///
fn translate_global_value(program: &Program, value: Value, output: &mut impl io::Write) {
    let value_data = program.borrow_value(value);
    let name = global_variable_name(&value_data).unwrap();
    writeln!(output, "  .data").unwrap();
    writeln!(output, "  .globl {}\n{}:", name, name).unwrap();

    match value_data.kind() {
        ValueKind::GlobalAlloc(alloc) => {
            let init_data = program.borrow_value(alloc.init());
            let size = init_data.ty().size();
            match init_data.kind() {
                ValueKind::ZeroInit(_) => writeln!(output, "  .zero {}", size).unwrap(),
                ValueKind::Integer(i) => writeln!(output, "  .word {}", i.value()).unwrap(),
                ValueKind::Aggregate(list) => {
                    let values = unpack(program, list);
                    for value in values {
                        if let ValueKind::Integer(i) = program.borrow_value(value).kind() {
                            writeln!(output, "  .word {}", i.value()).unwrap()
                        } else {
                            unreachable!()
                        }
                    }
                }
                _ => unreachable!(),
            }
        }
        _ => panic!("internal error: expected global alloc instruction"),
    }

    writeln!(output, "").unwrap();
}

/// Parsing function definitions, with declarations ignored.
/// For example:
///
/// ```riscv
///   .text
///   .globl main
/// main:
///   ret
/// ```
///
fn translate_function(program: &Program, func_data: &FunctionData, output: &mut impl io::Write) {
    // Ignore function declarations
    if func_data.layout().entry_bb() == None {
        return;
    }

    writeln!(output, "  .text").unwrap();
    let func_name = function_name(func_data).unwrap();
    writeln!(output, "  .globl {}\n{}:", func_name, func_name).unwrap();
    let (stack_size, save_ra, init_pos) = allocate_stack(func_data, output);

    let mut config = TranslateConfig {
        program,
        func_data,
        table: RegisterTable::new(),
        symbol: AllocTable::new(),
        stack_size,
        save_ra,
        stack_pos: Box::new(init_pos),
    };

    for (&bb, node) in func_data.layout().bbs() {
        let name = block_name(func_data, bb).unwrap();
        if !name.starts_with("entry") {
            writeln!(output, "{name}:").unwrap();
        }
        for &inst in node.insts().keys() {
            translate_instruction(inst, output, &mut config);
            writeln!(output, "").unwrap();
        }
    }
}

/// Getting the stack size needed for a given function.
/// The algorithm is described in detail in
/// [writeup](https://pku-minic.github.io/online-doc/#/lv8-func-n-global/func-def-n-call?id=%e7%94%9f%e6%88%90%e4%bb%a3%e7%a0%81).
///
/// Basically, there are 3 different usage of stack space:
/// - space for local variable
/// - space for saving RISCV register `ra` (short for "return address")
/// - space for function arguments
fn allocate_stack(func_data: &FunctionData, output: &mut impl io::Write) -> (usize, bool, i32) {
    let mut local = 0;
    let mut ra = 0;
    let mut args = 0;
    for (_bb, node) in func_data.layout().bbs() {
        for &inst in node.insts().keys() {
            if !func_data.dfg().value(inst).ty().is_unit() {
                let size = if matches!(func_data.dfg().value(inst).kind(), ValueKind::Alloc(_)) {
                    match func_data.dfg().value(inst).ty().kind() {
                        TypeKind::Pointer(value) => value.size(),
                        _ => unreachable!(),
                    }
                } else {
                    func_data.dfg().value(inst).ty().size()
                };
                local += size;
            }
            if let ValueKind::Call(call) = func_data.dfg().value(inst).kind() {
                ra = 4;
                if call.args().len() > 8 {
                    args = max(args, call.args().len() - 8);
                }
            }
        }
    }
    let total = local + ra + args * 4;
    let stack_size = (total + 15) & !0xf;
    if stack_size != 0 {
        if stack_size < 2048 {
            writeln!(output, "  addi sp, sp, -{}", stack_size).unwrap();
        } else {
            writeln!(output, "  li t0, {stack_size}").unwrap();
            writeln!(output, "  sub sp, sp, t0").unwrap();
        }
    }
    if ra != 0 {
        if stack_size - 4 < 2048 {
            writeln!(output, "  sw ra, {}(sp)", stack_size - 4).unwrap();
        } else {
            writeln!(output, "  li t0, {stack_size}").unwrap();
            writeln!(output, "  add t0, t0, sp").unwrap();
            writeln!(output, "  sw ra, 0(t0)").unwrap();
        }
    }
    (stack_size, ra != 0, (args * 4) as i32)
}

struct TranslateConfig<'a> {
    program: &'a Program,
    func_data: &'a FunctionData,
    table: RegisterTable,
    symbol: AllocTable,
    stack_size: usize,
    save_ra: bool,
    stack_pos: Box<i32>,
}

/// Translate a single KoopaIR instruction into several RISCV instructions.
fn translate_instruction(value: Value, output: &mut impl io::Write, config: &mut TranslateConfig) {
    match config.func_data.dfg().value(value).kind() {
        ValueKind::Integer(_) => unreachable!(),
        ValueKind::ZeroInit(_) => unreachable!(),
        ValueKind::Undef(_) => unreachable!(),
        ValueKind::Aggregate(_) => unreachable!(),
        ValueKind::FuncArgRef(_) => unreachable!(),
        ValueKind::BlockArgRef(_) => unreachable!(),
        ValueKind::Alloc(_) => {
            let size = match config.func_data.dfg().value(value).ty().kind() {
                TypeKind::Pointer(value) => value.size(),
                _ => unreachable!(),
            };
            writeln!(output, "  # at {}(sp), size {}", *config.stack_pos, size).unwrap();
            config.symbol.store_stack_pointer(value, *config.stack_pos);
            *config.stack_pos += size as i32;
        }
        ValueKind::GlobalAlloc(_) => unreachable!(),
        ValueKind::Load(load) => {
            let reg = if load.src().is_global() {
                let reg = config.table.get_vaccant().unwrap();
                let value_data = config.program.borrow_value(load.src());
                let name = global_variable_name(&value_data).unwrap();
                writeln!(output, "  la {reg}, {name}").unwrap();
                writeln!(output, "  lw {reg}, 0({reg})").unwrap();
                reg
            } else {
                let src = prepare_value(load.src(), output, config);
                writeln!(output, "  lw {src}, 0({src})").unwrap();
                src
            };

            save_stack(value, reg, output, config);
        }
        ValueKind::Store(store) => match config.func_data.dfg().value(store.value()).kind() {
            ValueKind::Aggregate(list) => {
                assert!(!store.dest().is_global());
                let values = local_unpack(config.func_data, list);
                let pos = config.symbol.get_stack_pointer(&store.dest());
                let tmp = config.table.get_vaccant().unwrap();
                for (index, value) in values.into_iter().enumerate() {
                    let reg = prepare_value(value, output, config);
                    let offset = pos + (index as i32) * 4;
                    if offset < 2048 {
                        writeln!(output, "  sw {reg}, {offset}(sp)").unwrap();
                    } else {
                        writeln!(output, "  li {tmp}, {offset}").unwrap();
                        writeln!(output, "  add {tmp}, {tmp}, sp").unwrap();
                        writeln!(output, "  sw {reg}, 0({tmp})").unwrap();
                    }
                    config.table.reset(reg).unwrap();
                }
                config.table.reset(tmp).unwrap();
            }
            _ => {
                let reg = prepare_value(store.value(), output, config);
                if store.dest().is_global() {
                    let value_data = config.program.borrow_value(store.dest());
                    let name = global_variable_name(&value_data).unwrap();
                    let tmp = config.table.get_vaccant().unwrap();
                    writeln!(output, "  la {tmp}, {name}").unwrap();
                    writeln!(output, "  sw {reg}, 0({tmp})").unwrap();
                    config.table.reset(tmp).unwrap();
                } else {
                    // let pos = config.symbol.get_stack_pointer(&store.dest());
                    let pos = prepare_value(store.dest(), output, config);
                    writeln!(output, "  sw {reg}, 0({pos})").unwrap();
                    config.table.reset(pos).unwrap();
                }
                config.table.reset(reg).unwrap();
            }
        },
        ValueKind::GetPtr(get) => {
            assert!(!get.src().is_global());
            let reg = prepare_value(get.src(), output, config);
            let s = match config.func_data.dfg().value(get.src()).ty().kind() {
                TypeKind::Pointer(ty) => ty.size(),
                _ => unreachable!(),
            };
            let stride = config.table.get_vaccant().unwrap();
            writeln!(output, "  li {stride}, {s}").unwrap();
            let index = prepare_value(get.index(), output, config);
            writeln!(output, "  mul {index}, {index}, {stride}").unwrap();
            writeln!(output, "  add {reg}, {reg}, {index}").unwrap();
            config.table.reset(stride).unwrap();
            config.table.reset(index).unwrap();
            save_stack(value, reg, output, config);
        }
        ValueKind::GetElemPtr(get) => {
            let (reg, s) = if get.src().is_global() {
                let tmp = config.table.get_vaccant().unwrap();
                let value_data = config.program.borrow_value(get.src());
                let name = global_variable_name(&value_data).unwrap();
                writeln!(output, "  la {tmp}, {name}").unwrap();
                let s = match config.program.borrow_value(get.src()).ty().kind() {
                    TypeKind::Pointer(array) => match array.kind() {
                        TypeKind::Array(elem, _) => elem.size(),
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                };
                (tmp, s)
            } else {
                let pos = prepare_value(get.src(), output, config);
                let s = match config.func_data.dfg().value(get.src()).ty().kind() {
                    TypeKind::Pointer(array) => match array.kind() {
                        TypeKind::Array(elem, _) => elem.size(),
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                };
                (pos, s)
            };
            let stride = config.table.get_vaccant().unwrap();
            writeln!(output, "  li {stride}, {s}").unwrap();
            let index = prepare_value(get.index(), output, config);
            writeln!(output, "  mul {index}, {index}, {stride}").unwrap();
            writeln!(output, "  add {reg}, {reg}, {index}").unwrap();
            config.table.reset(stride).unwrap();
            config.table.reset(index).unwrap();
            save_stack(value, reg, output, config);
        }

        ValueKind::Binary(bin) => {
            let left = prepare_value(bin.lhs(), output, config);
            let right = prepare_value(bin.rhs(), output, config);
            config.table.reset(left).unwrap();
            config.table.reset(right).unwrap();
            let res = config.table.get_vaccant().unwrap();

            match bin.op() {
                BinaryOp::Add => writeln!(output, "  add {res}, {left}, {right}"),
                BinaryOp::Sub => writeln!(output, "  sub {res}, {left}, {right}"),
                BinaryOp::Mul => writeln!(output, "  mul {res}, {left}, {right}"),
                BinaryOp::Div => writeln!(output, "  div {res}, {left}, {right}"),
                BinaryOp::Mod => writeln!(output, "  rem {res}, {left}, {right}"),
                BinaryOp::And => writeln!(output, "  and {res}, {left}, {right}"),
                BinaryOp::Or => writeln!(output, "  or {res}, {left}, {right}"),
                BinaryOp::Lt => writeln!(output, "  slt {res}, {left}, {right}"),
                BinaryOp::Le => {
                    writeln!(output, "  sgt {res}, {left}, {right}\n  seqz {res}, {left}")
                }
                BinaryOp::Gt => writeln!(output, "  sgt {res}, {left}, {right}"),
                BinaryOp::Ge => {
                    writeln!(output, "  slt {res}, {left}, {right}\n  seqz {res}, {left}")
                }
                BinaryOp::Eq => {
                    writeln!(output, "  xor {res}, {left}, {right}\n  seqz {res}, {left}")
                }
                BinaryOp::NotEq => {
                    writeln!(output, "  xor {res}, {left}, {right}\n  snez {res}, {left}")
                }
                _ => unimplemented!(),
            }
            .unwrap();

            save_stack(value, res, output, config);
        }
        ValueKind::Branch(branch) => {
            let cond = prepare_value(branch.cond(), output, config);
            let then_tag = block_name(config.func_data, branch.true_bb()).unwrap();
            let else_tag = block_name(config.func_data, branch.false_bb()).unwrap();
            writeln!(output, "  bnez {cond}, {then_tag}").unwrap();
            config.table.reset(cond).unwrap();
            writeln!(output, "  j {else_tag}").unwrap();
        }
        ValueKind::Jump(jump) => {
            let jmp_tag = block_name(config.func_data, jump.target()).unwrap();
            writeln!(output, "  j {jmp_tag}").unwrap();
        }
        ValueKind::Call(call) => {
            for (i, &arg) in call.args().iter().enumerate() {
                let reg = prepare_value(arg, output, config);
                if i < 8 {
                    writeln!(output, "  mv a{i}, {reg}").unwrap();
                } else {
                    writeln!(output, "  sw {reg}, {}(sp)", (i - 8) * 4).unwrap();
                }
                config.table.reset(reg).unwrap();
            }
            let callee = call.callee();
            let callee_data = config.program.func(callee);
            let func_name = function_name(callee_data).unwrap();
            writeln!(output, "  call {func_name}").unwrap();
            if !config.func_data.dfg().value(value).ty().is_unit() {
                save_stack(value, "a0".parse().unwrap(), output, config);
            }
        }
        ValueKind::Return(ret) => {
            match ret.value() {
                Some(val) => {
                    let reg = prepare_value(val, output, config);
                    writeln!(output, "  mv a0, {}", reg).unwrap();
                    config.table.reset(reg).unwrap();
                }
                None => {}
            };
            let stack_size = config.stack_size;

            if config.save_ra {
                if stack_size - 4 < 2048 {
                    writeln!(output, "  lw ra, {}(sp)", stack_size - 4).unwrap();
                } else {
                    writeln!(output, "  li t0, {stack_size}").unwrap();
                    writeln!(output, "  add t0, t0, sp").unwrap();
                    writeln!(output, "  lw ra, 0(t0)").unwrap();
                }
            }
            if stack_size > 0 {
                if stack_size < 2048 {
                    writeln!(output, "  addi sp, sp, {}", stack_size).unwrap();
                } else {
                    writeln!(output, "  li t0, {}", stack_size).unwrap();
                    writeln!(output, "  add sp, sp, t0").unwrap();
                }
            }
            writeln!(output, "  ret").unwrap();
        }
    }
}

fn prepare_value(
    value: Value,
    output: &mut impl io::Write,
    config: &mut TranslateConfig,
) -> Register {
    match config.symbol.get(&value) {
        Some(AllocPos::Reg(reg)) => *reg,
        Some(AllocPos::RegPointer(_)) => panic!("Register is not addressable"),
        Some(AllocPos::Stack(pos)) => {
            let pos = *pos;
            let reg = config.table.get_vaccant().unwrap();
            if pos < 2048 {
                writeln!(output, "  lw {reg}, {pos}(sp)").unwrap();
            } else {
                writeln!(output, "  li {reg}, {pos}").unwrap();
                writeln!(output, "  add {reg}, sp, {reg}").unwrap();
                writeln!(output, "  lw {reg}, 0({reg})").unwrap();
            }
            reg
        }
        Some(AllocPos::StackPointer(pos)) => {
            let pos = *pos;
            let reg = config.table.get_vaccant().unwrap();
            if pos < 2048 {
                writeln!(output, "  addi {reg}, sp, {pos}").unwrap();
            } else {
                writeln!(output, "  li {reg}, {pos}").unwrap();
                writeln!(output, "  add {reg}, sp, {reg}").unwrap();
            }
            reg
        }
        None => match config.func_data.dfg().value(value).kind() {
            ValueKind::Integer(int) => {
                if int.value() != 0 {
                    let reg = config.table.get_vaccant().unwrap();
                    writeln!(output, "  li {}, {}", reg, int.value()).unwrap();
                    reg
                } else {
                    "x0".parse().unwrap()
                }
            }
            ValueKind::FuncArgRef(arg) => {
                if arg.index() < 8 {
                    Register::new(10 + arg.index() as u8).unwrap()
                } else {
                    let reg = config.table.get_vaccant().unwrap();
                    let offset = config.stack_size + (arg.index() - 8) * 4;
                    writeln!(output, "  lw {reg}, {offset}(sp)").unwrap();
                    reg
                }
            }
            e => unimplemented!("{:#?}", e),
        },
    }
}

fn save_stack(
    value: Value,
    reg: Register,
    output: &mut impl io::Write,
    config: &mut TranslateConfig,
) {
    config.symbol.store_stack(value, *config.stack_pos);
    let pos = *config.stack_pos;
    if pos < 2048 {
        writeln!(output, "  sw {reg}, {pos}(sp)").unwrap();
    } else {
        let temp = config.table.get_vaccant().unwrap();
        writeln!(output, "  li {temp}, {pos}").unwrap();
        writeln!(output, "  add {temp}, sp, {temp}").unwrap();
        writeln!(output, "  sw {reg}, 0({temp})").unwrap();
        config.table.reset(temp).unwrap();
    }
    config.table.reset(reg).unwrap();
    *config.stack_pos += 4;
}

// Helper functions

/// Get the name of a basic block, without leading '%'
fn block_name(func: &FunctionData, bb: BasicBlock) -> Result<String, Error> {
    let name = func.dfg().bb(bb).name().as_ref();
    let name = name.ok_or(Error::InternalError(format!("Missing block name")))?;
    let name = name
        .strip_prefix("%koopa_builtin_")
        .ok_or(Error::InternalError(format!(
            "invalid function name '{name}' generated in koopa, expected to begin with '%koopa_builtin_'"
        )))?
        .to_string();
    Ok(name)
}

/// Get the name of a function, without leading '@'
fn function_name(func_data: &FunctionData) -> Result<String, Error> {
    Ok(func_data
        .name()
        .strip_prefix("@")
        .ok_or(Error::InternalError(format!(
            "invalid function name '{}' generated in koopa, expected to begin with '@'",
            func_data.name()
        )))?
        .to_string())
}

/// Get the name of a global variable, without leading '@'
fn global_variable_name(value_data: &ValueData) -> Result<String, Error> {
    let name = value_data
        .name()
        .as_ref()
        .ok_or(Error::InternalError(format!(
            "missing global variable name in koopa"
        )))?;
    Ok(name
        .strip_prefix("@")
        .ok_or(Error::InternalError(format!(
            "invalid function name '{}' generated in koopa, expected to begin with '@'",
            name
        )))?
        .to_string())
}

fn unpack(program: &Program, list: &Aggregate) -> Vec<Value> {
    let mut data = Vec::new();
    for &elem in list.elems() {
        match program.borrow_value(elem).kind() {
            ValueKind::Aggregate(sublist) => {
                data.extend(unpack(program, sublist));
            }
            ValueKind::Integer(_) => {
                data.push(elem.clone());
            }
            _ => unreachable!(),
        }
    }
    data
}

fn local_unpack(func: &FunctionData, list: &Aggregate) -> Vec<Value> {
    let mut data = Vec::new();
    for &elem in list.elems() {
        match func.dfg().value(elem).kind() {
            ValueKind::Aggregate(sublist) => {
                data.extend(local_unpack(func, sublist));
            }
            ValueKind::Integer(_) => {
                data.push(elem.clone());
            }
            _ => unreachable!(),
        }
    }
    data
}
