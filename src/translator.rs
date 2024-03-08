//! In this file, the conversion from Koopa to RISCV is provided.
//! Instead of implementing a `trait` as is described in the writeup,
//! the core of the file is implemented in the function [`translate_program`].

use crate::register::*;
use crate::util::Error;
use koopa::ir::{
    entities::ValueData, BasicBlock, BinaryOp, FunctionData, Program, Value, ValueKind,
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
                ValueKind::ZeroInit(_) => writeln!(output, "  .zero {}", size),
                ValueKind::Integer(i) => writeln!(output, "  .word {}", i.value()),
                _ => unimplemented!(),
            }
            .unwrap();
        }
        _ => panic!("internal error: expected global alloc instruction"),
    }

    writeln!(output, "").unwrap();
}

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
            let on_stack = !func_data.dfg().value(inst).ty().is_unit();
            translate_instruction(inst, output, &mut config, on_stack);
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
                local += 4;
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
        writeln!(output, "  addi sp, sp, -{}", stack_size).unwrap();
    }
    if ra != 0 {
        writeln!(output, "  sw ra, {}(sp)", stack_size - 4).unwrap();
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

/// Note that `sizeof(Value) = 0x4`, so instead of borrowing it,
/// I choose to pass it by copying directly.
fn translate_instruction(
    value: Value,
    output: &mut impl io::Write,
    config: &mut TranslateConfig,
    on_stack: bool,
) {
    // Where the result is stored
    let reg = match config.func_data.dfg().value(value).kind() {
        ValueKind::Integer(int) => {
            if int.value() != 0 {
                let reg = config.table.get_vaccant().unwrap();
                writeln!(output, "  li {}, {}", reg, int.value()).unwrap();
                Some(reg)
            } else {
                Some(Register::new(0).unwrap())
            }
        }

        ValueKind::Return(ret) => {
            match ret.value() {
                Some(val) => {
                    let reg = translate_i32(val, output, config);
                    if reg.id != 10 {
                        writeln!(output, "  mv a0, {}", reg).unwrap();
                    }
                }
                None => {}
            };
            if config.save_ra {
                writeln!(output, "  lw ra, {}(sp)", config.stack_size - 4).unwrap();
            }
            if config.stack_size > 0 {
                writeln!(output, "  addi sp, sp, {}", config.stack_size).unwrap();
            }
            writeln!(output, "  ret").unwrap();
            None
        }

        ValueKind::Binary(bin) => {
            let left = translate_i32(bin.lhs(), output, config);
            let right = translate_i32(bin.rhs(), output, config);
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
                BinaryOp::Le => writeln!(output, "  sgt {res}, {right}, {left}"),
                BinaryOp::Gt => writeln!(output, "  sgt {res}, {left}, {right}"),
                BinaryOp::Ge => writeln!(output, "  slt {res}, {right}, {left}"),
                BinaryOp::Eq => {
                    writeln!(output, "  xor {res}, {left}, {right}\n  seqz {res}, {left}")
                }
                BinaryOp::NotEq => {
                    writeln!(output, "  xor {res}, {left}, {right}\n  snez {res}, {left}")
                }
                _ => unimplemented!(),
            }
            .unwrap();

            Some(res)
        }

        ValueKind::Alloc(_alloc) => {
            writeln!(output, "  # Allocated at {}(sp)", *config.stack_pos).unwrap();
            config
                .symbol
                .store_stack_pointer(value, *config.stack_pos)
                .unwrap();
            *config.stack_pos += 4;
            return;
        }

        ValueKind::Store(store) => {
            let reg = translate_i32(store.value(), output, config);
            if store.dest().is_global() {
                let value_data = config.program.borrow_value(store.dest());
                let name = global_variable_name(&value_data).unwrap();
                let tmp = config.table.get_vaccant().unwrap();
                writeln!(output, "  la {tmp}, {name}").unwrap();
                writeln!(output, "  sw {reg}, 0({tmp})").unwrap();
                config.table.reset(tmp).unwrap();
            } else {
                let pos = config.symbol.get_stack_pointer(&store.dest()).unwrap();
                writeln!(output, "  sw {reg}, {pos}(sp)").unwrap();
            }
            config.table.reset(reg).unwrap();

            None
        }

        ValueKind::Load(load) => {
            let reg = config.table.get_vaccant().unwrap();
            if load.src().is_global() {
                let value_data = config.program.borrow_value(load.src());
                let name = global_variable_name(&value_data).unwrap();
                writeln!(output, "  la {reg}, {name}").unwrap();
                writeln!(output, "  lw {reg}, 0({reg})").unwrap();
            } else {
                let pos = config.symbol.get_stack_pointer(&load.src()).unwrap();
                writeln!(output, "  lw {reg}, {pos}(sp)").unwrap();
            }

            Some(reg)
        }

        ValueKind::Branch(branch) => {
            let cond = translate_i32(branch.cond(), output, config);
            let then_tag = block_name(config.func_data, branch.true_bb()).unwrap();
            let else_tag = block_name(config.func_data, branch.false_bb()).unwrap();
            writeln!(output, "  bnez {cond}, {then_tag}").unwrap();
            config.table.reset(cond).unwrap();
            writeln!(output, "  j {else_tag}").unwrap();
            None
        }

        ValueKind::Jump(jump) => {
            let jmp_tag = block_name(config.func_data, jump.target()).unwrap();
            writeln!(output, "  j {jmp_tag}").unwrap();
            None
        }

        ValueKind::Call(call) => {
            for (i, &arg) in call.args().iter().enumerate() {
                let reg = translate_i32(arg, output, config);
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
            Some("a0".parse().unwrap())
        }

        ValueKind::FuncArgRef(arg) => {
            if arg.index() < 8 {
                Some(Register::new(10 + arg.index() as u8).unwrap())
            } else {
                let offset = config.stack_size + (arg.index() - 8) * 4;
                config.symbol.store_stack(value, offset as i32).unwrap();
                return;
            }
        }

        kind => unimplemented!("unimplemented ValueKind {kind:#?}"),
    };

    if !config.func_data.dfg().value(value).ty().is_unit() {
        let reg = reg
            .ok_or(Error::InternalError(format!(
                "{:#?} is non-void type",
                config.func_data.dfg().value(value)
            )))
            .unwrap();

        if on_stack {
            config.symbol.store_stack(value, *config.stack_pos).unwrap();
            config.table.reset(reg).unwrap();
            writeln!(output, "  sw {}, {}(sp)", reg, config.stack_pos).unwrap();
            *config.stack_pos += 4;
        } else {
            config.symbol.store_register(value, reg).unwrap();
        }
    }
}

fn translate_i32(
    value: Value,
    output: &mut impl io::Write,
    config: &mut TranslateConfig,
) -> Register {
    // If value is already handled before, just return it directly.
    // Note that we need to load variables on stack into register.
    let value_data = config.func_data.dfg().value(value);
    if !value_data.ty().is_i32() {
        panic!(
            "internal error: type {} cannot be parsed as i32",
            value_data.ty()
        );
    }
    match config.symbol.get(&value) {
        Some(AllocPos::Reg(reg)) => *reg,
        Some(AllocPos::Stack(offset)) => {
            let reg = config.table.get_vaccant().unwrap();
            writeln!(output, "  lw {reg}, {offset}(sp)").unwrap();
            reg
        }
        Some(AllocPos::StackPointer(_)) => panic!("internal error: *i32 cannot be parsed as i32"),
        None => {
            translate_instruction(value, output, config, false);
            translate_i32(value, output, config)
        }
    }
}

// Helper functions

/// Get the name of a basic block, without leading '%'
fn block_name(func: &FunctionData, bb: BasicBlock) -> Result<String, Error> {
    let name = func.dfg().bb(bb).name().as_ref();
    let name = name.ok_or(Error::InternalError(format!("Missing block name")))?;
    let name = name
        .strip_prefix("%")
        .ok_or(Error::InternalError(format!(
            "invalid function name '{name}' generated in koopa, expected to begin with '%'"
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
