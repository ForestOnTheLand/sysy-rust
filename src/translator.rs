//! In this file, the conversion from Koopa to RISCV is provided.
//! Instead of implementing a `trait` as is described in the writeup,
//! the core of the file is implemented in the function [`translate_program`].

use crate::register::*;
use crate::util::Error;
use koopa::ir::{BasicBlock, BinaryOp, FunctionData, Program, Value, ValueKind};
use std::io;

fn block_tag(func: &FunctionData, bb: BasicBlock) -> Result<String, Error> {
    let name = func.dfg().bb(bb).name().as_ref();
    let name = name.ok_or(Error::InternalError(format!("Missing block name")))?;
    let name = name
        .strip_prefix("%")
        .ok_or(Error::InternalError(format!(
            "invalid function name '{name}'  generated in koopa, expected to begin with '%'"
        )))?
        .to_string();
    Ok(name)
}

pub fn translate_program(program: &Program, output: &mut impl io::Write) {
    // Functions
    writeln!(output, "  .text").unwrap();
    for &func in program.func_layout() {
        let func_data = program.func(func);
        translate_function(func_data, output);
    }
}

fn translate_function(func_data: &FunctionData, output: &mut impl io::Write) {
    let func_name = func_data
        .name()
        .strip_prefix("@")
        .ok_or(Error::InternalError(format!(
            "invalid function name '{}' generated in koopa, expected to begin with '@'",
            func_data.name()
        )))
        .unwrap();
    writeln!(output, "  .global {}\n{}:", func_name, func_name).unwrap();

    let stack_size = get_stack_size(func_data);

    if stack_size > 0 {
        writeln!(output, "  addi sp, sp, -{}", stack_size).unwrap();
    }

    let mut config = TranslateConfig {
        func_data,
        table: RegisterTable::new(),
        symbol: AllocTable::new(),
        stack_size,
        stack_pos: Box::new(0),
    };

    for (&bb, node) in func_data.layout().bbs() {
        let name = block_tag(func_data, bb).unwrap();
        if name != "entry" {
            writeln!(output, "{name}:").map_err(Error::IOError).unwrap();
        }
        for &inst in node.insts().keys() {
            let on_stack = !func_data.dfg().value(inst).ty().is_unit() && config.table.remain() < 3;
            translate_instruction(inst, output, &mut config, on_stack);
            writeln!(output, "").map_err(Error::IOError).unwrap();
        }
    }
}

fn get_stack_size(func_data: &FunctionData) -> i32 {
    let mut counter = 0;
    for (_bb, node) in func_data.layout().bbs() {
        for &inst in node.insts().keys() {
            if !func_data.dfg().value(inst).ty().is_unit() {
                counter += 4;
            }
        }
    }

    // Note that we need to align the size as a multiple of 16
    (counter + 15) & !0xf
}

struct TranslateConfig<'a> {
    func_data: &'a FunctionData,
    table: RegisterTable,
    symbol: AllocTable,
    stack_size: i32,
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
    // If value is already handled before, just return it directly.
    // Note that we need to load variables on stack into register.
    writeln!(
        output,
        "  # {:?}",
        config.func_data.dfg().value(value).kind()
    )
    .unwrap();
    // Where the result is stored
    let reg = match config.func_data.dfg().value(value).kind() {
        ValueKind::Integer(int) => {
            let reg = config.table.get_vaccant().unwrap();
            writeln!(output, "  li {}, {}", reg, int.value()).unwrap();
            Some(reg)
        }

        ValueKind::Return(ret) => {
            match ret.value() {
                Some(val) => {
                    let reg = translate_value(val, output, config);
                    if reg.id != 10 {
                        writeln!(output, "  mv a0, {}", reg).unwrap();
                    }
                }
                None => {}
            };
            if config.stack_size > 0 {
                writeln!(output, "  addi sp, sp, {}", config.stack_size).unwrap();
            }
            writeln!(output, "  ret").unwrap();
            None
        }

        ValueKind::Binary(bin) => {
            let left = translate_value(bin.lhs(), output, config);
            let right = translate_value(bin.rhs(), output, config);
            println!("{left}, {right}");
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
            config.symbol.store_stack(value, *config.stack_pos).unwrap();
            writeln!(output, "  # alloc at {}(sp)", config.stack_pos).unwrap();
            *config.stack_pos += 4;
            return;
        }

        ValueKind::Store(store) => {
            let reg = translate_value(store.value(), output, config);
            let pos = config.symbol.get_stack(&store.dest()).unwrap();
            writeln!(output, "  sw {reg}, {pos}(sp)").unwrap();
            config.table.reset(reg).unwrap();

            None
        }

        ValueKind::Load(load) => {
            let reg = config.table.get_vaccant().unwrap();
            let pos = config.symbol.get_stack(&load.src()).unwrap();
            writeln!(output, "  lw {reg}, {pos}(sp)").unwrap();

            Some(reg)
        }

        ValueKind::Branch(branch) => {
            let cond = translate_value(branch.cond(), output, config);
            let then_tag = block_tag(config.func_data, branch.true_bb()).unwrap();
            let else_tag = block_tag(config.func_data, branch.false_bb()).unwrap();
            writeln!(output, "  bnez {cond}, {then_tag}").unwrap();
            config.table.reset(cond).unwrap();
            writeln!(output, "  j {else_tag}").unwrap();
            None
        }

        ValueKind::Jump(jump) => {
            let jmp_tag = block_tag(config.func_data, jump.target()).unwrap();
            writeln!(output, "  j {jmp_tag}").unwrap();
            None
        }

        _ => unimplemented!(),
    };

    if config.func_data.dfg().value(value).ty().is_unit() {
        writeln!(output, "  # void type").unwrap();
    } else {
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
            writeln!(output, "  # alloc at {}(sp)", config.stack_pos).unwrap();
            *config.stack_pos += 4;
        } else {
            writeln!(output, "  # alloc at {}", reg).unwrap();
            config.symbol.store_register(value, reg).unwrap();
        }
    }
}

fn translate_value(
    value: Value,
    output: &mut impl io::Write,
    config: &mut TranslateConfig,
) -> Register {
    writeln!(output, "  # Translate Value").unwrap();
    match config.symbol.get(&value) {
        Some(AllocPos::Reg(reg)) => *reg,
        Some(AllocPos::Stack(offset)) => {
            let reg = config.table.get_vaccant().unwrap();
            writeln!(output, "  lw {reg}, {offset}(sp)").unwrap();
            reg
        }
        None => {
            translate_instruction(value, output, config, false);
            translate_value(value, output, config)
        }
    }
}
