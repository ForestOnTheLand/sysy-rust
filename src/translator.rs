//! In this file, the conversion from Koopa to RISCV is provided.
//! Instead of implementing a `trait` as is described in the writeup,
//! the core of the file is implemented in the function `translate_program`.

use crate::register::*;
use crate::util::Error;
use koopa::ir::{BinaryOp, FunctionData, Program, Value, ValueKind};
use std::io;

pub fn translate_program(program: &Program, output: &mut impl io::Write) -> Result<(), Error> {
    // Functions
    writeln!(output, "  .text").map_err(Error::IOError)?;
    for &func in program.func_layout() {
        let func_data = program.func(func);
        translate_function(func_data, output)?;
    }
    Ok(())
}

fn translate_function(func_data: &FunctionData, output: &mut impl io::Write) -> Result<(), Error> {
    let func_name = func_data
        .name()
        .strip_prefix("@")
        .ok_or(Error::InternalError(format!(
            "invalid function name '{}' generated in koopa, expected to begin with '@'",
            func_data.name()
        )))?;
    writeln!(output, "  .global {}\n{}:", func_name, func_name).map_err(Error::IOError)?;

    let stack_size = get_stack_size(func_data);

    if stack_size > 0 {
        writeln!(output, "  addi sp, sp, -{}", stack_size).map_err(Error::IOError)?;
    }

    let mut config = TranslateConfig {
        func_data,
        table: RegisterTable::new(),
        symbol: AllocTable::new(),
        stack_size,
        stack_pos: Box::new(0),
    };

    for (_bb, node) in func_data.layout().bbs() {
        for &inst in node.insts().keys() {
            let on_stack = !func_data.dfg().value(inst).ty().is_unit() && config.table.remain() < 3;
            translate_instruction(inst, output, &mut config, on_stack)?;
            writeln!(output, "").map_err(Error::IOError)?;
        }
    }
    Ok(())
}

fn get_stack_size(func_data: &FunctionData) -> i32 {
    let mut counter = 0;
    for (_bb, node) in func_data.layout().bbs() {
        for &inst in node.insts().keys() {
            println!("{:#?}", inst);
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
) -> Result<(), Error> {
    // If value is already handled before, just return it directly.
    // Note that we need to load variables on stack into register.
    writeln!(
        output,
        "  # {:?}",
        config.func_data.dfg().value(value).kind()
    )
    .map_err(Error::IOError)?;
    // Where the result is stored
    let reg = match config.func_data.dfg().value(value).kind() {
        ValueKind::Integer(int) => {
            let reg = config.table.get_vaccant()?;
            writeln!(output, "  li {}, {}", reg, int.value()).map_err(Error::IOError)?;
            Some(reg)
        }

        ValueKind::Return(ret) => {
            match ret.value() {
                Some(val) => {
                    let reg = translate_value(val, output, config)?;
                    if reg.id != 10 {
                        writeln!(output, "  mv a0, {}", reg).map_err(Error::IOError)?;
                    }
                }
                None => {}
            };
            if config.stack_size > 0 {
                writeln!(output, "  addi sp, sp, {}", config.stack_size).map_err(Error::IOError)?;
            }
            writeln!(output, "  ret").map_err(Error::IOError)?;
            None
        }

        ValueKind::Binary(bin) => {
            let left = translate_value(bin.lhs(), output, config)?;
            let right = translate_value(bin.rhs(), output, config)?;
            config.table.reset(left)?;
            config.table.reset(right)?;
            let res = config.table.get_vaccant()?;

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
                    writeln!(output, "  xor {res}, {left}, {right}\n  seqz {res}. {left}")
                }
                BinaryOp::NotEq => {
                    writeln!(output, "  xor {res}, {left}, {right}\n  snez {res}, {left}")
                }
                _ => unimplemented!(),
            }
            .map_err(Error::IOError)?;

            Some(res)
        }

        ValueKind::Alloc(_alloc) => {
            config.symbol.store_stack(value, *config.stack_pos)?;
            writeln!(output, "  # alloc at {}(sp)", config.stack_pos).map_err(Error::IOError)?;
            *config.stack_pos += 4;
            return Ok(());
        }

        ValueKind::Store(store) => {
            let reg = translate_value(store.value(), output, config)?;
            let pos = config.symbol.get_stack(&store.dest()).unwrap();
            writeln!(output, "  sw {reg}, {pos}(sp)").map_err(Error::IOError)?;
            config.table.reset(reg)?;

            None
        }

        ValueKind::Load(load) => {
            let reg = config.table.get_vaccant()?;
            let pos = config.symbol.get_stack(&load.src()).unwrap();
            writeln!(output, "  lw {reg}, {pos}(sp)").map_err(Error::IOError)?;

            Some(reg)
        }

        _ => unimplemented!(),
    };

    // And insert it after getting `result`
    // if let Some(reg) = result {
    //     config.symbol.store_register(value, reg)?;
    // }

    if config.func_data.dfg().value(value).ty().is_unit() {
        writeln!(output, "  # void type").map_err(Error::IOError)?;
        return Ok(());
    }

    let reg = reg.ok_or(Error::InternalError("???".to_string()))?;

    if on_stack {
        config.symbol.store_stack(value, *config.stack_pos)?;
        config.table.reset(reg)?;
        writeln!(output, "  sw {}, {}(sp)", reg, config.stack_pos).map_err(Error::IOError)?;
        writeln!(output, "  # alloc at {}(sp)", config.stack_pos).map_err(Error::IOError)?;
        *config.stack_pos += 4;
    } else {
        writeln!(output, "  # alloc at {}", reg).map_err(Error::IOError)?;
        config.symbol.store_register(value, reg)?;
    }

    Ok(())
}

fn translate_value(
    value: Value,
    output: &mut impl io::Write,
    config: &mut TranslateConfig,
) -> Result<Register, Error> {
    match config.symbol.get(&value) {
        Some(AllocPos::Reg(reg)) => Ok(*reg),
        Some(AllocPos::Stack(offset)) => {
            let reg = config.table.get_vaccant()?;
            writeln!(output, "  lw {reg}, {offset}(sp)").map_err(Error::IOError)?;
            Ok(reg)
        }
        None => {
            translate_instruction(value, output, config, false)?;
            translate_value(value, output, config)
        }
    }
}
