//! In this file, the conversion from Koopa to RISCV is provided.
//! Instead of implementing a `trait` as is described in the writeup,
//! the core of the file is implemented in the function `translate_program`.

use crate::util::Error;
use koopa::ir::{dfg::DataFlowGraph, FunctionData, Program, Value, ValueKind};
use std::io;

pub fn translate_program(program: &Program, output: &mut impl io::Write) -> Result<(), Error> {
    // Functions
    output.write(b"  .text\n").map_err(Error::InvalidFile)?;
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
        .ok_or(Error::InvalidFunctionName)?;
    let dfg = func_data.dfg();
    output
        .write(format!("  .global {}\n{}:\n", func_name, func_name).as_bytes())
        .map_err(Error::InvalidFile)?;
    for (&_bb, node) in func_data.layout().bbs() {
        for &inst in node.insts().keys() {
            translate_value(inst, dfg, output)?;
        }
    }
    Ok(())
}

/// Note that `sizeof(Value) = 0x4`, so instead of borrowing it,
/// I choose to pass it by copying directly.
fn translate_value(
    value: Value,
    dfg: &DataFlowGraph,
    output: &mut impl io::Write,
) -> Result<(), Error> {
    let value_data = dfg.value(value);
    match value_data.kind() {
        ValueKind::Integer(int) => {
            output
                .write(format!("  li a0, {}\n", int.value()).as_bytes())
                .map_err(Error::InvalidFile)?;
        }
        ValueKind::Return(ret) => {
            match ret.value() {
                Some(val) => {
                    translate_value(val, dfg, output)?;
                }
                None => {}
            }
            output.write(b"  ret\n").map_err(Error::InvalidFile)?;
        }
        _ => unreachable!(),
    }
    Ok(())
}
