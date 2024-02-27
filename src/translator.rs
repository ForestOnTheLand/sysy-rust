//! In this file, the conversion from Koopa to RISCV is provided.
//! Instead of implementing a `trait` as is described in the writeup,
//! the core of the file is implemented in the function `translate_program`.

use crate::register::*;
use crate::util::Error;
use koopa::ir::{dfg::DataFlowGraph, BinaryOp, FunctionData, Program, Value, ValueKind};
use std::{collections::HashMap, io};

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
    let dfg = func_data.dfg();
    writeln!(output, "  .global {}\n{}:", func_name, func_name).map_err(Error::IOError)?;

    let mut table = RegisterTable::new();
    let mut symbol: HashMap<Value, Register> = HashMap::new();

    for (&_bb, node) in func_data.layout().bbs() {
        for &inst in node.insts().keys() {
            translate_value(inst, dfg, output, &mut table, &mut symbol)?;
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
    table: &mut RegisterTable,
    symbol: &mut HashMap<Value, Register>,
) -> Result<Register, Error> {
    // If value is already handled before, just return it directly.
    if let Some(reg) = symbol.get(&value) {
        return Ok(*reg);
    }

    // Where the result is stored
    let result = match dfg.value(value).kind() {
        ValueKind::Integer(int) => {
            if int.value() == 0 {
                Register::new(0)?
            } else {
                let reg = table.get_vaccant()?;
                writeln!(output, "  li {}, {}", reg, int.value()).map_err(Error::IOError)?;
                reg
            }
        }

        ValueKind::Return(ret) => {
            match ret.value() {
                Some(val) => {
                    let reg = match symbol.get(&val) {
                        Some(r) => r,
                        None => {
                            translate_value(val, dfg, output, table, symbol)?;
                            symbol.get(&val).unwrap()
                        }
                    };
                    if reg.id != 0 {
                        writeln!(output, "  mv a0, {}", reg).map_err(Error::IOError)?;
                    }
                }
                None => {}
            }
            writeln!(output, "  ret").map_err(Error::IOError)?;
            Register::new(10)?
        }

        ValueKind::Binary(bin) => {
            let left = translate_value(bin.lhs(), dfg, output, table, symbol)?;
            let right = translate_value(bin.rhs(), dfg, output, table, symbol)?;
            table.reset(left)?;
            table.reset(right)?;
            let res = table.get_vaccant()?;

            match bin.op() {
                // + -
                BinaryOp::Add => {
                    writeln!(output, "  add {}, {}, {}", res, left, right)
                }
                BinaryOp::Sub => {
                    writeln!(output, "  sub {}, {}, {}", res, left, right)
                }
                // * / %
                BinaryOp::Mul => {
                    writeln!(output, "  mul {}, {}, {}", res, left, right)
                }
                BinaryOp::Div => {
                    writeln!(output, "  div {}, {}, {}", res, left, right)
                }
                BinaryOp::Mod => {
                    writeln!(output, "  rem {}, {}, {}", res, left, right)
                }
                // &
                BinaryOp::And => {
                    writeln!(output, "  and {}, {}, {}", res, left, right)
                }
                // |
                BinaryOp::Or => {
                    writeln!(output, "  or {}, {}, {}", res, left, right)
                }
                // < <= > >=
                BinaryOp::Lt => {
                    writeln!(output, "  slt {}, {}, {}", res, left, right)
                }
                BinaryOp::Le => {
                    writeln!(output, "  sgt {}, {}, {}", res, right, left)
                }
                BinaryOp::Gt => {
                    writeln!(output, "  sgt {}, {}, {}", res, left, right)
                }
                BinaryOp::Ge => {
                    writeln!(output, "  slt {}, {}, {}", res, right, left)
                }
                // == !=
                BinaryOp::Eq => {
                    writeln!(output, "  xor {res}, {left}, {right}\n  seqz {res}. {left}")
                }
                BinaryOp::NotEq => {
                    writeln!(output, "  xor {res}, {left}, {right}\n  snez {res}, {left}")
                }
                _ => unimplemented!(),
            }
            .map_err(Error::IOError)?;

            res
        }

        _ => unimplemented!(),
    };

    // And insert it after getting `result`
    symbol.insert(value, result);

    Ok(result)
}
