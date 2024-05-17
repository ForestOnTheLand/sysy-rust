//! In this file, the conversion from Koopa to RISCV is provided.
//! Instead of implementing a `trait` as is described in the writeup,
//! the core of the file is implemented in the function [`translate_program`].

use crate::lifetime::Allocator;
use crate::lifetime::LifeTime;
use crate::riscv::*;
use crate::translate_util::*;
use koopa::ir::{
    entities::ValueData, values::Aggregate, BasicBlock, BinaryOp, FunctionData, Program, TypeKind,
    Value, ValueKind,
};
use std::{cmp::max, io};

pub fn output_program(program: &RiscvProgram, output: impl io::Write) {
    let mut output = output;
    writeln!(output, "{}", program).unwrap();
}

/// The core part of this module. Translate a KoopaIR program `program` into RISCV
/// assembly, which is written into the `output`.
///
/// # Panic
///
/// When encountering invalid KoopaIR program or unimplemented functions
///
pub fn translate_program(program: &Program) -> RiscvProgram {
    let mut code = RiscvProgram::new();
    // Global variables
    for &value in program.inst_layout() {
        translate_global_value(program, value, &mut code);
    }

    // Functions
    for &func in program.func_layout() {
        let func_data = program.func(func);
        translate_function(program, func_data, &mut code);
    }
    code
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
fn translate_global_value(program: &Program, value: Value, code: &mut RiscvProgram) {
    let value_data = program.borrow_value(value);
    let name = global_variable_name(&value_data).unwrap();

    let init = match value_data.kind() {
        ValueKind::GlobalAlloc(alloc) => {
            let init_data = program.borrow_value(alloc.init());
            let size = init_data.ty().size();
            match init_data.kind() {
                ValueKind::ZeroInit(_) => vec![0; size / 4],
                ValueKind::Integer(i) => vec![i.value()],
                ValueKind::Aggregate(list) => global_unpack(program, list),
                _ => unreachable!(),
            }
        }
        _ => panic!("internal error: expected global alloc instruction"),
    };
    code.values.push(RiscvValue { name, init });
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
fn translate_function(program: &Program, func_data: &FunctionData, code: &mut RiscvProgram) {
    // Ignore function declarations
    if func_data.layout().entry_bb() == None {
        return;
    }

    let func_name = function_name(func_data).unwrap();
    let lifetime = LifeTime::new(func_data);

    let allocator = Allocator::linear_scan_register_allocation(lifetime);

    let stack_layout = allocate_stack(func_data, &allocator);

    let mut function = RiscvFunction {
        stack_layout: stack_layout.clone(),
        blocks: Vec::new(),
    };

    let mut config = TranslateConfig {
        program,
        func_data,
        table: RegGroup::new_temp(),
        symbol: AllocTable::new(),
        stack_layout: stack_layout.clone(),
        stack_pos: Box::new(stack_layout.args as i32 * 4),
        allocator,
    };

    for (&bb, node) in func_data.layout().bbs() {
        let mut name = block_name(func_data, bb).unwrap();
        if name.starts_with("entry") {
            name = func_name.clone();
        }
        let mut block = RiscvBlock {
            name,
            instructions: Vec::new(),
        };
        for &inst in node.insts().keys() {
            translate_instruction(inst, &mut block.instructions, &mut config);
        }
        function.blocks.push(block);
    }

    code.functions.push(function);
}

/// Getting the stack size needed for a given function.
/// The algorithm is described in detail in
/// [writeup](https://pku-minic.github.io/online-doc/#/lv8-func-n-global/func-def-n-call?id=生成代码).
///
/// Basically, there are 3 different usage of stack space:
/// - space for local variable
/// - space for saving RISCV register `ra` (short for "return address")
/// - space for function arguments
fn allocate_stack(func_data: &FunctionData, allocator: &Allocator) -> StackLayout {
    let mut local = 0;
    let mut args = 0;
    let mut save = 0;
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
                if call.args().len() > 8 {
                    args = max(args, call.args().len() - 8);
                }
                save = max(
                    save,
                    1 + allocator.get_occupied_registers(inst, call.args()).len(),
                );
            }
        }
    }
    let total = local + args * 4 + save * 4;
    let stack_size = (total + 15) & !0xf;
    StackLayout {
        total: stack_size,
        args,
        save,
    }
}

struct TranslateConfig<'a> {
    program: &'a Program,
    func_data: &'a FunctionData,
    table: RegGroup,
    symbol: AllocTable,
    stack_layout: StackLayout,
    stack_pos: Box<i32>,
    allocator: Allocator,
}

/// Translate a single KoopaIR instruction into several RISCV instructions.
fn translate_instruction(
    value: Value,
    insts: &mut Vec<RiscvInstruction>,
    config: &mut TranslateConfig,
) {
    match config.allocator.allocation.get(&value) {
        Some(r) => insts.push(RiscvInstruction::Comment(format!(
            "{}: value in {r}, lifetime {:?}",
            config.allocator.lifetime.index.get(&value).unwrap(),
            config.allocator.lifetime.interval.get(&value).unwrap()
        ))),
        None => insts.push(RiscvInstruction::Comment(format!("value on stack"))),
    }
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
            insts.push(RiscvInstruction::Comment(format!(
                "alloc at {}(sp), size {}",
                *config.stack_pos, size
            )));
            config.symbol.store_stack_pointer(value, *config.stack_pos);
            *config.stack_pos += size as i32;
        }
        ValueKind::GlobalAlloc(_) => unreachable!(),
        ValueKind::Load(load) => {
            let expected = config.allocator.allocation.get(&value).cloned();
            let reg = if load.src().is_global() {
                let reg = expected.unwrap_or_else(|| config.table.get_vaccant());
                let value_data = config.program.borrow_value(load.src());
                let name = global_variable_name(&value_data).unwrap();
                insts.push(RiscvInstruction::La(reg, name));
                insts.push(RiscvInstruction::Lw(reg, 0, reg));
                reg
            } else {
                let reg = prepare_value(load.src(), insts, config, expected);
                insts.push(RiscvInstruction::Lw(reg, 0, reg));
                reg
            };

            save_value(value, reg, insts, config);
        }
        ValueKind::Store(store) => match config.func_data.dfg().value(store.value()).kind() {
            ValueKind::Aggregate(list) => {
                assert!(!store.dest().is_global());
                let values = local_unpack(config.func_data, list);
                let pos = config.symbol.get_stack_pointer(&store.dest());
                for (index, value) in values.into_iter().enumerate() {
                    let reg = prepare_value(value, insts, config, None);
                    let offset = pos + (index as i32) * 4;
                    insts.push(RiscvInstruction::Sw(reg, offset, Register::SP));
                    config.table.reset(reg);
                }
            }
            _ => {
                let reg = prepare_value(store.value(), insts, config, None);
                if store.dest().is_global() {
                    let value_data = config.program.borrow_value(store.dest());
                    let name = global_variable_name(&value_data).unwrap();
                    let tmp = config.table.get_vaccant();
                    insts.push(RiscvInstruction::La(tmp, name));
                    insts.push(RiscvInstruction::Sw(reg, 0, tmp));
                    config.table.reset(tmp);
                } else {
                    let pos = prepare_value(store.dest(), insts, config, None);
                    insts.push(RiscvInstruction::Sw(reg, 0, pos));
                    config.table.reset(pos);
                }
                config.table.reset(reg);
            }
        },
        ValueKind::GetPtr(get) => {
            assert!(!get.src().is_global());
            let reg = prepare_value(get.src(), insts, config, None);
            let s = match config.func_data.dfg().value(get.src()).ty().kind() {
                TypeKind::Pointer(ty) => ty.size(),
                _ => unreachable!(),
            };
            let index = prepare_value(get.index(), insts, config, None);
            insts.push(RiscvInstruction::Muli(index, index, s as i32));
            insts.push(RiscvInstruction::Add(reg, reg, index));
            config.table.reset(index);
            save_value(value, reg, insts, config);
        }
        ValueKind::GetElemPtr(get) => {
            let (reg, s) = if get.src().is_global() {
                let tmp = config.table.get_vaccant();
                let value_data = config.program.borrow_value(get.src());
                let name = global_variable_name(&value_data).unwrap();
                insts.push(RiscvInstruction::La(tmp, name));
                let s = match config.program.borrow_value(get.src()).ty().kind() {
                    TypeKind::Pointer(array) => match array.kind() {
                        TypeKind::Array(elem, _) => elem.size(),
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                };
                (tmp, s)
            } else {
                let pos = prepare_value(get.src(), insts, config, None);
                let s = match config.func_data.dfg().value(get.src()).ty().kind() {
                    TypeKind::Pointer(array) => match array.kind() {
                        TypeKind::Array(elem, _) => elem.size(),
                        _ => unreachable!(),
                    },
                    _ => unreachable!(),
                };
                (pos, s)
            };
            let index = prepare_value(get.index(), insts, config, None);
            insts.push(RiscvInstruction::Muli(index, index, s as i32));
            insts.push(RiscvInstruction::Add(reg, reg, index));
            config.table.reset(index);
            save_value(value, reg, insts, config);
        }

        ValueKind::Binary(bin) => {
            let left = prepare_value(bin.lhs(), insts, config, None);
            let right = prepare_value(bin.rhs(), insts, config, None);
            config.table.reset(left);
            config.table.reset(right);
            let expected = config.allocator.allocation.get(&value).cloned();
            let res = expected.unwrap_or_else(|| config.table.get_vaccant());

            match bin.op() {
                BinaryOp::Add => insts.push(RiscvInstruction::Add(res, left, right)),
                BinaryOp::Sub => insts.push(RiscvInstruction::Sub(res, left, right)),
                BinaryOp::Mul => insts.push(RiscvInstruction::Mul(res, left, right)),
                BinaryOp::Div => insts.push(RiscvInstruction::Div(res, left, right)),
                BinaryOp::Mod => insts.push(RiscvInstruction::Rem(res, left, right)),
                BinaryOp::And => insts.push(RiscvInstruction::And(res, left, right)),
                BinaryOp::Or => insts.push(RiscvInstruction::Or(res, left, right)),
                BinaryOp::Lt => insts.push(RiscvInstruction::Slt(res, left, right)),
                BinaryOp::Le => {
                    insts.push(RiscvInstruction::Sgt(res, left, right));
                    insts.push(RiscvInstruction::Seqz(res, res));
                }
                BinaryOp::Gt => insts.push(RiscvInstruction::Sgt(res, left, right)),
                BinaryOp::Ge => {
                    insts.push(RiscvInstruction::Slt(res, left, right));
                    insts.push(RiscvInstruction::Seqz(res, res));
                }
                BinaryOp::Eq => {
                    insts.push(RiscvInstruction::Xor(res, left, right));
                    insts.push(RiscvInstruction::Seqz(res, res));
                }
                BinaryOp::NotEq => {
                    insts.push(RiscvInstruction::Xor(res, left, right));
                    insts.push(RiscvInstruction::Snez(res, res));
                }
                _ => unimplemented!(),
            };

            save_value(value, res, insts, config);
        }
        ValueKind::Branch(branch) => {
            let cond = prepare_value(branch.cond(), insts, config, None);
            let then_tag = block_name(config.func_data, branch.true_bb()).unwrap();
            let else_tag = block_name(config.func_data, branch.false_bb()).unwrap();

            insts.push(RiscvInstruction::Beqz(cond, else_tag));
            config.table.reset(cond);
            insts.push(RiscvInstruction::Jump(then_tag));
        }
        ValueKind::Jump(jump) => {
            let jmp_tag = block_name(config.func_data, jump.target()).unwrap();
            insts.push(RiscvInstruction::Jump(jmp_tag));
        }
        ValueKind::Call(call) => {
            // Get currently occupied registers. Save them (a_) into s_ registers.
            let caller_saved = config.allocator.get_occupied_registers(value, call.args());
            for (i, (reg, _)) in caller_saved.iter().enumerate() {
                insts.push(RiscvInstruction::Mv(Register::S[i], *reg));
            }
            // Save arguments into registers (a_) or onto stack
            for (i, &arg) in call.args().iter().enumerate() {
                let mut reg = prepare_value(arg, insts, config, None);
                if let Some(index) = caller_saved.iter().position(|(r, _)| *r == reg) {
                    reg = Register::S[index];
                }
                if i < 8 {
                    insts.push(RiscvInstruction::Mv(Register::A[i], reg));
                } else {
                    insts.push(RiscvInstruction::Sw(
                        reg,
                        ((i - 8) * 4) as i32,
                        Register::SP,
                    ));
                }
                config.table.reset(reg);
            }
            // Call the function
            let callee = call.callee();
            let callee_data = config.program.func(callee);
            let func_name = function_name(callee_data).unwrap();
            insts.push(RiscvInstruction::Call(func_name));
            // Save the return value, if any
            if !config.func_data.dfg().value(value).ty().is_unit() {
                save_value(value, Register::A0, insts, config);
            }
            // Restore caller saved registers
            for (i, (reg, need_load)) in caller_saved.iter().enumerate() {
                if *need_load {
                    insts.push(RiscvInstruction::Mv(*reg, Register::S[i]));
                }
            }
        }
        ValueKind::Return(ret) => {
            match ret.value() {
                Some(val) => {
                    let reg = prepare_value(val, insts, config, None);
                    insts.push(RiscvInstruction::Mv(Register::A0, reg));
                    config.table.reset(reg);
                }
                None => {}
            };
            insts.push(RiscvInstruction::Ret(config.stack_layout.clone()));
        }
    }
}

fn prepare_value(
    value: Value,
    insts: &mut Vec<RiscvInstruction>,
    config: &mut TranslateConfig,
    reg: Option<Register>, // hint that indicates which register is preferred; not forced.
) -> Register {
    match config.symbol.get(&value) {
        Some(AllocPos::Reg(reg)) => *reg,
        Some(AllocPos::Stack(pos)) => {
            let pos = *pos;
            let reg = reg.unwrap_or_else(|| config.table.get_vaccant());
            insts.push(RiscvInstruction::Lw(reg, pos, Register::SP));
            reg
        }
        Some(AllocPos::StackPointer(pos)) => {
            let pos = *pos;
            let reg = reg.unwrap_or_else(|| config.table.get_vaccant());
            insts.push(RiscvInstruction::Addi(reg, Register::SP, pos));
            reg
        }
        None => match config.func_data.dfg().value(value).kind() {
            ValueKind::Integer(int) => {
                if int.value() != 0 {
                    let reg = config.table.get_vaccant();
                    insts.push(RiscvInstruction::Li(reg, int.value()));
                    reg
                } else {
                    Register::X0
                }
            }
            ValueKind::FuncArgRef(arg) => {
                if arg.index() < 8 {
                    Register::A[arg.index()]
                } else {
                    let reg = config.table.get_vaccant();
                    let offset = config.stack_layout.total + (arg.index() - 8) * 4;
                    insts.push(RiscvInstruction::Lw(reg, offset as i32, Register::SP));
                    reg
                }
            }
            e => unimplemented!("{:#?}", e),
        },
    }
}

fn save_value(
    value: Value,
    reg: Register,
    insts: &mut Vec<RiscvInstruction>,
    config: &mut TranslateConfig,
) {
    match config.allocator.allocation.get(&value) {
        Some(dst) => {
            config.symbol.store_register(value, *dst);
            if *dst != reg {
                insts.push(RiscvInstruction::Mv(*dst, reg));
            }
            config.table.reset(reg);
        }
        None => {
            config.symbol.store_stack(value, *config.stack_pos);
            let pos = *config.stack_pos;
            insts.push(RiscvInstruction::Sw(reg, pos, Register::SP));
            config.table.reset(reg);
            *config.stack_pos += 4;
        }
    }
}

// Helper functions

/// Get the name of a basic block, without leading '%koopa_builtin_'
fn block_name(func: &FunctionData, bb: BasicBlock) -> Option<String> {
    let name = func.dfg().bb(bb).name().as_ref()?;
    Some(name.strip_prefix("%koopa_builtin_")?.to_string())
}

/// Get the name of a function, without leading '@'
fn function_name(func_data: &FunctionData) -> Option<String> {
    Some(func_data.name().strip_prefix("@")?.to_string())
}

/// Get the name of a global variable, without leading '@'
fn global_variable_name(value_data: &ValueData) -> Option<String> {
    let name = value_data.name().as_ref()?;
    Some(name.strip_prefix("@")?.to_string())
}

fn global_unpack(program: &Program, list: &Aggregate) -> Vec<i32> {
    let mut data = Vec::new();
    for &elem in list.elems() {
        match program.borrow_value(elem).kind() {
            ValueKind::Aggregate(sublist) => {
                data.extend(global_unpack(program, sublist));
            }
            ValueKind::Integer(_) => {
                data.push(match program.borrow_value(elem.clone()).kind() {
                    ValueKind::Integer(i) => i.value(),
                    _ => unreachable!(),
                });
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
