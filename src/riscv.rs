use crate::translate_util::Register;

#[derive(Debug, Clone)]
pub struct StackLayout {
    pub args: usize,
    pub save: usize,
    pub total: usize,
}

#[derive(Debug)]
pub struct RiscvProgram {
    pub values: Vec<RiscvValue>,
    pub functions: Vec<RiscvFunction>,
}

#[derive(Debug)]
pub struct RiscvValue {
    pub name: String,
    pub init: Vec<i32>,
}

#[derive(Debug)]
pub struct RiscvFunction {
    pub stack_layout: StackLayout,
    pub blocks: Vec<RiscvBlock>,
}

#[derive(Debug)]
pub struct RiscvBlock {
    pub name: String,
    pub instructions: Vec<RiscvInstruction>,
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum RiscvInstruction {
    Nop,
    // Comment
    Comment(String),
    // Control flow
    Beqz(Register, String),
    Bnez(Register, String),
    Jump(String),
    Call(String),
    Ret(StackLayout),
    // Memory
    Lw(Register, i32, Register),
    Sw(Register, i32, Register),
    // Calculate
    Add(Register, Register, Register),
    Addi(Register, Register, i32),
    Sub(Register, Register, Register),
    Slt(Register, Register, Register),
    Sgt(Register, Register, Register),
    Seqz(Register, Register),
    Snez(Register, Register),
    Xor(Register, Register, Register),
    Xori(Register, Register, i32),
    Or(Register, Register, Register),
    Ori(Register, Register, i32),
    And(Register, Register, Register),
    Andi(Register, Register, i32),
    Sll(Register, Register, Register),
    Srl(Register, Register, Register),
    Sra(Register, Register, Register),
    Mul(Register, Register, Register),
    Muli(Register, Register, i32),
    Div(Register, Register, Register),
    Rem(Register, Register, Register),
    // Load & Move
    Li(Register, i32),
    La(Register, String),
    Mv(Register, Register),
}

impl RiscvProgram {
    pub fn new() -> RiscvProgram {
        RiscvProgram {
            values: Vec::new(),
            functions: Vec::new(),
        }
    }
}

impl std::fmt::Display for RiscvProgram {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "  .data")?;
        for value in self.values.iter() {
            write!(f, "{}", value)?;
        }
        writeln!(f, "\n  .text")?;
        for func in self.functions.iter() {
            write!(f, "{}", func)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for RiscvValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "  .globl {}\n{}:\n", self.name, self.name)?;
        let mut zero_counter = 0;
        for value in self.init.iter() {
            if *value != 0 {
                if zero_counter != 0 {
                    writeln!(f, "  .zero {zero_counter}")?;
                }
                writeln!(f, "  .word {value}")?;
            } else {
                zero_counter += 4;
            }
        }
        if zero_counter != 0 {
            writeln!(f, "  .zero {zero_counter}")?;
        }
        Ok(())
    }
}

impl std::fmt::Display for RiscvFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = &self.blocks[0].name;
        writeln!(f, "  .globl {}\n{}:", name, name)?;
        let stack_size = self.stack_layout.total;
        if self.stack_layout.save != 0 {
            writeln!(f, "  sw ra, -4(sp)")?;
            for i in 1..self.stack_layout.save {
                writeln!(f, "  sw s{}, -{}(sp)", i - 1, 4 + 4 * i)?;
            }
        }
        if stack_size != 0 {
            if stack_size < 2048 {
                writeln!(f, "  addi sp, sp, -{}", stack_size)?;
            } else {
                writeln!(f, "  li t0, {stack_size}")?;
                writeln!(f, "  sub sp, sp, t0")?;
            }
        }
        for (i, block) in self.blocks.iter().enumerate() {
            if i != 0 {
                writeln!(f, "{}:", block.name)?;
            }
            for inst in block.instructions.iter() {
                write!(f, "{}", inst)?;
            }
        }
        Ok(())
    }
}

impl std::fmt::Display for RiscvInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RiscvInstruction::Comment(s) => writeln!(f, "  # {s}"),
            RiscvInstruction::Nop => Ok(()),
            RiscvInstruction::Beqz(d, s) => writeln!(f, "  beqz {d}, {s}"),
            RiscvInstruction::Bnez(d, s) => writeln!(f, "  bnez {d}, {s}"),
            RiscvInstruction::Jump(label) => writeln!(f, "  j {label}"),
            RiscvInstruction::Call(label) => writeln!(f, "  call {label}"),
            RiscvInstruction::Ret(layout) => {
                if layout.total > 0 {
                    if layout.total < 2048 {
                        writeln!(f, "  addi sp, sp, {}", layout.total)?;
                    } else {
                        writeln!(f, "  li t0, {}", layout.total)?;
                        writeln!(f, "  add sp, sp, t0")?;
                    }
                }
                if layout.save != 0 {
                    writeln!(f, "  lw ra, -4(sp)")?;
                    for i in 1..layout.save {
                        writeln!(f, "  lw s{}, -{}(sp)", i - 1, 4 + 4 * i)?;
                    }
                }

                writeln!(f, "  ret")
            }
            RiscvInstruction::Lw(d, i, b) => {
                if *i < 2048 && *i >= -2048 {
                    writeln!(f, "  lw {d}, {i}({b})")
                } else {
                    writeln!(f, "  li t0, {i}\n  add t0, t0, {b}\n  lw {d}, (t0)")
                }
            }
            RiscvInstruction::Sw(d, i, b) => {
                if *i < 2048 && *i >= -2048 {
                    writeln!(f, "  sw {d}, {i}({b})")
                } else {
                    writeln!(f, "  li t0, {i}\n  add t0, t0, sp\n  sw {d}, (t0)")
                }
            }
            RiscvInstruction::Add(d, a, b) => writeln!(f, "  add {d}, {a}, {b}"),
            RiscvInstruction::Addi(d, a, i) => {
                if *i < 2048 && *i >= -2048 {
                    writeln!(f, "  addi {d}, {a}, {i}")
                } else {
                    writeln!(f, "  li t0, {i}\n  add {d}, {a}, t0")
                }
            }
            RiscvInstruction::Sub(d, a, b) => writeln!(f, "  sub {d}, {a}, {b}"),
            RiscvInstruction::Slt(d, a, b) => writeln!(f, "  slt {d}, {a}, {b}"),
            RiscvInstruction::Sgt(d, a, b) => writeln!(f, "  sgt {d}, {a}, {b}"),
            RiscvInstruction::Seqz(d, s) => writeln!(f, "  seqz {d}, {s}"),
            RiscvInstruction::Snez(d, s) => writeln!(f, "  snez {d}, {s}"),
            RiscvInstruction::Xor(d, a, b) => writeln!(f, "  xor {d}, {a}, {b}"),
            RiscvInstruction::Xori(d, a, b) => writeln!(f, "  xori {d}, {a}, {b}"),
            RiscvInstruction::Or(d, a, b) => writeln!(f, "  or {d}, {a}, {b}"),
            RiscvInstruction::Ori(d, a, b) => writeln!(f, "  ori {d}, {a}, {b}"),
            RiscvInstruction::And(d, a, b) => writeln!(f, "  and {d}, {a}, {b}"),
            RiscvInstruction::Andi(d, a, b) => writeln!(f, "  andi {d}, {a}, {b}"),
            RiscvInstruction::Sll(d, a, b) => writeln!(f, "  sll {d}, {a}, {b}"),
            RiscvInstruction::Srl(d, a, b) => writeln!(f, "  srl {d}, {a}, {b}"),
            RiscvInstruction::Sra(d, a, b) => writeln!(f, "  sra {d}, {a}, {b}"),
            RiscvInstruction::Mul(d, a, b) => writeln!(f, "  mul {d}, {a}, {b}"),
            RiscvInstruction::Muli(d, a, i) => match int_log2(*i) {
                Some(u) => writeln!(f, "  slli {d}, {a}, {u}"),
                None => writeln!(f, "  li t0, {i}\n  mul {d}, {a}, t0"),
            },
            RiscvInstruction::Div(d, a, b) => writeln!(f, "  div {d}, {a}, {b}"),
            RiscvInstruction::Rem(d, a, b) => writeln!(f, "  rem {d}, {a}, {b}"),
            RiscvInstruction::Li(d, s) => writeln!(f, "  li {d}, {s}"),
            RiscvInstruction::La(d, s) => writeln!(f, "  la {d}, {s}"),
            RiscvInstruction::Mv(d, s) => writeln!(f, "  mv {d}, {s}"),
        }
    }
}

fn int_log2(i: i32) -> Option<u32> {
    if i & (i - 1) == 0 {
        i.checked_ilog2()
    } else {
        None
    }
}
