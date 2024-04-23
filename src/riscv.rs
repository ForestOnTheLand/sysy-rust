use crate::translate_util::Register;

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
    pub stack_size: usize,
    pub save_ra: bool,
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
    // Comment
    Comment(String),
    Nop,
    // Control flow
    Beqz(Register, String),
    Bnez(Register, String),
    Jump(String),
    Call(String),
    Ret { stack_size: usize, save_ra: bool },
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
        let stack_size = self.stack_size;
        if stack_size != 0 {
            if stack_size < 2048 {
                writeln!(f, "  addi sp, sp, -{}", stack_size)?;
            } else {
                writeln!(f, "  li t0, {stack_size}")?;
                writeln!(f, "  sub sp, sp, t0")?;
            }
        }
        if self.save_ra {
            if stack_size - 4 < 2048 {
                writeln!(f, "  sw ra, {}(sp)", stack_size - 4)?;
            } else {
                writeln!(f, "  li t0, {}", stack_size - 4)?;
                writeln!(f, "  add t0, t0, sp")?;
                writeln!(f, "  sw ra, 0(t0)")?;
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
            RiscvInstruction::Ret {
                stack_size,
                save_ra,
            } => {
                if *save_ra {
                    if stack_size - 4 < 2048 {
                        writeln!(f, "  lw ra, {}(sp)", stack_size - 4)?;
                    } else {
                        writeln!(f, "  li t0, {}", stack_size - 4)?;
                        writeln!(f, "  add t0, t0, sp")?;
                        writeln!(f, "  lw ra, 0(t0)")?;
                    }
                }
                if *stack_size > 0 {
                    if *stack_size < 2048 {
                        writeln!(f, "  addi sp, sp, {}", stack_size)?;
                    } else {
                        writeln!(f, "  li t0, {}", stack_size)?;
                        writeln!(f, "  add sp, sp, t0")?;
                    }
                }
                writeln!(f, "  ret")
            }
            RiscvInstruction::Lw(d, a, b) => writeln!(f, "  lw {d}, {a}({b})"),
            RiscvInstruction::Sw(d, a, b) => writeln!(f, "  sw {d}, {a}({b})"),
            RiscvInstruction::Add(d, a, b) => writeln!(f, "  add {d}, {a}, {b}"),
            RiscvInstruction::Addi(d, a, b) => writeln!(f, "  addi {d}, {a}, {b}"),
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
            RiscvInstruction::Div(d, a, b) => writeln!(f, "  div {d}, {a}, {b}"),
            RiscvInstruction::Rem(d, a, b) => writeln!(f, "  rem {d}, {a}, {b}"),
            RiscvInstruction::Li(d, s) => writeln!(f, "  li {d}, {s}"),
            RiscvInstruction::La(d, s) => writeln!(f, "  la {d}, {s}"),
            RiscvInstruction::Mv(d, s) => writeln!(f, "  mv {d}, {s}"),
        }
    }
}
