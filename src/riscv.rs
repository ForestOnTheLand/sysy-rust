use crate::translate_util::Register;

#[derive(Debug, Clone)]
pub struct StackLayout {
    /// whether the function doesn't calls any other functions
    pub leaf: bool,
    /// the maximum number of arguments that need to pass by storing in the stack
    pub args: usize,
    /// the maximum number of `a_` registers that need to be save
    pub save_reg_a: usize,
    /// the maximum number of `s_` registers that need to be save
    pub save_reg_s: usize,
    /// the total size of the stack, in bytes
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

#[derive(Debug, Clone, Copy)]
#[allow(dead_code)]
pub enum Bop {
    Add,
    Sub,
    Slt,
    Xor,
    Or,
    And,
    Sll,
    Srl,
    Sra,
    Mul,
    Div,
    Rem,
}

impl Bop {
    fn name(&self) -> &'static str {
        match self {
            Bop::Add => "add",
            Bop::Sub => "sub",
            Bop::Slt => "slt",
            Bop::Xor => "xor",
            Bop::Or => "or",
            Bop::And => "and",
            Bop::Sll => "sll",
            Bop::Srl => "srl",
            Bop::Sra => "sra",
            Bop::Mul => "mul",
            Bop::Div => "div",
            Bop::Rem => "rem",
        }
    }

    fn combinable(&self) -> bool {
        matches!(
            self,
            Bop::Add | Bop::Slt | Bop::Or | Bop::And | Bop::Sll | Bop::Srl | Bop::Sra | Bop::Xor
        )
    }
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
    Bge(Register, Register, String),
    Blt(Register, Register, String),
    Beq(Register, Register, String),
    Bne(Register, Register, String),
    Jump(String),
    Call(String),
    Ret,
    // Memory
    Lw(Register, i32, Register),
    Sw(Register, i32, Register),
    // Binary Expression
    Bexp(Bop, Register, Register, Register),
    Bexpi(Bop, Register, Register, i32),
    // Unary Expression
    Seqz(Register, Register),
    Snez(Register, Register),
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
            writeln!(f, "{}", func)?;
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
        writeln!(f, "  sw ra, -4(sp)\n  sw s0, -8(sp)")?;
        for i in 1..=self.stack_layout.save_reg_s {
            writeln!(f, "  sw s{}, -{}(sp)", i, 8 + 4 * i)?;
        }
        writeln!(f, "  mv s0, sp")?;
        if stack_size < 2048 {
            writeln!(f, "  addi sp, sp, -{stack_size}")?;
        } else {
            writeln!(f, "  li t0, {stack_size}")?;
            writeln!(f, "  sub sp, sp, t0")?;
        }
        for (i, block) in self.blocks.iter().enumerate() {
            if i != 0 {
                writeln!(f, "{}:", block.name)?;
            }
            for inst in block.instructions.iter() {
                inst.display(f, &self.stack_layout)?;
            }
        }
        Ok(())
    }
}

impl RiscvInstruction {
    fn display(&self, f: &mut std::fmt::Formatter<'_>, layout: &StackLayout) -> std::fmt::Result {
        match self {
            RiscvInstruction::Comment(s) => writeln!(f, "  # {s}"),
            RiscvInstruction::Nop => Ok(()),
            RiscvInstruction::Beqz(d, s) => writeln!(f, "  beqz {d}, {s}"),
            RiscvInstruction::Bnez(d, s) => writeln!(f, "  bnez {d}, {s}"),
            RiscvInstruction::Blt(a, b, s) => writeln!(f, "  blt {a}, {b}, {s}"),
            RiscvInstruction::Bge(a, b, s) => writeln!(f, "  bge {a}, {b}, {s}"),
            RiscvInstruction::Beq(a, b, s) => writeln!(f, "  beq {a}, {b}, {s}"),
            RiscvInstruction::Bne(a, b, s) => writeln!(f, "  bne {a}, {b}, {s}"),
            RiscvInstruction::Jump(label) => writeln!(f, "  j {label}"),
            RiscvInstruction::Call(label) => writeln!(f, "  call {label}"),
            RiscvInstruction::Ret => {
                if layout.total < 2048 {
                    writeln!(f, "  addi sp, sp, {}", layout.total)?;
                } else {
                    writeln!(f, "  li t0, {}", layout.total)?;
                    writeln!(f, "  add sp, sp, t0")?;
                }
                writeln!(f, "  lw ra, -4(sp)\n  lw s0, -8(sp)")?;
                for i in 1..=layout.save_reg_s {
                    writeln!(f, "  lw s{}, -{}(sp)", i, 8 + 4 * i)?;
                }

                writeln!(f, "  ret")
            }
            RiscvInstruction::Lw(d, i, b) => {
                if (-2048..2048).contains(i) {
                    writeln!(f, "  lw {d}, {i}({b})")
                } else {
                    writeln!(f, "  li t0, {i}\n  add t0, t0, {b}\n  lw {d}, (t0)")
                }
            }
            RiscvInstruction::Sw(d, i, b) => {
                if (-2048..2048).contains(i) {
                    writeln!(f, "  sw {d}, {i}({b})")
                } else {
                    writeln!(f, "  li t0, {i}\n  add t0, t0, sp\n  sw {d}, (t0)")
                }
            }
            RiscvInstruction::Bexp(op, dst, a, b) => writeln!(f, "  {} {dst}, {a}, {b}", op.name()),
            RiscvInstruction::Bexpi(op, dst, src, int) => {
                if (-2048..2048).contains(int) && op.combinable() {
                    writeln!(f, "  {}i {dst}, {src}, {int}", op.name())
                } else if matches!(op, Bop::Sub) && (-2047..=2048).contains(int) {
                    writeln!(f, "  addi {dst}, {src}, {}", -int)
                } else if matches!(op, Bop::Mul) && int_log2(int).is_some() {
                    writeln!(f, "  slli {dst}, {src}, {}", int_log2(int).unwrap())
                } else {
                    writeln!(f, "  li t0, {int}\n  {} {dst}, {src}, t0", op.name())
                }
            }
            RiscvInstruction::Seqz(d, s) => writeln!(f, "  seqz {d}, {s}"),
            RiscvInstruction::Snez(d, s) => writeln!(f, "  snez {d}, {s}"),
            RiscvInstruction::Li(d, s) => writeln!(f, "  li {d}, {s}"),
            RiscvInstruction::La(d, s) => writeln!(f, "  la {d}, {s}"),
            RiscvInstruction::Mv(d, s) => writeln!(f, "  mv {d}, {s}"),
        }
    }
}

fn int_log2(i: &i32) -> Option<u32> {
    if *i & (*i - 1) == 0 {
        i.checked_ilog2()
    } else {
        None
    }
}
