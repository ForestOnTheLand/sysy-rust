use crate::{
    riscv::{Bop, RiscvBlock, RiscvFunction, RiscvInstruction, RiscvProgram},
    translate_util::{RegGroup, Register},
};

impl RiscvProgram {
    pub fn optimize(&mut self) {
        for func in self.functions.iter_mut() {
            func.clear_useless();
            func.fold_constant();
            func.fold_move();
            func.fold_addi();
            func.fold_condition();
            func.eliminate_load();
            func.eliminate_jump();
        }
    }
}

impl RiscvFunction {
    /// Clear instructions that essentially do nothing.
    fn clear_useless(&mut self) {
        use RiscvInstruction::*;
        for block in self.blocks.iter_mut() {
            let num = block.instructions.len();
            for i in 0..num {
                match block.instructions[i] {
                    Comment(_) => block.instructions[i] = Nop,
                    Mv(d, s) if d == s => block.instructions[i] = Nop,
                    Bexpi(Bop::Xor, d, s, 0) if d == s => block.instructions[i] = Nop,
                    Bexp(Bop::Xor, d, s, Register::X0) if d == s => block.instructions[i] = Nop,
                    Bexpi(Bop::Mul, dst, Register::X0, _) => block.instructions[i] = Li(dst, 0),
                    Bexpi(Bop::Add, dst, a, 0) | Bexp(Bop::Add, dst, a, Register::X0) => {
                        block.instructions[i] = if dst == a { Nop } else { Mv(dst, a) };
                    }

                    // Bexp(_, Register::X0, _, _) => block.instructions[i] = Nop,
                    // Bexpi(_, Register::X0, _, _) => block.instructions[i] = Nop,
                    _ => {}
                };
            }
            block.clear_nop();
        }
    }

    /// Combine addi with other instructions, such as
    /// ```riscv
    ///   addi t1, 100, sp
    ///   sw t1, 0(t1)
    /// # Equivalent to
    ///   sw t1, 100(sp)
    /// ```
    fn fold_addi(&mut self) {
        use RiscvInstruction::{Bexp, Bexpi, Lw, Mv, Nop, Sw};
        for block in self.blocks.iter_mut() {
            let num = block.instructions.len();
            for i in (1..num).rev() {
                let (address, base, offset) = match block.instructions[i - 1] {
                    Bexpi(Bop::Add, address, base, offset) => (address, base, offset),
                    Bexp(Bop::Add, address, base, Register::X0) => (address, base, 0),
                    Mv(address, base) => (address, base, 0),
                    _ => continue,
                };
                if RegGroup::VAR.contains(&address) {
                    continue;
                }
                match block.instructions[i] {
                    Lw(dst, int, addr) => {
                        if addr == address {
                            block.instructions[i] = Nop;
                            block.instructions[i - 1] = Lw(dst, offset + int, base);
                        }
                    }
                    Sw(dst, int, addr) => {
                        if addr == address {
                            block.instructions[i] = Nop;
                            block.instructions[i - 1] = Sw(dst, offset + int, base);
                        }
                    }
                    _ => {}
                }
            }
            block.clear_nop();
        }
    }

    fn fold_condition(&mut self) {
        use RiscvInstruction::*;
        for block in self.blocks.iter_mut() {
            let num = block.instructions.len();
            if num == 0 {
                continue;
            }
            for i in (1..num).rev() {
                match block.instructions[i] {
                    Beqz(cond, ref label) => match block.instructions[i - 1] {
                        Snez(dst, src) if dst == cond => {
                            block.instructions[i - 1] = Beqz(src, label.clone());
                            block.instructions[i] = Nop;
                        }
                        Seqz(dst, src) if dst == cond => {
                            block.instructions[i - 1] = Bnez(src, label.clone());
                            block.instructions[i] = Nop;
                        }
                        Bexp(Bop::Slt, dst, a, b) if dst == cond => {
                            block.instructions[i - 1] = Bge(a, b, label.clone());
                            block.instructions[i] = Nop;
                        }
                        Bexp(Bop::Xor | Bop::Sub, dst, a, b) if dst == cond => {
                            block.instructions[i - 1] = Beq(a, b, label.clone());
                            block.instructions[i] = Nop;
                        }
                        Bexpi(Bop::Slt, dst, a, b) if dst == cond => {
                            block.instructions[i] = Bge(a, Register::T0, label.clone());
                            block.instructions[i - 1] = Li(Register::T0, b);
                        }
                        Bexpi(Bop::Xor | Bop::Sub, dst, a, b) if dst == cond => {
                            block.instructions[i] = Beq(a, Register::T0, label.clone());
                            block.instructions[i - 1] = Li(Register::T0, b);
                        }
                        _ => {}
                    },
                    Bnez(cond, ref label) => match block.instructions[i - 1] {
                        Snez(dst, src) if dst == cond => {
                            block.instructions[i - 1] = Bnez(src, label.clone());
                            block.instructions[i] = Nop;
                        }
                        Seqz(dst, src) if dst == cond => {
                            block.instructions[i - 1] = Beqz(src, label.clone());
                            block.instructions[i] = Nop;
                        }
                        Bexp(Bop::Slt, dst, a, b) if dst == cond => {
                            block.instructions[i - 1] = Blt(a, b, label.clone());
                            block.instructions[i] = Nop;
                        }
                        Bexp(Bop::Xor | Bop::Sub, dst, a, b) if dst == cond => {
                            block.instructions[i - 1] = Bne(a, b, label.clone());
                            block.instructions[i] = Nop;
                        }
                        Bexpi(Bop::Slt, dst, a, b) if dst == cond => {
                            block.instructions[i] = Blt(a, Register::T0, label.clone());
                            block.instructions[i - 1] = Li(Register::T0, b);
                        }
                        Bexpi(Bop::Xor | Bop::Sub, dst, a, b) if dst == cond => {
                            block.instructions[i] = Bne(a, Register::T0, label.clone());
                            block.instructions[i - 1] = Li(Register::T0, b);
                        }
                        _ => {}
                    },
                    _ => {}
                };
            }
            block.clear_nop();
        }
    }

    fn fold_move(&mut self) {
        use RiscvInstruction::{Bexp, Bexpi, Lw, Mv, Nop};
        for block in self.blocks.iter_mut() {
            let num = block.instructions.len();
            if num == 0 {
                continue;
            }
            for i in 1..(num - 1) {
                if let Mv(dst, src) = block.instructions[i] {
                    if RegGroup::VAR.contains(&src) {
                        continue;
                    }
                    match block.instructions[i - 1] {
                        Bexpi(op, d, a, b) if d == src => {
                            block.instructions[i - 1] = Nop;
                            block.instructions[i] = Bexpi(op, dst, a, b);
                        }
                        Bexp(op, d, a, b) if d == src => {
                            block.instructions[i - 1] = Nop;
                            block.instructions[i] = Bexp(op, dst, a, b);
                        }
                        Lw(d, a, b) if d == src => {
                            block.instructions[i - 1] = Nop;
                            block.instructions[i] = Lw(dst, a, b);
                        }
                        _ => {}
                    };
                }
            }
            block.clear_nop();
        }
    }

    fn fold_constant(&mut self) {
        use RiscvInstruction::*;
        for block in self.blocks.iter_mut() {
            let num = block.instructions.len();
            if num == 0 {
                continue;
            }
            for i in 0..(num - 1) {
                if let Li(reg, value) = block.instructions[i] {
                    let clear = !RegGroup::VAR.contains(&reg);
                    match block.instructions[i + 1] {
                        Bexp(op, d, a, b) if b == reg => {
                            if clear {
                                block.instructions[i] = Nop;
                            }
                            block.instructions[i + 1] = Bexpi(op, d, a, value);
                        }
                        Bexpi(Bop::Mul, d, a, b) if a == reg => {
                            if clear {
                                block.instructions[i] = Nop;
                            }
                            block.instructions[i + 1] = Li(d, value * b);
                        }
                        Mv(d, s) if s == reg => {
                            if clear {
                                block.instructions[i] = Nop;
                            }
                            block.instructions[i + 1] = Li(d, value);
                        }
                        _ => {}
                    };
                }
            }
            block.clear_nop();
        }
    }

    /// Eliminate useless load, such as
    /// ```riscv
    ///   sw t1, 16(sp)
    ///   lw t1, 16(sp) # useless
    /// ```
    fn eliminate_load(&mut self) {
        use RiscvInstruction::{Lw, Nop, Sw};
        for block in self.blocks.iter_mut() {
            let num = block.instructions.len();
            let mut need_clean = false;
            if num == 0 {
                continue;
            }
            for i in 0..(num - 1) {
                if let Sw(reg_1, offset_1, addr_1) = block.instructions[i] {
                    if let Lw(reg_2, offset_2, addr_2) = block.instructions[i + 1] {
                        if offset_1 == offset_2 && addr_1 == addr_2 {
                            if reg_1 == reg_2 {
                                block.instructions[i + 1] = Nop;
                                need_clean = true;
                            }
                        }
                    }
                }
            }
            if need_clean {
                block.clear_nop();
            }
        }
    }

    /// Eliminate useless jump, such as
    /// ```riscv
    ///   j label # useless
    /// label:
    /// ```
    fn eliminate_jump(&mut self) {
        use RiscvInstruction::Jump;
        let num = self.blocks.len();
        for i in 0..(num - 1) {
            if let Some(Jump(label)) = self.blocks[i].instructions.last() {
                if &self.blocks[i + 1].name == label {
                    self.blocks[i].instructions.pop();
                }
            }
        }
    }
}

impl RiscvBlock {
    /// Clear Nops in a block.
    fn clear_nop(&mut self) {
        self.instructions
            .retain(|x| !matches!(x, RiscvInstruction::Nop));
    }
}
