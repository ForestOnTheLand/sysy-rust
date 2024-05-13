use crate::riscv::{RiscvBlock, RiscvFunction, RiscvInstruction, RiscvProgram};

impl RiscvProgram {
    pub fn optimize(&mut self) {
        for func in self.functions.iter_mut() {
            func.clear_useless();
            func.fold_addi();
            func.eliminate_load();
            func.eliminate_jump();
        }
    }
}

impl RiscvFunction {
    /// Clear instructions that essentially do nothing.
    fn clear_useless(&mut self) {
        use RiscvInstruction::{Nop, Xori};
        for block in self.blocks.iter_mut() {
            let num = block.instructions.len();
            let mut need_clean = false;
            for i in 0..num {
                if let Xori(d, s, 0) = block.instructions[i] {
                    if d == s {
                        block.instructions[i] = Nop;
                        need_clean = true;
                    }
                }
            }
            if need_clean {
                block.clear_nop();
            }
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
        use RiscvInstruction::{Addi, Lw, Nop, Sw};
        for block in self.blocks.iter_mut() {
            let num = block.instructions.len();
            let mut need_clean = false;
            for i in 0..(num - 1) {
                match block.instructions[i] {
                    Addi(address, base, offset) => match block.instructions[i + 1] {
                        Lw(dst, 0, addr) => {
                            if addr == address {
                                block.instructions[i] = Nop;
                                block.instructions[i + 1] = Lw(dst, offset, base);
                                need_clean = true;
                            }
                        }
                        Sw(dst, 0, addr) => {
                            if addr == address {
                                block.instructions[i] = Nop;
                                block.instructions[i + 1] = Sw(dst, offset, base);
                                need_clean = true;
                            }
                        }
                        _ => {}
                    },
                    _ => {}
                }
            }
            if need_clean {
                block.clear_nop();
            }
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
