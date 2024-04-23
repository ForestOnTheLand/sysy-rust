use crate::riscv::{RiscvBlock, RiscvFunction, RiscvInstruction, RiscvProgram};

impl RiscvProgram {
    pub fn optimize(&mut self) {
        for func in self.functions.iter_mut() {
            func.fold_addi();
            func.eliminate_load();
        }
    }
}

impl RiscvFunction {
    fn fold_addi(&mut self) {
        use RiscvInstruction::{Addi, Lw, Nop, Sw};
        for block in self.blocks.iter_mut() {
            let num = block.instructions.len();
            let mut need_clean = false;
            for i in 0..(num - 1) {
                if let Addi(address, base, offset) = block.instructions[i] {
                    match block.instructions[i + 1] {
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
                    }
                }
            }
            if need_clean {
                block.clear_nop();
            }
        }
    }

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
}

impl RiscvBlock {
    fn clear_nop(&mut self) {
        self.instructions
            .retain(|x| !matches!(x, RiscvInstruction::Nop));
    }
}
