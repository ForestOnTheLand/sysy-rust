use crate::riscv::{RiscvFunction, RiscvInstruction, RiscvProgram};

impl RiscvProgram {
    pub fn optimize(&mut self) {
        for func in self.functions.iter_mut() {
            func.eliminate_load();
        }
    }
}

impl RiscvFunction {
    fn eliminate_load(&mut self) {
        use RiscvInstruction::{Lw, Nop, Sw};
        for block in self.blocks.iter_mut() {
            let num = block.instructions.len();
            for i in 0..(num - 1) {
                if let Sw(reg_1, offset_1, addr_1) = block.instructions[i] {
                    if let Lw(reg_2, offset_2, addr_2) = block.instructions[i + 1] {
                        if offset_1 == offset_2 && addr_1 == addr_2 {
                            if reg_1 == reg_2 {
                                block.instructions[i + 1] = Nop;
                            }
                        }
                    }
                }
            }
        }
    }
}
