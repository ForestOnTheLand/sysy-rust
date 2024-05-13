//! Allocation policy of RISCV assembly code.
//! Temporary variables are allocated on either registers or stack.
//! We will use a trivial policy: use at most 12 registers for variables,
//! and if there are more variables, allocate them on stack.

use koopa::ir::Value;
use std::collections::HashMap;
use std::{fmt, str};

/// RISCV registers, with [`Register::id`] in 0~31 (32 in total)
#[derive(Debug, Clone, Copy)]
pub struct Register {
    id: u8,
}

#[allow(dead_code)]
impl Register {
    pub const X0: Register = Register { id: 0 };
    pub const RA: Register = Register { id: 1 };
    pub const SP: Register = Register { id: 2 };
    pub const GP: Register = Register { id: 3 };
    pub const TP: Register = Register { id: 4 };
    pub const T0: Register = Register { id: 5 };
    pub const T1: Register = Register { id: 6 };
    pub const T2: Register = Register { id: 7 };
    pub const S0: Register = Register { id: 8 };
    pub const S1: Register = Register { id: 9 };
    pub const A0: Register = Register { id: 10 };
    pub const A1: Register = Register { id: 11 };
    pub const A2: Register = Register { id: 12 };
    pub const A3: Register = Register { id: 13 };
    pub const A4: Register = Register { id: 14 };
    pub const A5: Register = Register { id: 15 };
    pub const A6: Register = Register { id: 16 };
    pub const A7: Register = Register { id: 17 };
    pub const S2: Register = Register { id: 18 };
    pub const S3: Register = Register { id: 19 };
    pub const S4: Register = Register { id: 20 };
    pub const S5: Register = Register { id: 21 };
    pub const S6: Register = Register { id: 22 };
    pub const S7: Register = Register { id: 23 };
    pub const S8: Register = Register { id: 24 };
    pub const S9: Register = Register { id: 25 };
    pub const S10: Register = Register { id: 26 };
    pub const S11: Register = Register { id: 27 };
    pub const T3: Register = Register { id: 28 };
    pub const T4: Register = Register { id: 29 };
    pub const T5: Register = Register { id: 30 };
    pub const T6: Register = Register { id: 31 };

    pub const A: [Register; 8] = [
        Register::A0,
        Register::A1,
        Register::A2,
        Register::A3,
        Register::A4,
        Register::A5,
        Register::A6,
        Register::A7,
    ];

    /// names of registers, see <https://pku-minic.github.io/online-doc/#/misc-app-ref/riscv-insts>
    /// for more detailed information
    const NAME: [&'static str; 32] = [
        "x0", "ra", "sp", "gp", "tp", "t0", "t1", "t2", "s0", "s1", "a0", "a1", "a2", "a3", "a4",
        "a5", "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11", "t3", "t4",
        "t5", "t6",
    ];
    /// temp registers that are allowed to use freely, namely t0~t6
    const TMP_REG: [u8; 6] = [6, 7, 28, 29, 30, 31];
}

impl Register {
    #[allow(dead_code)]
    pub fn name(&self) -> &str {
        Register::NAME[self.id as usize]
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Register::NAME[self.id as usize])
    }
}

impl PartialEq for Register {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Eq for Register {}

/// Record the state of registers (occupied or not)
pub struct RegisterTable {
    state: [bool; 32],
    available: i32,
}

impl RegisterTable {
    pub fn new() -> RegisterTable {
        RegisterTable {
            state: [false; 32],
            available: 7,
        }
    }

    pub fn get_vaccant(&mut self) -> Register {
        for id in Register::TMP_REG {
            if !self.state[id as usize] {
                self.state[id as usize] = true;
                self.available -= 1;
                return Register { id };
            }
        }
        panic!("no available registers now");
    }

    pub fn reset(&mut self, reg: Register) {
        if !Register::TMP_REG.contains(&reg.id) {
            return;
        }
        if self.state[reg.id as usize] {
            self.state[reg.id as usize] = false;
            self.available += 1;
        } else {
            panic!("register {reg} is not being occupied now");
        }
    }

    #[allow(dead_code)]
    pub fn remain(&self) -> i32 {
        self.available
    }
}

/// Record the position (register or stack) of a single variable.
pub enum AllocPos {
    #[allow(dead_code)]
    Reg(Register), // x = %reg
    Stack(i32),        // x = offset(%sp)
    StackPointer(i32), // x = %sp + offset = &offset(%sp)
}

/// Record the position (register or stack) of all variables in a function.
pub struct AllocTable {
    data: HashMap<Value, AllocPos>,
}

impl AllocTable {
    pub fn new() -> AllocTable {
        AllocTable {
            data: HashMap::new(),
        }
    }

    pub fn get(&self, value: &Value) -> Option<&AllocPos> {
        self.data.get(value)
    }

    #[allow(dead_code)]
    pub fn store_register(&mut self, value: Value, reg: Register) {
        self.data.insert(value, AllocPos::Reg(reg));
    }

    pub fn store_stack(&mut self, value: Value, offset: i32) {
        self.data.insert(value, AllocPos::Stack(offset));
    }

    pub fn store_stack_pointer(&mut self, value: Value, offset: i32) {
        self.data.insert(value, AllocPos::StackPointer(offset));
    }

    pub fn get_stack_pointer(&mut self, value: &Value) -> i32 {
        if let Some(AllocPos::StackPointer(p)) = self.data.get(value) {
            p.clone()
        } else {
            panic!()
        }
    }
}
