//! Allocation policy of RISCV assembly code.
//! Temporary variables are allocated on either registers or stack.
//! We will use a trivial policy: use at most 12 registers for variables,
//! and if there are more variables, allocate them on stack.

use koopa::ir::Value;
use std::collections::HashMap;
use std::{fmt, str};

/// RISCV registers, with [`Register::id`] in 0~31 (32 in total)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Register(u8);

macro_rules! register_define {
    ($($name:ident $value:literal),*) => {
        $(pub const $name: Register = Register($value);)*
    };
}
macro_rules! register_list {
    ($($name:ident),*) => {
        [$(Register::$name),*]
    };
}

#[allow(dead_code)]
impl Register {
    register_define! {
        X0 0, RA 1, SP 2, GP 3, TP 4, T0 5, T1 6, T2 7, S0 8, S1 9, A0 10,
        A1 11, A2 12, A3 13, A4 14, A5 15, A6 16, A7 17, S2 18, S3 19, S4 20,
        S5 21, S6 22, S7 23, S8 24, S9 25, S10 26, S11 27, T3 28, T4 29, T5 30, T6 31
    }

    pub const A: [Register; 8] = register_list!(A0, A1, A2, A3, A4, A5, A6, A7);
    pub const S: [Register; 12] = register_list!(S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11);
    pub const T: [Register; 7] = register_list!(T0, T1, T2, T3, T4, T5, T6);

    /// names of registers, see <https://pku-minic.github.io/online-doc/#/misc-app-ref/riscv-insts>
    /// for more detailed information
    const NAME: [&'static str; 32] = [
        "x0", "ra", "sp", "gp", "tp", "t0", "t1", "t2", "s0", "s1", "a0", "a1", "a2", "a3", "a4",
        "a5", "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11", "t3", "t4",
        "t5", "t6",
    ];
}

impl Register {
    #[allow(dead_code)]
    pub fn name(&self) -> &str {
        Register::NAME[self.0 as usize]
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Register::NAME[self.0 as usize])
    }
}

#[derive(Debug)]
pub struct RegGroup {
    list: &'static [Register],
    state: [bool; 32],
}

impl RegGroup {
    pub const TEMP: [Register; 4] = register_list!(T1, T2, T3, T4);
    pub const STORE: [Register; 8] = register_list!(A0, A1, A2, A3, A4, A5, A6, A7);
    pub const VAR: [Register; 11] = register_list!(S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11);

    pub fn new_temp() -> Self {
        Self {
            list: &Self::TEMP,
            state: [false; 32],
        }
    }

    pub fn new_var() -> Self {
        Self {
            list: &Self::VAR,
            state: [false; 32],
        }
    }

    pub fn new_store() -> Self {
        Self {
            list: &Self::STORE,
            state: [false; 32],
        }
    }

    pub fn num(&self) -> usize {
        self.list.len()
    }

    pub fn get_vaccant(&mut self) -> Register {
        for reg in self.list.iter() {
            if !self.state[reg.0 as usize] {
                self.state[reg.0 as usize] = true;
                return *reg;
            }
        }
        panic!("no available registers now");
    }

    pub fn try_get_vaccant(&mut self) -> Option<Register> {
        for reg in self.list.iter() {
            if !self.state[reg.0 as usize] {
                self.state[reg.0 as usize] = true;
                return Some(*reg);
            }
        }
        None
    }

    pub fn reset(&mut self, reg: Register) {
        if !self.list.contains(&reg) {
            return;
        }
        if self.state[reg.0 as usize] {
            self.state[reg.0 as usize] = false;
        } else {
            panic!("register {reg} is not being occupied now");
        }
    }
}

/// Record the position (register or stack) of a single variable.
#[derive(PartialEq)]
pub enum AllocPos {
    Reg(Register),        // x = %reg
    RegPointer(Register), // x = &(%reg)
    Stack(i32),           // x = offset(%sp)
    StackPointer(i32),    // x = %sp + offset = &offset(%sp)
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

    pub fn store_register(&mut self, value: Value, reg: Register) {
        self.data.insert(value, AllocPos::Reg(reg));
    }

    pub fn store_register_pointer(&mut self, value: Value, reg: Register) {
        self.data.insert(value, AllocPos::RegPointer(reg));
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
