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
    pub id: u8,
}

/// names of registers, see <https://pku-minic.github.io/online-doc/#/misc-app-ref/riscv-insts>
/// for more detailed information
const REG_NAME: [&str; 32] = [
    "x0", "ra", "sp", "gp", "tp", "t0", "t1", "t2", "s0", "s1", "a0", "a1", "a2", "a3", "a4", "a5",
    "a6", "a7", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11", "t3", "t4", "t5",
    "t6",
];

impl Register {
    pub fn new(id: u8) -> Register {
        if id >= 32 {
            panic!("register id '{id}' out of range")
        }
        Register { id }
    }

    #[allow(dead_code)]
    pub fn name(&self) -> &str {
        REG_NAME[self.id as usize]
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", REG_NAME[self.id as usize])
    }
}

impl str::FromStr for Register {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let id = match s {
            "x0" => 0,
            "ra" => 1,
            "sp" => 2,
            "gp" => 3,
            "tp" => 4,
            "t0" => 5,
            "t1" => 6,
            "t2" => 7,
            "s0" => 8,
            "s1" => 9,
            "a0" => 10,
            "a1" => 11,
            "a2" => 12,
            "a3" => 13,
            "a4" => 14,
            "a5" => 15,
            "a6" => 16,
            "a7" => 17,
            "s2" => 18,
            "s3" => 19,
            "s4" => 20,
            "s5" => 21,
            "s6" => 22,
            "s7" => 23,
            "s8" => 24,
            "s9" => 25,
            "s10" => 26,
            "s11" => 27,
            "t3" => 28,
            "t4" => 29,
            "t5" => 30,
            "t6" => 31,
            _ => return Err(format!("internal error: invalid register name '{s}'")),
        };
        Ok(Register { id })
    }
}

/// temp registers that are allowed to use freely, namely t0~t6
const TMP_REG: [u8; 7] = [5, 6, 7, 28, 29, 30, 31];

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
        for id in TMP_REG {
            if !self.state[id as usize] {
                self.state[id as usize] = true;
                self.available -= 1;
                return Register { id };
            }
        }
        panic!("no available registers now");
    }

    pub fn reset(&mut self, reg: Register) {
        if !TMP_REG.contains(&reg.id) {
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
    Stack(i32), // x = offset(%sp)
    #[allow(dead_code)]
    RegPointer(Register), // x = &%reg
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

    #[allow(dead_code)]
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
