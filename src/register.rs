//! Allocation policy of RISCV assembly code.
//! Temporary variables are allocated on either registers or stack.
//! We will use a trivial policy: use at most 12 registers for variables,
//! and if there are more variables, allocate them on stack.

use crate::util::Error;
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
    pub fn new(id: u8) -> Result<Register, Error> {
        if id < 32 {
            Ok(Register { id })
        } else {
            Err(Error::InternalError(format!(
                "register id '{id}' out of range"
            )))
        }
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
    type Err = Error;
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
            _ => return Err(Error::InternalError(format!("Invalid register name '{s}'"))),
        };
        Ok(Register::new(id as u8).unwrap())
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

    pub fn get_vaccant(&mut self) -> Result<Register, Error> {
        for id in TMP_REG {
            if !self.state[id as usize] {
                self.state[id as usize] = true;
                self.available -= 1;
                return Register::new(id);
            }
        }
        Err(Error::InternalError(
            "no available registers now".to_string(),
        ))
    }

    pub fn reset(&mut self, reg: Register) -> Result<(), Error> {
        if !TMP_REG.contains(&reg.id) {
            // Operations onto non-temporary registers is ignored safely
            Ok(())
        } else if self.state[reg.id as usize] {
            self.state[reg.id as usize] = false;
            self.available += 1;
            Ok(())
        } else {
            Err(Error::InternalError(format!(
                "register {reg} is not being occupied now"
            )))
        }
    }

    #[allow(dead_code)]
    pub fn remain(&self) -> i32 {
        self.available
    }
}

/// Record the position (register or stack) of a single variable.
pub enum AllocPos {
    Reg(Register),     // i32, stored at `register`
    Stack(i32),        // i32, stored at `offset`(sp)
    StackPointer(i32), // *i32, pointing at `offset`(sp)
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

    pub fn get_stack(&self, value: &Value) -> Option<i32> {
        match self.data.get(value) {
            Some(AllocPos::Stack(offset)) => Some(*offset),
            _ => None,
        }
    }

    pub fn get_stack_pointer(&self, value: &Value) -> Option<i32> {
        match self.data.get(value) {
            Some(AllocPos::StackPointer(offset)) => Some(*offset),
            _ => None,
        }
    }

    pub fn store_register(&mut self, value: Value, reg: Register) -> Result<(), Error> {
        match self.data.insert(value, AllocPos::Reg(reg)) {
            None => Ok(()),
            Some(_) => Err(Error::InternalError(
                "value stored in register before".to_string(),
            )),
        }
    }

    pub fn store_stack(&mut self, value: Value, offset: i32) -> Result<(), Error> {
        match self.data.insert(value, AllocPos::Stack(offset)) {
            None => Ok(()),
            Some(_) => Err(Error::InternalError(
                "variable stored in stack before".to_string(),
            )),
        }
    }

    pub fn store_stack_pointer(&mut self, value: Value, offset: i32) -> Result<(), Error> {
        match self.data.insert(value, AllocPos::StackPointer(offset)) {
            None => Ok(()),
            Some(_) => Err(Error::InternalError(
                "pointer stored in stack before".to_string(),
            )),
        }
    }
}
