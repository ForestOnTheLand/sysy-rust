//! RISCV registers

use core::fmt;
use std::collections::HashSet;

use crate::util::Error;

/// All 32 RISCV registers
#[derive(Debug, Clone, Copy)]
pub struct Register {
    pub id: u8,
}

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
            Err(Error::InvalidRegisterError)
        }
    }

    pub fn name(&self) -> &str {
        REG_NAME[self.id as usize]
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", REG_NAME[self.id as usize])
    }
}

/// temp registers that are allowed to be used
const TMP_REG: [u8; 15] = [5, 6, 7, 28, 29, 30, 31, 10, 11, 12, 13, 14, 15, 16, 17];

pub struct RegisterTable {
    state: [bool; 32],
}

impl RegisterTable {
    pub fn new() -> RegisterTable {
        RegisterTable { state: [false; 32] }
    }

    pub fn get_vaccant(&mut self) -> Result<Register, Error> {
        for id in TMP_REG {
            if !self.state[id as usize] {
                self.state[id as usize] = true;
                return Register::new(id).or(Err(Error::InternalError));
            }
        }
        Err(Error::RegisterAllocError)
    }

    pub fn reset(&mut self, reg: Register) -> Result<(), Error> {
        if reg.id == 0 {
            // Operations onto x0 is ignored safely
            Ok(())
        } else if self.state[reg.id as usize] {
            self.state[reg.id as usize] = false;
            Ok(())
        } else {
            Err(Error::RegisterFreeError)
        }
    }
}
