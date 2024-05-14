use std::{
    cmp::{max, min},
    collections::{BTreeSet, HashMap},
};

use koopa::ir::{FunctionData, Value};

use crate::translate_util::{RegGroup, Register};

#[derive(Eq, Clone, Copy, Debug)]
struct ActiveVariable {
    value: Value,
    start: u32,
    id: u32,
    end: u32,
}

impl PartialEq for ActiveVariable {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl PartialOrd for ActiveVariable {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        (self.end, self.id).partial_cmp(&(other.end, other.id))
    }
}

impl Ord for ActiveVariable {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (self.end, self.id).cmp(&(other.end, other.id))
    }
}

#[derive(Debug, Clone)]
pub struct LifeTime {
    index: HashMap<Value, u32>,
    interval: HashMap<Value, (u32, u32)>,
    insts: Vec<ActiveVariable>,
}

impl LifeTime {
    pub fn new(func: &FunctionData) -> Self {
        let mut index = HashMap::new();
        let mut interval = HashMap::new();
        let mut i = 1;
        for (_, bb) in func.layout().bbs() {
            for (value, _) in bb.insts() {
                index.insert(value.clone(), i);
                i += 1;
            }
        }
        for (_, bb) in func.layout().bbs() {
            for (value, _) in bb.insts() {
                let mut l = u32::MAX;
                let mut r = 0u32;
                for friend in func.dfg().value(*value).used_by() {
                    let id = *index.get(friend).unwrap();
                    l = min(l, id);
                    r = max(r, id);
                }
                interval.insert(value.clone(), (l, r));
            }
        }
        let mut insts: Vec<ActiveVariable> = interval
            .iter()
            .map(|(v, (l, r))| ActiveVariable {
                value: *v,
                start: *l,
                id: *index.get(v).unwrap(),
                end: *r,
            })
            .collect();
        insts.sort_unstable_by_key(|v| v.start);
        Self {
            index,
            interval,
            insts,
        }
    }
}

#[derive(Debug)]
pub struct Allocator {
    pub allocation: HashMap<Value, Register>,
    lifetime: LifeTime,
    active: BTreeSet<ActiveVariable>,
    regs: RegGroup,
}

impl Allocator {
    pub fn linear_scan_register_allocation(lifetime: LifeTime) -> Self {
        let mut allocator = Allocator {
            lifetime: LifeTime {
                index: HashMap::new(),
                interval: HashMap::new(),
                insts: Vec::new(),
            },
            active: BTreeSet::new(),
            allocation: HashMap::new(),
            regs: RegGroup::new_store(),
        };
        for inst in lifetime.insts.iter().cloned() {
            allocator.expire_old_intervals(inst);
            if allocator.active.len() == allocator.regs.num() {
                allocator.spill_at_interval(inst);
            } else {
                let reg = allocator.regs.get_vaccant();
                allocator.allocation.insert(inst.value, reg);
                assert!(allocator.active.insert(inst));
            }
        }
        allocator.lifetime = lifetime;
        allocator
    }

    fn expire_old_intervals(&mut self, inst: ActiveVariable) {
        loop {
            if let Some(&var) = self.active.first() {
                if var.end >= inst.start {
                    return;
                }
                self.active.pop_first();
                let reg = *self.allocation.get(&var.value).unwrap();
                self.regs.reset(reg);
            } else {
                break;
            }
        }
    }

    fn spill_at_interval(&mut self, inst: ActiveVariable) {
        let spill = self.active.last().unwrap().clone();
        if spill.end > inst.end {
            let reg = self.allocation.remove(&spill.value).unwrap().clone();
            self.allocation.insert(inst.value, reg);
            assert!(self.active.remove(&spill));
            assert!(self.active.insert(inst));
        }
    }

    pub fn get_occupied_registers(&self, value: Value) -> Vec<Register> {
        let mut registers = Vec::new();
        let id = self.lifetime.index.get(&value).unwrap().clone();
        for inst in self.lifetime.insts.iter().cloned() {
            if inst.start > id {
                break;
            }
            if inst.end >= id {
                if let Some(r) = self.allocation.get(&inst.value) {
                    registers.push(*r);
                }
            }
        }
        registers
    }
}
