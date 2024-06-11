use std::{
    cmp::{max, min},
    collections::{BTreeSet, HashMap},
};

use koopa::ir::{FunctionData, TypeKind, Value, ValueKind};

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

/// A wrapper around [`koopa::ir::entities::ValueData::used_by`].
/// This records the lifetime info of a [`Value`].
#[derive(Debug, Clone)]
pub struct LifeTime {
    pub index: HashMap<Value, u32>,
    pub interval: HashMap<Value, (u32, u32)>,
    pub pre_alloc: HashMap<Value, Register>,
    insts: Vec<ActiveVariable>,
}

impl LifeTime {
    pub fn new(func: &FunctionData) -> Self {
        let mut index = HashMap::new();
        let mut interval = HashMap::new();
        let mut alloc_in_s = HashMap::new();
        let mut pre_alloc = HashMap::new();
        let mut i = 1;
        for (_, bb) in func.layout().bbs() {
            for (value, _) in bb.insts() {
                index.insert(value.clone(), i);
                i += 1;
            }
        }
        for (_, bb) in func.layout().bbs() {
            for (value, _) in bb.insts() {
                if func.dfg().value(*value).ty().is_unit() {
                    continue;
                }
                if matches!(func.dfg().value(*value).kind(), ValueKind::Alloc(_)) {
                    if func.dfg().value(*value).name().is_some() {
                        let size = match func.dfg().value(*value).ty().kind() {
                            TypeKind::Pointer(value) => value.size(),
                            _ => unreachable!(),
                        };
                        if size == 4 && alloc_in_s.len() < 11 {
                            let reg = Register::S[1 + alloc_in_s.len()];
                            alloc_in_s.insert(*value, reg);
                            pre_alloc.insert(*value, reg);
                        }
                        continue;
                    }
                }
                if let ValueKind::Load(load) = func.dfg().value(*value).kind() {
                    if let Some(reg) = alloc_in_s.get(&load.src()) {
                        pre_alloc.insert(*value, *reg);
                    }
                }
                let mut l = u32::MAX;
                let mut r = 0u32;
                for friend in func.dfg().value(*value).used_by() {
                    let id = *index.get(friend).unwrap();
                    l = min(l, id);
                    r = max(r, id);
                }
                interval.insert(value.clone(), (min(l, *index.get(value).unwrap()), r));
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
            pre_alloc,
        }
    }
}

/// Inspired by [LSRA](https://en.wikipedia.org/wiki/Live_variable_analysis).
#[derive(Debug)]
pub struct Allocator {
    pub allocation: HashMap<Value, Register>,
    pub lifetime: LifeTime,
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
                pre_alloc: HashMap::new(),
            },
            active: BTreeSet::new(),
            allocation: HashMap::new(),
            regs: RegGroup::new_store(),
        };
        for inst in lifetime.insts.iter().cloned() {
            allocator.expire_old_intervals(inst);
            if allocator.active.len() == allocator.regs.num() {
                println!("Warning: variable spilled on stack");
                allocator.spill_at_interval(inst);
            } else {
                let reg = allocator.regs.get_vaccant();
                allocator.allocation.insert(inst.value, reg);
                assert!(allocator.active.insert(inst));
            }
        }
        for (k, v) in lifetime.pre_alloc.iter() {
            allocator.allocation.insert(*k, *v);
        }
        allocator.lifetime = lifetime;
        allocator
    }

    fn expire_old_intervals(&mut self, inst: ActiveVariable) {
        loop {
            if let Some(&var) = self.active.first() {
                // Different from the reference, I changed ">=" into ">",
                // which may have a significant change on the generated code.
                if var.end > inst.start {
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

    pub fn get_occupied_registers(&self, value: Value, args: &[Value]) -> Vec<(Register, bool)> {
        let mut registers = Vec::new();
        let id = self.lifetime.index.get(&value).unwrap().clone();
        for inst in self.lifetime.insts.iter().cloned() {
            if inst.start >= id {
                break;
            }
            if inst.end >= id {
                if let Some(r) = self.allocation.get(&inst.value) {
                    if let Some(i) = Register::A.iter().position(|reg| reg == r) {
                        if i < args.len() && self.allocation.get(&args[i]) == Some(r) {
                            continue;
                        }
                    }
                    registers.push((*r, inst.end > id));
                }
            }
        }
        registers
    }
}
