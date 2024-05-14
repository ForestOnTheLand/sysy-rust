use std::{
    cmp::{max, min},
    collections::{BTreeSet, HashMap},
};

use koopa::ir::{FunctionData, Value};

use crate::translate_util::{RegGroup, Register};

#[derive(Eq, Clone, Copy)]
struct ActiveVariable {
    value: Value,
    start: u32,
    end: u32,
}

impl PartialEq for ActiveVariable {
    fn eq(&self, other: &Self) -> bool {
        self.end == other.end
    }
}

impl PartialOrd for ActiveVariable {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.end.partial_cmp(&other.end)
    }
}

impl Ord for ActiveVariable {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.end.cmp(&other.end)
    }
}

pub struct LifeTime {
    index: HashMap<Value, u32>,
    interval: HashMap<Value, (u32, u32)>,
    insts: Vec<ActiveVariable>,
}

impl LifeTime {
    fn new(func: &FunctionData) -> Self {
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

pub struct Allocator {
    active: BTreeSet<ActiveVariable>,
    allocation: HashMap<Value, Register>,
    regs: RegGroup,
}

impl Allocator {
    pub fn linear_scan_register_allocation(lifetime: &LifeTime) -> Self {
        let mut allocator = Allocator {
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
                allocator.active.insert(inst);
            }
        }
        allocator
    }

    fn expire_old_intervals(&mut self, inst: ActiveVariable) {
        loop {
            if let Some(&var) = self.active.first() {
                if var.end >= inst.start {
                    return;
                }
                self.active.pop_first();
                self.regs.reset(*self.allocation.get(&var.value).unwrap());
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
            self.active.remove(&spill);
            self.active.insert(inst);
        }
    }
}
