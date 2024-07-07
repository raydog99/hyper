use std::collections::HashMap;
use std::collections::HashSet;
use llvm_sys::prelude::*;
use llvm_sys::core::*;
use crate::packer::Packer;
use crate::solver::Solver;
use crate::vector_pack::{VectorPack, OperandPack, VectorPackContext};

const C_SPLAT: f32 = 1.0;
const C_PERM: f32 = 2.0;
const C_INSERT: f32 = 2.0;
const C_SHUFFLE: f32 = 2.0;
const C_EXTRACT: f32 = 1.0;

static mut ALLOW_DEINTERLEAVE: bool = false;
static mut ALLOW_TRANSPOSE: bool = false;

pub struct Solution {
    cost: f32,
    packs: Vec<*mut VectorPack>,
}

pub struct Heuristic {
    solutions: HashMap<*mut OperandPack, Solution>,
    scalar_costs: HashMap<LLVMValueRef, f32>,
    pkr: *mut Packer,
}

impl Heuristic {
    pub fn new(pkr: *mut Packer) -> Self {
        Heuristic {
            solutions: HashMap::new(),
            scalar_costs: HashMap::new(),
            pkr,
        }
    }

    pub unsafe fn get_cost(&self, vp: *mut VectorPack) -> f32 {
        let mut cost = (*vp).get_producing_cost();
        for op in (*vp).get_operand_packs() {
            if !Self::all_cmp_inst(op) {
                cost += self.get_cost(op);
            }
        }
        cost
    }

    fn all_cmp_inst(op: *mut OperandPack) -> bool {
        unsafe {
            (*op).iter().all(|v| LLVMIsAICmp(*v) != std::ptr::null_mut())
        }
    }

    unsafe fn deinterleave(vp_ctx: *mut VectorPackContext, op: *mut OperandPack, stride: u32) -> Vec<*mut OperandPack> {
        vec![]
    }

    unsafe fn transpose(vp_ctx: *mut VectorPackContext, op: *mut OperandPack, n: u32) -> Option<*mut OperandPack> {
        if (*op).size() % n != 0 {
            return None;
        }
        let m = (*op).size() / n;
        let mut t = Vec::with_capacity((*op).size() as usize);
        for i in 0..m {
            for j in 0..n {
                t.push((*op).get(j * m + i));
            }
        }
        Some((*vp_ctx).get_canonical_operand_pack(&t))
    }

    pub unsafe fn solve(&mut self, op: *mut OperandPack) -> Solution {
        if let Some(sol) = self.solutions.get(&op) {
            return sol.clone();
        }

        self.solutions.insert(op, Solution { cost: 0.0, packs: vec![] });

        let mut cost = 0.0;
        let mut inserted = HashSet::new();
        for (i, v) in (*op).iter().enumerate() {
            if !LLVMIsAConstant(*v) && inserted.insert(*v) {
                cost += self.get_cost_value(*v) + C_INSERT;
            }
        }

        let mut sol = Solution { cost, packs: vec![] };
        if cost == 0.0 {
            self.solutions.insert(op, sol.clone());
            return sol;
        }

        let broadcast_cost = self.get_cost_value((*op).front()) + C_SPLAT;
        if (*op).is_splat() && cost > broadcast_cost {
            sol = Solution { cost: broadcast_cost, packs: vec![] };
            self.solutions.insert(op, sol.clone());
            return sol;
        }

        let vp_ctx = (*self.pkr).get_context();
        let deduped = (*vp_ctx).dedup(op);
        let extra_cost = if deduped != op { C_SHUFFLE } else { 0.0 };

        let opi = (*self.pkr).get_producer_info(deduped);
        for vp in (*opi).get_producers() {
            let new_sol = Solution { cost: self.get_cost(vp) + extra_cost, packs: vec![vp] };
            if new_sol.cost < sol.cost {
                sol = new_sol;
            }
        }

        if ALLOW_TRANSPOSE {
            for n in &[2, 4, 8] {
                if let Some(t) = Self::transpose(vp_ctx, op, *n) {
                    let opi = (*self.pkr).get_producer_info(t);
                    for vp in (*opi).get_producers() {
                        let new_sol = Solution { cost: self.get_cost(vp) + C_PERM, packs: vec![vp] };
                        if new_sol.cost < sol.cost {
                            sol = new_sol;
                        }
                    }
                }
            }
        }

        if ALLOW_DEINTERLEAVE {
            for stride in &[2, 4, 8] {
                if (*deduped).size() % stride == 0 {
                    let ops = Self::deinterleave(vp_ctx, deduped, *stride);
                    let mut cost = C_SHUFFLE * ops.len() as f32;
                    let mut packs = vec![];
                    for op2 in ops {
                        let sol2 = self.solve(op2);
                        packs.extend_from_slice(&sol2.packs);
                        cost += sol2.cost;
                        if cost > sol.cost {
                            break;
                        }
                    }
                    if cost < sol.cost {
                        sol = Solution { cost, packs };
                    }
                }
            }
        }

        self.solutions.insert(op, sol.clone());
        sol
    }

    unsafe fn get_cost_value(&mut self, v: LLVMValueRef) -> f32 {
        if LLVMIsAInstruction(v) == std::ptr::null_mut() {
            return 0.0;
        }

        if let Some(&cost) = self.scalar_costs.get(&v) {
            return cost;
        }

        self.scalar_costs.insert(v, 0.0);
        let mut cost = (*self.pkr).get_scalar_cost(v);
        for i in 0..LLVMGetNumOperands(v) {
            let op = LLVMGetOperand(v, i);
            cost += self.get_cost_value(op);
        }
        self.scalar_costs.insert(v, cost);
        cost
    }
}