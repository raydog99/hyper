use llvm_sys::prelude::*;
use llvm_sys::core::*;
use std::collections::HashMap;
use std::cmp::Ordering;

pub struct MatchManager {
    op_matches: HashMap<*const Operation, Vec<Operation::Match>>,
}

impl MatchManager {
    fn sort_by_output(a: &Operation::Match, b: &Operation::Match) -> Ordering {
        unsafe {
            let a_ptr = a.output as *const _ as usize;
            let b_ptr = b.output as *const _ as usize;
            a_ptr.cmp(&b_ptr)
        }
    }

    unsafe fn match_value(&mut self, v: LLVMValueRef) {
        for (op, matches) in &mut self.op_matches {
            (*op).match_(v, matches);
        }
    }

    pub unsafe fn create(insts: &[*const InstBinding], f: LLVMValueRef) -> Self {
        let mut mm = MatchManager { op_matches: HashMap::new() };
        for inst in insts {
            for lane_op in (&**inst).get_lane_ops() {
                let op = lane_op.get_operation();
                mm.op_matches.entry(op).or_insert_with(Vec::new);
            }
        }
        let mut bb = LLVMGetFirstBasicBlock(f);
        while !bb.is_null() {
            let mut inst = LLVMGetFirstInstruction(bb);
            while !inst.is_null() {
                mm.match_value(inst);
                inst = LLVMGetNextInstruction(inst);
            }
            bb = LLVMGetNextBasicBlock(bb);
        }
        for matches in mm.op_matches.values_mut() {
            matches.sort_by(Self::sort_by_output);
        }
        mm
    }

    pub unsafe fn create_with_instructions(insts: &[*const InstBinding], to_match: &[LLVMValueRef]) -> Self {
        let mut mm = MatchManager { op_matches: HashMap::new() };
        for inst in insts {
            for lane_op in (&**inst).get_lane_ops() {
                let op = lane_op.get_operation();
                mm.op_matches.entry(op).or_insert_with(Vec::new);
            }
        }
        for inst in to_match {
            mm.match_value(*inst);
        }
        for matches in mm.op_matches.values_mut() {
            matches.sort_by(Self::sort_by_output);
        }
        mm
    }

    pub fn get_matches(&self, op: *const Operation) -> &[Operation::Match] {
        self.op_matches.get(&op).map(|v| v.as_slice()).unwrap_or(&[])
    }

    pub fn get_matches_for_output(&self, op: *const Operation, output: LLVMValueRef) -> &[Operation::Match] {
        let matches = self.get_matches(op);
        let dummy_match = Operation::Match { live_in: false, operands: vec![], output };
        let lower = matches.partition_point(|m| Self::sort_by_output(m, &dummy_match) == Ordering::Less);
        let upper = matches.partition_point(|m| Self::sort_by_output(m, &dummy_match) != Ordering::Greater);
        &matches[lower..upper]
    }
}