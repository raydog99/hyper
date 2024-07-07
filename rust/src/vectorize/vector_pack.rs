use llvm_sys::prelude::*;
use llvm_sys::core::*;
use std::collections::HashMap;

pub struct VectorPack {
    kind: PackKind,
    producer: Option<InstBinding>,
    matches: Vec<Option<Operation::Match>>,
    loads: Vec<LLVMValueRef>,
    stores: Vec<LLVMValueRef>,
    phis: Vec<LLVMValueRef>,
    rdx: Option<ReductionInfo>,
    rdx_len: usize,
    geps: Vec<LLVMValueRef>,
    gammas: Vec<Gamma>,
    cmps: Vec<LLVMValueRef>,
    cp: Option<ConditionPack>,
    is_gather_scatter: bool,
    operand_packs: Vec<OperandPack>,
    ordered_values: Vec<LLVMValueRef>,
    cost: f64,
    producing_cost: f64,
    vp_ctx: VectorPackContext,
}

impl VectorPack {
    pub unsafe fn compute_operand_packs_for_general(&self) -> Vec<OperandPack> {
        let sig = self.producer.as_ref().unwrap().get_signature();
        let num_inputs = sig.num_inputs();
        let lane_ops = self.producer.as_ref().unwrap().get_lane_ops();
        let num_lanes = lane_ops.len();
        let mut operand_packs = vec![OperandPack::new(); num_inputs];

        for i in 0..num_inputs {
            let mut input_values = Vec::new();
            let mut element_size = 0;
            for j in 0..num_lanes {
                let bound_slices = lane_ops[j].get_bound_slices();
                for (k, bs) in bound_slices.iter().enumerate() {
                    if bs.input_id == i {
                        element_size = bs.size();
                        let v = self.matches[j].as_ref().map(|m| m.inputs[k]).unwrap_or(std::ptr::null_mut());
                        input_values.push(BoundInput { slice: bs.clone(), v });
                    }
                }
            }
            assert!(element_size != 0);

            input_values.sort_by_key(|bv| bv.slice.lo);

            let mut cur_offset = 0;
            let stride = input_values[0].slice.size();
            let op = &mut operand_packs[i];
            for bv in input_values {
                while cur_offset < bv.slice.lo {
                    op.push(std::ptr::null_mut());
                    cur_offset += stride;
                }
                assert_eq!(cur_offset, bv.slice.lo);
                op.push(bv.v);
                cur_offset += stride;
            }
            let input_size = sig.input_bitwidths[i];
            while cur_offset < input_size {
                op.push(std::ptr::null_mut());
                cur_offset += stride;
            }
            assert_eq!(op.size() * stride, input_size);

            if op.front().is_null() && op.is_splat() {
                let element_type = LLVMIntTypeInContext(self.vp_ctx.get_context(), element_size as u32);
                op.ty = Some(LLVMVectorType(element_type, op.size() as u32));
            }
        }

        operand_packs
    }
}