use llvm_sys::prelude::*;
use llvm_sys::core::*;
use std::ffi::CString;

#[derive(Clone, Copy)]
pub struct BinaryIROperation {
    opcode: LLVMOpcode,
    bitwidth: u32,
}

impl BinaryIROperation {
    pub fn match_op(&self, v: LLVMValueRef, matches: &mut Vec<Match>) -> bool {
        unsafe {
            if LLVMGetInstructionOpcode(v) == self.opcode && has_bit_width(v, self.bitwidth) {
                let operands = [LLVMGetOperand(v, 0), LLVMGetOperand(v, 1)];
                matches.push(Match {
                    live_in: false,
                    operands: operands.to_vec(),
                    result: v,
                });
                true
            } else {
                false
            }
        }
    }

    pub fn get_maximum_vf(&self, tti: LLVMTargetTransformInfoRef) -> u32 {
        unsafe {
            LLVMGetLoadStoreVecRegBitWidth(tti, 0) / self.bitwidth
        }
    }

    pub fn get_name(&self) -> String {
        unsafe {
            let opcode_name = CString::new(LLVMGetOpcodeName(self.opcode)).unwrap();
            format!("{}-i{}", opcode_name.to_str().unwrap(), self.bitwidth)
        }
    }
}

#[derive(Clone, Copy)]
pub struct UnaryIROperation {
    opcode: LLVMOpcode,
    bitwidth: u32,
}

impl UnaryIROperation {
    pub fn match_op(&self, v: LLVMValueRef, matches: &mut Vec<Match>) -> bool {
        unsafe {
            if LLVMGetInstructionOpcode(v) == self.opcode && has_bit_width(v, self.bitwidth) {
                let operands = [LLVMGetOperand(v, 0)];
                matches.push(Match {
                    live_in: false,
                    operands: operands.to_vec(),
                    result: v,
                });
                true
            } else {
                false
            }
        }
    }

    pub fn get_maximum_vf(&self, tti: LLVMTargetTransformInfoRef) -> u32 {
        unsafe {
            LLVMGetLoadStoreVecRegBitWidth(tti, 0) / self.bitwidth
        }
    }

    pub fn get_name(&self) -> String {
        unsafe {
            let opcode_name = CString::new(LLVMGetOpcodeName(self.opcode)).unwrap();
            format!("{}-{}", opcode_name.to_str().unwrap(), self.bitwidth)
        }
    }
}

pub struct IRVectorBinding {
    op: BinaryIROperation,
    name: String,
    sig: InstSignature,
    lane_ops: Vec<BoundOperation>,
}

impl IRVectorBinding {
    pub fn get_cost(&self, tti: LLVMTargetTransformInfoRef, ctx: LLVMContextRef) -> f32 {
        unsafe {
            let scalar_ty = if is_float(self.op.opcode) {
                if self.op.bitwidth == 32 {
                    LLVMFloatTypeInContext(ctx)
                } else {
                    LLVMDoubleTypeInContext(ctx)
                }
            } else {
                LLVMIntTypeInContext(ctx, self.op.bitwidth)
            };
            let num_elems = self.lane_ops.len() as u32;
            let vec_ty = LLVMVectorType(scalar_ty, num_elems);
            LLVMGetArithmeticInstrCost(tti, self.op.opcode, vec_ty)
        }
    }

    pub fn create(op: BinaryIROperation, vector_width: u32) -> Self {
        let sig = InstSignature {
            input_widths: vec![vector_width, vector_width],
            output_widths: vec![vector_width],
            has_imm8: false,
        };
        let elem_width = op.bitwidth;
        let num_lanes = vector_width / elem_width;
        let lane_ops = (0..num_lanes).map(|i| {
            let lo = i * elem_width;
            let hi = lo + elem_width;
            BoundOperation {
                op: op,
                input_bindings: vec![
                    InputBinding { input_idx: 0, lo, hi },
                    InputBinding { input_idx: 1, lo, hi },
                ],
            }
        }).collect();
        IRVectorBinding {
            op,
            name: op.get_name(),
            sig,
            lane_ops,
        }
    }

    pub unsafe fn emit(&self, operands: &[LLVMValueRef], builder: LLVMBuilderRef) -> LLVMValueRef {
        assert_eq!(operands.len(), 2);
        LLVMBuildBinOp(builder, self.op.opcode, operands[0], operands[1], b"\0".as_ptr() as *const _)
    }

    pub fn is_supported(&self, tti: LLVMTargetTransformInfoRef) -> bool {
        self.lane_ops.len() as u32 <= self.op.get_maximum_vf(tti)
    }
}