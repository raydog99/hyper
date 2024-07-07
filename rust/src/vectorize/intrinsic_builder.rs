use llvm_sys::prelude::*;
use llvm_sys::core::*;
use std::ffi::CString;
use std::collections::HashMap;

pub struct IntrinsicBuilder {
    inst_wrappers: LLVMModuleRef,
    builder: LLVMBuilderRef,
}

impl IntrinsicBuilder {
    pub unsafe fn create(&self, name: &str, operands: &[LLVMValueRef], imm8: u8) -> LLVMValueRef {
        let wrapper_name = format!("intrinsic_wrapper_{}_{}", name, imm8);
        let wrapper_name_c = CString::new(wrapper_name).unwrap();
        let f = LLVMGetNamedFunction(self.inst_wrappers, wrapper_name_c.as_ptr());
        assert!(!f.is_null(), "Intrinsic wrapper undefined.");

        let bb = LLVMGetFirstBasicBlock(f);
        assert!(!bb.is_null() && LLVMGetNextBasicBlock(bb).is_null(), 
                "Intrinsic Wrapper should have a single basic block");

        let num_args = LLVMCountParams(f);
        assert_eq!(operands.len(), num_args as usize);

        let mut v_map = HashMap::new();
        for (i, &arg) in LLVMGetParams(f).iter().enumerate() {
            assert!(LLVMCastIsValid(LLVMBitCast, operands[i], LLVMTypeOf(arg)) != 0,
                    "Invalid input type");
            let operand = LLVMBuildBitCast(self.builder, operands[i], LLVMTypeOf(arg), b"\0".as_ptr() as *const _);
            v_map.insert(arg, operand);
        }

        let mut ret_val = None;
        let mut inst = LLVMGetFirstInstruction(bb);
        while !inst.is_null() {
            if LLVMGetInstructionOpcode(inst) == LLVMRet {
                ret_val = Some(LLVMGetReturnValue(inst));
                break;
            }
            let new_i = LLVMInstructionClone(inst);
            LLVMInsertIntoBuilder(self.builder, new_i);
            v_map.insert(inst, new_i);
            LLVMRemapInstructionInPlace(new_i, |old_val| {
                v_map.get(&old_val).cloned().unwrap_or(old_val)
            }, LLVMRemapInstructionFlags::LLVMRemapIgnoreMissingLocals as u32);

            if LLVMGetInstructionOpcode(new_i) == LLVMCall {
                let callee = LLVMGetCalledValue(new_i);
                if LLVMIsAFunction(callee) != std::ptr::null_mut() {
                    let callee_func = LLVMValueAsFunction(callee);
                    assert!(LLVMIsFunctionVarArg(callee_func) != 0, "Called function must be an intrinsic");
                    let m = LLVMGetGlobalParent(LLVMGetInstructionParent(new_i));
                    let intrinsic_decl = LLVMAddFunction(m, LLVMGetValueName(callee_func), LLVMGetElementType(LLVMTypeOf(callee_func)));
                    LLVMSetFunctionCallConv(intrinsic_decl, LLVMGetFunctionCallConv(callee_func));
                    LLVMSetInstructionCallConv(new_i, LLVMGetFunctionCallConv(callee_func));
                    LLVMSetCalledValue(new_i, intrinsic_decl);
                }
            }

            inst = LLVMGetNextInstruction(inst);
        }

        let ret_val = ret_val.expect("Wrapper not returning explicitly");
        let output = v_map.get(&ret_val).expect("Return value not found in value map");
        *output
    }
}