use std::collections::HashMap;
use llvm_sys::prelude::*;
use llvm_sys::core::*;

pub struct ControlReifier {
    ctx: LLVMContextRef,
    reified_values: HashMap<(ControlCondition, VLoop), LLVMValueRef>,
    inserted_insts: Vec<LLVMValueRef>,
}

impl ControlReifier {
    pub fn new(ctx: LLVMContextRef) -> Self {
        ControlReifier {
            ctx,
            reified_values: HashMap::new(),
            inserted_insts: Vec::new(),
        }
    }

    pub unsafe fn reify(&mut self, c: Option<&ControlCondition>, vl: &mut VLoop) -> LLVMValueRef {
        match c {
            None => LLVMConstInt(LLVMInt1TypeInContext(self.ctx), 1, 0),
            Some(c) => {
                if let Some(&v) = self.reified_values.get(&(c.clone(), vl.clone())) {
                    return v;
                }

                let reified = match c {
                    ControlCondition::And(and) => {
                        self.reify(and.parent.as_ref(), vl);
                        let mut cond = and.cond;
                        if !and.is_true {
                            let not = LLVMBuildNot(vl.builder, cond, b"not\0".as_ptr() as *const _);
                            self.inserted_insts.push(not);
                            vl.add_instruction(not, and.parent.as_ref());
                            cond = not;
                        }
                        vl.create_one_hot_phi(and.parent.as_ref(), cond, LLVMConstInt(LLVMInt1TypeInContext(self.ctx), 0, 0), b"reified.onehot\0".as_ptr() as *const _)
                    },
                    ControlCondition::Or(or) => {
                        let mut reified = self.reify(Some(&or.conds[0]), vl);
                        for c2 in &or.conds[1..] {
                            let tmp = LLVMBuildOr(vl.builder, reified, self.reify(Some(c2), vl), b"or\0".as_ptr() as *const _);
                            self.inserted_insts.push(tmp);
                            vl.add_instruction(tmp, None);
                            reified = tmp;
                        }
                        reified
                    },
                };

                self.reified_values.insert((c.clone(), vl.clone()), reified);

                if let ControlCondition::And(and) = c {
                    self.reify(and.complement.as_ref(), vl);
                }

                reified
            }
        }
    }

    pub fn has_value(&self, c: Option<&ControlCondition>, vl: &VLoop) -> bool {
        c.map_or(true, |c| self.reified_values.contains_key(&(c.clone(), vl.clone())))
    }

    pub fn get_value(&self, c: Option<&ControlCondition>, vl: &VLoop) -> LLVMValueRef {
        assert!(self.has_value(c, vl));
        match c {
            None => unsafe { LLVMConstInt(LLVMInt1TypeInContext(self.ctx), 1, 0) },
            Some(c) => *self.reified_values.get(&(c.clone(), vl.clone())).unwrap(),
        }
    }
}