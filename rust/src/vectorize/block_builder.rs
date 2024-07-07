use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

use llvm_sys::prelude::*;
use llvm_sys::core::*;

pub enum ControlCondition {
    And(ConditionAnd),
    Or(ConditionOr),
}

pub struct ConditionAnd {
    parent: Option<Rc<ControlCondition>>,
    cond: LLVMValueRef,
    is_true: bool,
    complement: Option<Rc<ControlCondition>>,
}

pub struct ConditionOr {
    conds: Vec<Rc<ControlCondition>>,
    greatest_common_cond: Option<Rc<ControlCondition>>,
}

pub struct BlockBuilder {
    ctx: LLVMContextRef,
    function: LLVMValueRef,
    emit_condition: Box<dyn Fn(LLVMValueRef) -> LLVMValueRef>,
    active_conds: HashMap<Option<Rc<ControlCondition>>, LLVMBasicBlockRef>,
    semi_active_conds: HashMap<Rc<ControlCondition>, Vec<Rc<ControlCondition>>>,
    dummy_counter: i32,
}

impl BlockBuilder {
    pub fn new(entry_bb: LLVMBasicBlockRef, emit_condition: Box<dyn Fn(LLVMValueRef) -> LLVMValueRef>) -> Self {
        unsafe {
            let ctx = LLVMGetModuleContext(LLVMGetGlobalParent(entry_bb));
            let function = LLVMGetBasicBlockParent(entry_bb);
            let mut active_conds = HashMap::new();
            active_conds.insert(None, entry_bb);
            
            BlockBuilder {
                ctx,
                function,
                emit_condition,
                active_conds,
                semi_active_conds: HashMap::new(),
                dummy_counter: 0,
            }
        }
    }

    pub fn create_block(&self) -> LLVMBasicBlockRef {
        unsafe {
            LLVMAppendBasicBlockInContext(self.ctx, self.function, b"\0".as_ptr() as *const _)
        }
    }

    pub fn get_block_for(&mut self, c: Option<Rc<ControlCondition>>) -> LLVMBasicBlockRef {
        if let Some(block) = self.active_conds.get(&c) {
            return *block;
        }

        match c {
            Some(cond) => {
                if self.semi_active_conds.contains_key(&cond) {
                    self.handle_semi_active_cond(cond)
                } else {
                    match &*cond {
                        ControlCondition::And(and) => self.handle_condition_and(and),
                        ControlCondition::Or(or) => self.handle_condition_or(or),
                    }
                }
            }
            None => panic!("Unexpected None condition"),
        }
    }

    fn handle_semi_active_cond(&mut self, c: Rc<ControlCondition>) -> LLVMBasicBlockRef {
        let conds = self.get_active_conds(&c);
        let new_bb = self.create_block();
        
        for c2 in conds {
            let old_bb = self.active_conds.remove(&Some(c2.clone())).unwrap();
            unsafe {
                LLVMBuildBr(LLVMCreateBuilderInContext(self.ctx), new_bb);
            }
        }

        self.active_conds.insert(Some(c), new_bb);
        new_bb
    }

    fn handle_condition_and(&mut self, and: &ConditionAnd) -> LLVMBasicBlockRef {
        let if_true = self.create_block();
        let if_false = self.create_block();
        let parent_bb = self.get_block_for(and.parent.clone());
        let cond = (self.emit_condition)(and.cond);
        
        unsafe {
            let builder = LLVMCreateBuilderInContext(self.ctx);
            LLVMPositionBuilderAtEnd(builder, parent_bb);
            LLVMBuildCondBr(builder, cond, if_true, if_false);
            LLVMDisposeBuilder(builder);
        }

        let result_bb = if and.is_true { if_true } else { if_false };
        
        self.active_conds.remove(&and.parent);
        self.semi_active_conds.insert(and.parent.clone().unwrap(), vec![Rc::new(ControlCondition::And(and.clone())), and.complement.clone().unwrap()]);
        self.active_conds.insert(Some(Rc::new(ControlCondition::And(and.clone()))), result_bb);
        self.active_conds.insert(Some(and.complement.clone().unwrap()), if and.is_true { if_false } else { if_true });
        
        result_bb
    }

    fn handle_condition_or(&mut self, or: &ConditionOr) -> LLVMBasicBlockRef {
        let common_c = or.greatest_common_cond.clone();
        let conds = if !self.semi_active_conds.contains_key(&common_c.clone().unwrap()) {
            self.get_block_for(common_c.clone());
            vec![common_c.unwrap()]
        } else {
            self.get_active_conds(&common_c.unwrap())
        };

        let new_bb = self.create_block();
        let aux_bb = self.create_block();
        let conds_to_join: HashMap<_, _> = or.conds.iter().map(|c| (c.clone(), ())).collect();
        let mut joined = HashMap::new();

        for c2 in conds {
            let old_bb = self.active_conds.remove(&Some(c2.clone())).unwrap();
            unsafe {
                let builder = LLVMCreateBuilderInContext(self.ctx);
                LLVMPositionBuilderAtEnd(builder, old_bb);
                if conds_to_join.contains_key(&c2) {
                    LLVMBuildBr(builder, new_bb);
                    joined.insert(c2.clone(), ());
                } else {
                    LLVMBuildBr(builder, aux_bb);
                }
                LLVMDisposeBuilder(builder);
            }
        }

        let unjoined_conds: Vec<_> = or.conds.iter()
            .filter(|c| !joined.contains_key(c))
            .cloned()
            .collect();

        let drain_bb = self.create_block();
        unsafe {
            let builder = LLVMCreateBuilderInContext(self.ctx);
            LLVMPositionBuilderAtEnd(builder, aux_bb);
            if unjoined_conds.is_empty() {
                LLVMBuildBr(builder, drain_bb);
            } else {
                let ce = ConditionEmitter::new(builder, common_c.clone(), &self.emit_condition);
                let cond = ce.emit_disjunction(&unjoined_conds);
                LLVMBuildCondBr(builder, cond, new_bb, drain_bb);
            }
            LLVMDisposeBuilder(builder);
        }

        let dummy_c = self.get_dummy_condition();
        self.active_conds.insert(Some(Rc::new(ControlCondition::Or(or.clone()))), new_bb);
        self.active_conds.insert(Some(dummy_c.clone()), drain_bb);
        self.semi_active_conds.insert(common_c.unwrap(), vec![Rc::new(ControlCondition::Or(or.clone())), dummy_c]);
        self.active_conds.remove(&common_c);

        new_bb
    }

    fn get_active_conds(&self, c: &ControlCondition) -> Vec<Rc<ControlCondition>> {
        let mut visited = HashMap::new();
        let mut conds = Vec::new();

        fn dfs(c: &ControlCondition, visited: &mut HashMap<Rc<ControlCondition>, ()>, conds: &mut Vec<Rc<ControlCondition>>, bb: &BlockBuilder) {
            if visited.contains_key(c) {
                return;
            }
            visited.insert(Rc::new(c.clone()), ());
            if bb.active_conds.contains_key(&Some(Rc::new(c.clone()))) {
                conds.push(Rc::new(c.clone()));
                return;
            }
            if let Some(children) = bb.semi_active_conds.get(&Rc::new(c.clone())) {
                for child in children {
                    dfs(&*child, visited, conds, bb);
                }
            }
        }

        dfs(c, &mut visited, &mut conds, self);
        conds
    }

    fn get_dummy_condition(&mut self) -> Rc<ControlCondition> {
        self.dummy_counter += 1;
        Rc::new(ControlCondition::And(ConditionAnd {
            parent: None,
            cond: unsafe { LLVMConstInt(LLVMInt1TypeInContext(self.ctx), self.dummy_counter as u64, 0) },
            is_true: true,
            complement: None,
        }))
    }

    pub fn set_block_for_condition(&mut self, block: LLVMBasicBlockRef, c: Rc<ControlCondition>) {
        assert!(self.active_conds.contains_key(&Some(c.clone())), "Can only set block for active condition");
        self.active_conds.insert(Some(c), block);
    }
}

struct ConditionEmitter<'a> {
    builder: LLVMBuilderRef,
    common: Option<Rc<ControlCondition>>,
    emit_condition: &'a dyn Fn(LLVMValueRef) -> LLVMValueRef,
    emitted: RefCell<HashMap<Rc<ControlCondition>, LLVMValueRef>>,
}

impl<'a> ConditionEmitter<'a> {
    fn new(builder: LLVMBuilderRef, common: Option<Rc<ControlCondition>>, emit_condition: &'a dyn Fn(LLVMValueRef) -> LLVMValueRef) -> Self {
        ConditionEmitter {
            builder,
            common,
            emit_condition,
            emitted: RefCell::new(HashMap::new()),
        }
    }

    fn emit(&self, c: &Option<Rc<ControlCondition>>) -> LLVMValueRef {
        match c {
            None | Some(ref c) if self.common.as_ref() == Some(c) => unsafe {
                LLVMConstInt(LLVMInt1TypeInContext(LLVMGetGlobalContext()), 1, 0)
            },
            Some(cond) => match &**cond {
                ControlCondition::And(and) => self.emit_and(and),
                ControlCondition::Or(or) => self.emit_or(or),
            },
        }
    }

    fn emit_and(&self, and: &ConditionAnd) -> LLVMValueRef {
        if let Some(v) = self.emitted.borrow().get(&Rc::new(ControlCondition::And(and.clone()))) {
            return *v;
        }
        let parent = self.emit(&and.parent);
        let cond = (self.emit_condition)(and.cond);
        let v = unsafe {
            if and.is_true {
                LLVMBuildAnd(self.builder, parent, cond, b"\0".as_ptr() as *const _)
            } else {
                let not_cond = LLVMBuildNot(self.builder, cond, b"\0".as_ptr() as *const _);
                LLVMBuildAnd(self.builder, parent, not_cond, b"\0".as_ptr() as *const _)
            }
        };
        self.emitted.borrow_mut().insert(Rc::new(ControlCondition::And(and.clone())), v);
        v
    }

    fn emit_or(&self, or: &ConditionOr) -> LLVMValueRef {
        if let Some(v) = self.emitted.borrow().get(&Rc::new(ControlCondition::Or(or.clone()))) {
            return *v;
        }
        let v = self.emit_disjunction(&or.conds);
        self.emitted.borrow_mut().insert(Rc::new(ControlCondition::Or(or.clone())), v);
        v
    }

    fn emit_disjunction(&self, conds: &[Rc<ControlCondition>]) -> LLVMValueRef {
        let values: Vec<LLVMValueRef> = conds.iter().map(|c| self.emit(&Some(c.clone()))).collect();
        unsafe {
            LLVMBuildOr(self.builder, values.as_ptr(), values.len() as u32, b"\0".as_ptr() as *const _)
        }
    }
}