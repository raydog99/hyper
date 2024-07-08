use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::builder::Builder;
use inkwell::values::{BasicValue, BasicValueEnum, InstructionValue, PhiValue, VectorValue};
use inkwell::types::{BasicType, BasicTypeEnum, VectorType};
use inkwell::basic_block::BasicBlock;
use std::collections::HashMap;

// Command line options
static mut SCALARIZE_VARIABLE_INSERT_EXTRACT: bool = true;
static mut SCALARIZE_LOAD_STORE: bool = false;

type ValueVector<'ctx> = Vec<BasicValueEnum<'ctx>>;
type ScatterMap<'ctx> = HashMap<BasicValueEnum<'ctx>, ValueVector<'ctx>>;
type GatherList<'ctx> = Vec<(InstructionValue<'ctx>, ValueVector<'ctx>)>;

#[derive(Debug, PartialEq)]
enum LoopUnrollResult {
    Unmodified,
    PartiallyUnrolled,
    FullyUnrolled,
}

struct UnrolledValue<'ctx> {
    iteration: u32,
    original: BasicValueEnum<'ctx>,
}

struct VectorLayout<'ctx> {
    vec_ty: VectorType<'ctx>,
    elem_ty: BasicTypeEnum<'ctx>,
    vec_align: u32,
    elem_size: u64,
}

struct Scalarizer<'ctx> {
    context: &'ctx Context,
    module: &'ctx Module<'ctx>,
    builder: &'ctx Builder<'ctx>,
    scattered: ScatterMap<'ctx>,
    gathered: GatherList<'ctx>,
    potentially_dead_instrs: Vec<InstructionValue<'ctx>>,
    parallel_loop_access_md_kind: u32,
}

impl<'ctx> Scalarizer<'ctx> {
    fn new(context: &'ctx Context, module: &'ctx Module<'ctx>, builder: &'ctx Builder<'ctx>) -> Self {
        Self {
            context,
            module,
            builder,
            scattered: HashMap::new(),
            gathered: Vec::new(),
            potentially_dead_instrs: Vec::new(),
            parallel_loop_access_md_kind: context.get_kind_id("llvm.mem.parallel_loop_access"),
        }
    }

    fn skip_past_phi_nodes_and_dbg(&self, mut iter: InstructionValue<'ctx>) -> InstructionValue<'ctx> {
        while let Some(inst) = iter.as_instruction() {
            match inst.get_opcode() {
                inkwell::values::InstructionOpcode::Phi => {
                    iter = unsafe { iter.get_next_instruction().unwrap() };
                }
                inkwell::values::InstructionOpcode::Call => {
                    if let Some(called_fn) = inst.get_called_function_value() {
                        if called_fn.get_name().to_str().unwrap() == "llvm.dbg.value" {
                            iter = unsafe { iter.get_next_instruction().unwrap() };
                            continue;
                        }
                    }
                    return iter;
                }
                _ => return iter,
            }
        }
        iter
    }

    fn scatter(&mut self, point: BasicBlock<'ctx>, v: BasicValueEnum<'ctx>) -> ValueVector<'ctx> {
        let scatter_helper = |bb: BasicBlock<'ctx>, v: BasicValueEnum<'ctx>| -> ValueVector<'ctx> {
            let ty = v.get_type();
            let size = match ty {
                BasicTypeEnum::VectorType(vt) => vt.get_size(),
                BasicTypeEnum::PointerType(pt) => {
                    if let BasicTypeEnum::VectorType(vt) = pt.get_element_type() {
                        vt.get_size()
                    } else {
                        panic!("Unexpected type in scatter_helper");
                    }
                }
                _ => panic!("Unexpected type in scatter_helper"),
            };

            let mut cache = self.scattered.entry(v).or_insert_with(|| vec![None; size as usize]);
            
            for i in 0..size {
                if cache[i as usize].is_none() {
                    let new_val = match ty {
                        BasicTypeEnum::PointerType(pt) => {
                            if i == 0 {
                                let new_ptr_ty = pt.get_element_type().into_vector_type(size).ptr_type(pt.get_address_space());
                                self.builder.build_bitcast(v, new_ptr_ty, &format!("{}.i0", v.get_name().to_str().unwrap()))
                            } else {
                                let gep = unsafe {
                                    self.builder.build_gep(v, &[self.context.i32_type().const_int(i as u64, false)], 
                                                           &format!("{}.i{}", v.get_name().to_str().unwrap(), i))
                                };
                                self.builder.build_load(gep, &format!("{}.i{}", v.get_name().to_str().unwrap(), i))
                            }
                        }
                        _ => self.builder.build_extract_element(
                            v.into_vector_value(),
                            self.context.i32_type().const_int(i as u64, false),
                            &format!("{}.i{}", v.get_name().to_str().unwrap(), i),
                        ),
                    };
                    cache[i as usize] = Some(new_val);
                }
            }
            cache.iter().map(|v| v.unwrap()).collect()
        };

        match v {
            BasicValueEnum::PointerValue(pv) => {
                if let Some(arg) = pv.as_instruction_value() {
                    let func = arg.get_parent().unwrap().get_parent().unwrap();
                    let entry = func.get_first_basic_block().unwrap();
                    scatter_helper(entry, v)
                } else {
                    scatter_helper(point, v)
                }
            }
            BasicValueEnum::InstructionValue(iv) => {
                let bb = iv.get_parent().unwrap();
                let iter = self.skip_past_phi_nodes_and_dbg(unsafe { iv.get_next_instruction().unwrap() });
                scatter_helper(bb, v)
            }
            _ => scatter_helper(point, v),
        }
    }

    fn gather(&mut self, op: InstructionValue<'ctx>, cv: ValueVector<'ctx>) {
        self.transfer_metadata_and_ir_flags(op, &cv);

        if let Some(sv) = self.scattered.get_mut(&BasicValueEnum::InstructionValue(op)) {
            for (i, v) in cv.iter().enumerate() {
                if let Some(old_v) = sv[i] {
                    if old_v != *v {
                        old_v.replace_all_uses_with(*v);
                        self.potentially_dead_instrs.push(old_v.into_instruction_value());
                    }
                }
            }
        }
        self.scattered.insert(BasicValueEnum::InstructionValue(op), cv.clone());
        self.gathered.push((op, cv));
    }

    fn can_transfer_metadata(&self, tag: u32) -> bool {
        tag == self.context.get_kind_id("tbaa")
            || tag == self.context.get_kind_id("fpmath")
            || tag == self.context.get_kind_id("tbaa.struct")
            || tag == self.context.get_kind_id("invariant.load")
            || tag == self.context.get_kind_id("alias.scope")
            || tag == self.context.get_kind_id("noalias")
            || tag == self.parallel_loop_access_md_kind
            || tag == self.context.get_kind_id("access_group")
    }

    fn transfer_metadata_and_ir_flags(&self, op: InstructionValue<'ctx>, cv: &ValueVector<'ctx>) {
        for v in cv {
            if let Some(new_inst) = v.as_instruction_value() {
                for &(kind, md) in op.get_metadata() {
                    if self.can_transfer_metadata(kind) {
                        new_inst.set_metadata(kind, md);
                    }
                }
                if let Some(debug_loc) = op.get_debug_loc() {
                    if new_inst.get_debug_loc().is_none() {
                        new_inst.set_debug_loc(debug_loc);
                    }
                }
            }
        }
    }

    fn get_vector_layout(&self, ty: BasicTypeEnum<'ctx>, alignment: u32) -> Option<VectorLayout<'ctx>> {
        if let BasicTypeEnum::VectorType(vt) = ty {
            let elem_ty = vt.get_element_type();
            if self.module.get_data_layout().get_type_alloc_size(&elem_ty)
                == self.module.get_data_layout().get_type_store_size(&elem_ty)
            {
                Some(VectorLayout {
                    vec_ty: vt,
                    elem_ty,
                    vec_align: alignment,
                    elem_size: self.module.get_data_layout().get_type_store_size(&elem_ty),
                })
            } else {
                None
            }
        } else {
            None
        }
    }

    fn visit_select_inst(&mut self, si: InstructionValue<'ctx>) -> bool {
        if let BasicTypeEnum::VectorType(vt) = si.get_type() {
            let num_elems = vt.get_size();
            let vop1 = self.scatter(si.get_parent().unwrap(), si.get_operand(1).unwrap());
            let vop2 = self.scatter(si.get_parent().unwrap(), si.get_operand(2).unwrap());
            let mut res = Vec::with_capacity(num_elems as usize);

            if let BasicTypeEnum::VectorType(_) = si.get_operand(0).unwrap().get_type() {
                let vop0 = self.scatter(si.get_parent().unwrap(), si.get_operand(0).unwrap());
                for i in 0..num_elems {
                    res.push(self.builder.build_select(
                        vop0[i as usize].into_int_value(),
                        vop1[i as usize],
                        vop2[i as usize],
                        &format!("{}.i{}", si.get_name().to_str().unwrap(), i),
                    ));
                }
            } else {
                let cond = si.get_operand(0).unwrap();
                for i in 0..num_elems {
                    res.push(self.builder.build_select(
                        cond.into_int_value(),
                        vop1[i as usize],
                        vop2[i as usize],
                        &format!("{}.i{}", si.get_name().to_str().unwrap(), i),
                    ));
                }
            }
            self.gather(si, res);
            true
        } else {
            false
        }
    }

    // Implement other visit_* methods here (visit_cmp_inst, visit_binary_operator, etc.)

    fn visit_instruction(&mut self, inst: InstructionValue<'ctx>) -> bool {
        match inst.get_opcode() {
            inkwell::values::InstructionOpcode::Select => self.visit_select_inst(inst),
            inkwell::values::InstructionOpcode::ICmp => self.visit_cmp_inst(inst, false),
            inkwell::values::InstructionOpcode::FCmp => self.visit_cmp_inst(inst, true),
            inkwell::values::InstructionOpcode::Add |
            inkwell::values::InstructionOpcode::FAdd |
            inkwell::values::InstructionOpcode::Sub |
            inkwell::values::InstructionOpcode::FSub |
            inkwell::values::InstructionOpcode::Mul |
            inkwell::values::InstructionOpcode::FMul |
            inkwell::values::InstructionOpcode::UDiv |
            inkwell::values::InstructionOpcode::SDiv |
            inkwell::values::InstructionOpcode::FDiv |
            inkwell::values::InstructionOpcode::URem |
            inkwell::values::InstructionOpcode::SRem |
            inkwell::values::InstructionOpcode::FRem => self.visit_binary_operator(inst),
            inkwell::values::InstructionOpcode::GetElementPtr => self.visit_gep_inst(inst),
            inkwell::values::InstructionOpcode::Trunc |
            inkwell::values::InstructionOpcode::ZExt |
            inkwell::values::InstructionOpcode::SExt |
            inkwell::values::InstructionOpcode::FPToUI |
            inkwell::values::InstructionOpcode::FPToSI |
            inkwell::values::InstructionOpcode::UIToFP |
            inkwell::values::InstructionOpcode::SIToFP |
            inkwell::values::InstructionOpcode::FPTrunc |
            inkwell::values::InstructionOpcode::FPExt |
            inkwell::values::InstructionOpcode::PtrToInt |
            inkwell::values::InstructionOpcode::IntToPtr |
            inkwell::values::InstructionOpcode::BitCast |
            inkwell::values::InstructionOpcode::AddrSpaceCast => self.visit_cast_inst(inst),
            inkwell::values::InstructionOpcode::Load => self.visit_load_inst(inst),
            inkwell::values::InstructionOpcode::Store => self.visit_store_inst(inst),
            inkwell::values::InstructionOpcode::Call => self.visit_call_inst(inst),
            inkwell::values::InstructionOpcode::Phi => self.visit_phi_node(inst),
            _ => false,
        }
    }

    fn visit_basic_block(&mut self, bb: BasicBlock<'ctx>) -> bool {
        let mut changed = false;
        for inst in bb.get_instructions() {
            if self.visit_instruction(inst) {
                changed = true;
            }
        }
        changed
    }

    fn visit_function(&mut self, func: inkwell::values::FunctionValue<'ctx>) -> bool {
        let mut changed = false;
        for bb in func.get_basic_blocks() {
            if self.visit_basic_block(bb) {
                changed = true;
            }
        }
        changed || self.finish()
    }

    fn finish(&mut self) -> bool {
        if self.gathered.is_empty() && self.scattered.is_empty() {
            return false;
        }

        for (op, cv) in &self.gathered {
            if !op.is_undef() {
                let ty = op.get_type();
                let res = match ty {
                    BasicTypeEnum::VectorType(vt) => {
                        let count = vt.get_size();
                        let mut res = self.context.const_vector(&[]);
                        for (i, &v) in cv.iter().enumerate() {
                            res = self.builder.build_insert_element(
                                res,
                                v,
                                self.context.i32_type().const_int(i as u64, false),
                                &format!("{}.upto{}", op.get_name().to_str().unwrap(), i),
                            );
                        }
                        BasicValueEnum::VectorValue(res)
                    }
                    _ => {
                        assert!(cv.len() == 1 && ty == cv[0].get_type());
                        cv[0]
                    }
                };
                if op != &res.into_instruction_value() {
                    op.replace_all_uses_with(res);
                    self.potentially_dead_instrs.push(*op);
                }
            }
        }

        for inst in &self.potentially_dead_instrs {
            if inst.is_constant() || inst.get_num_uses() == 0 {
                unsafe { inst.remove_from_parent(); }
            }
        }

        self.gathered.clear();
        self.scattered.clear();
        self.potentially_dead_instrs.clear();

        true
    }

    pub fn run_on_module(&mut self) -> bool {
        let mut changed = false;
        for func in self.module.get_functions() {
            if self.visit_function(func) {
                changed = true;
            }
        }
        changed
    }

    fn visit_cmp_inst(&mut self, ci: InstructionValue<'ctx>, is_fcmp: bool) -> bool {
        if let BasicTypeEnum::VectorType(vt) = ci.get_type() {
            let num_elems = vt.get_size();
            let vop0 = self.scatter(ci.get_parent().unwrap(), ci.get_operand(0).unwrap());
            let vop1 = self.scatter(ci.get_parent().unwrap(), ci.get_operand(1).unwrap());
            let mut res = Vec::with_capacity(num_elems as usize);

            for i in 0..num_elems {
                let name = format!("{}.i{}", ci.get_name().to_str().unwrap(), i);
                let cmp = if is_fcmp {
                    self.builder.build_float_compare(
                        ci.get_icmp_predicate().unwrap(),
                        vop0[i as usize].into_float_value(),
                        vop1[i as usize].into_float_value(),
                        &name,
                    )
                } else {
                    self.builder.build_int_compare(
                        ci.get_icmp_predicate().unwrap(),
                        vop0[i as usize].into_int_value(),
                        vop1[i as usize].into_int_value(),
                        &name,
                    )
                };
                res.push(BasicValueEnum::IntValue(cmp));
            }
            self.gather(ci, res);
            true
        } else {
            false
        }
    }

    fn visit_binary_operator(&mut self, bo: InstructionValue<'ctx>) -> bool {
        if let BasicTypeEnum::VectorType(vt) = bo.get_type() {
            let num_elems = vt.get_size();
            let vop0 = self.scatter(bo.get_parent().unwrap(), bo.get_operand(0).unwrap());
            let vop1 = self.scatter(bo.get_parent().unwrap(), bo.get_operand(1).unwrap());
            let mut res = Vec::with_capacity(num_elems as usize);

            for i in 0..num_elems {
                let name = format!("{}.i{}", bo.get_name().to_str().unwrap(), i);
                let op = match bo.get_opcode() {
                    inkwell::values::InstructionOpcode::Add => self.builder.build_int_add(
                        vop0[i as usize].into_int_value(),
                        vop1[i as usize].into_int_value(),
                        &name,
                    ),
                    inkwell::values::InstructionOpcode::FAdd => self.builder.build_float_add(
                        vop0[i as usize].into_float_value(),
                        vop1[i as usize].into_float_value(),
                        &name,
                    ),
                    _ => unimplemented!("Binary operation not implemented"),
                };
                res.push(op);
            }
            self.gather(bo, res);
            true
        } else {
            false
        }
    }

    fn visit_gep_inst(&mut self, gepi: InstructionValue<'ctx>) -> bool {
        if let BasicTypeEnum::VectorType(vt) = gepi.get_type() {
            let num_elems = vt.get_size();
            let vbase = self.scatter(gepi.get_parent().unwrap(), gepi.get_operand(0).unwrap());
            let mut vindices = Vec::new();
            for i in 1..gepi.get_num_operands() {
                vindices.push(self.scatter(gepi.get_parent().unwrap(), gepi.get_operand(i).unwrap()));
            }
            let mut res = Vec::with_capacity(num_elems as usize);

            for i in 0..num_elems {
                let name = format!("{}.i{}", gepi.get_name().to_str().unwrap(), i);
                let indices: Vec<BasicValueEnum> = vindices.iter().map(|v| v[i as usize]).collect();
                let gep = unsafe {
                    self.builder.build_gep(
                        vbase[i as usize].into_pointer_value(),
                        &indices,
                        &name,
                    )
                };
                res.push(BasicValueEnum::PointerValue(gep));
            }
            self.gather(gepi, res);
            true
        } else {
            false
        }
    }

    fn visit_cast_inst(&mut self, ci: InstructionValue<'ctx>) -> bool {
        if let BasicTypeEnum::VectorType(vt) = ci.get_type() {
            let num_elems = vt.get_size();
            let vop = self.scatter(ci.get_parent().unwrap(), ci.get_operand(0).unwrap());
            let mut res = Vec::with_capacity(num_elems as usize);

            for i in 0..num_elems {
                let name = format!("{}.i{}", ci.get_name().to_str().unwrap(), i);
                let cast = match ci.get_opcode() {
                    inkwell::values::InstructionOpcode::Trunc => self.builder.build_int_trunc(
                        vop[i as usize].into_int_value(),
                        vt.get_element_type().into_int_type(),
                        &name,
                    ),
                    _ => unimplemented!("Cast operation not implemented"),
                };
                res.push(cast);
            }
            self.gather(ci, res);
            true
        } else {
            false
        }
    }

    fn visit_load_inst(&mut self, li: InstructionValue<'ctx>) -> bool {
        unsafe {
            if !SCALARIZE_LOAD_STORE || li.as_load_inst().unwrap().get_volatile() {
                return false;
            }
        }

        if let Some(layout) = self.get_vector_layout(li.get_type(), li.get_alignment()) {
            let num_elems = layout.vec_ty.get_size();
            let vptr = self.scatter(li.get_parent().unwrap(), li.get_operand(0).unwrap());
            let mut res = Vec::with_capacity(num_elems as usize);

            for i in 0..num_elems {
                let name = format!("{}.i{}", li.get_name().to_str().unwrap(), i);
                let load = self.builder.build_load(vptr[i as usize].into_pointer_value(), &name);
                load.set_alignment(li.get_alignment());
                res.push(load);
            }
            self.gather(li, res);
            true
        } else {
            false
        }
    }

    fn visit_store_inst(&mut self, si: InstructionValue<'ctx>) -> bool {
        unsafe {
            if !SCALARIZE_LOAD_STORE || si.as_store_inst().unwrap().get_volatile() {
                return false;
            }
        }

        if let Some(layout) = self.get_vector_layout(si.get_operand(0).unwrap().get_type(), si.get_alignment()) {
            let num_elems = layout.vec_ty.get_size();
            let vptr = self.scatter(si.get_parent().unwrap(), si.get_operand(1).unwrap());
            let vval = self.scatter(si.get_parent().unwrap(), si.get_operand(0).unwrap());

            for i in 0..num_elems {
                let store = self.builder.build_store(vptr[i as usize].into_pointer_value(), vval[i as usize]);
                store.set_alignment(si.get_alignment());
            }
            true
        } else {
            false
        }
    }

    fn visit_call_inst(&mut self, ci: InstructionValue<'ctx>) -> bool {
        if let BasicTypeEnum::VectorType(vt) = ci.get_type() {
            let num_elems = vt.get_size();
            let mut vargs = Vec::new();
            for i in 0..ci.get_num_operands() - 1 {
                vargs.push(self.scatter(ci.get_parent().unwrap(), ci.get_operand(i).unwrap()));
            }
            let callee = ci.get_operand(ci.get_num_operands() - 1).unwrap();
            let mut res = Vec::with_capacity(num_elems as usize);

            for i in 0..num_elems {
                let name = format!("{}.i{}", ci.get_name().to_str().unwrap(), i);
                let args: Vec<BasicValueEnum> = vargs.iter().map(|v| v[i as usize]).collect();
                let call = self.builder.build_call(callee.into_function_value(), &args, &name);
                res.push(call.try_as_basic_value().left().unwrap());
            }
            self.gather(ci, res);
            true
        } else {
            false
        }
    }

    fn visit_phi_node(&mut self, phi: InstructionValue<'ctx>) -> bool {
        if let BasicTypeEnum::VectorType(vt) = phi.get_type() {
            let num_elems = vt.get_size();
            let mut res = Vec::with_capacity(num_elems as usize);

            for i in 0..num_elems {
                let name = format!("{}.i{}", phi.get_name().to_str().unwrap(), i);
                let new_phi = self.builder.build_phi(vt.get_element_type(), &name);
                for j in 0..phi.get_num_incoming() {
                    let incoming_value = phi.get_incoming(j).unwrap().0;
                    let incoming_block = phi.get_incoming(j).unwrap().1;
                    let vop = self.scatter(incoming_block, incoming_value);
                    new_phi.add_incoming(&[(&vop[i as usize], incoming_block)]);
                }
                res.push(BasicValueEnum::PhiValue(new_phi));
            }
            self.gather(phi, res);
            true
        } else {
            false
        }
    }
}