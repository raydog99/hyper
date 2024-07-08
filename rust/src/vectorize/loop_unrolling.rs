use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::PassManager;
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, InstructionValue, PhiValue};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::types::BasicTypeEnum;
use std::collections::{HashMap, HashSet};
use structopt::StructOpt;
use inkwell::instructions::BranchInst;

#[derive(Debug)]
struct Loop {
    header: BasicBlock,
    latch: BasicBlock,
    blocks: Vec<BasicBlock>,
    exits: Vec<BasicBlock>,
}

#[derive(Debug, StructOpt)]
#[structopt(name = "loop_unroll", about = "LLVM Loop Unrolling Pass")]
struct UnrollLoopOptions {
    #[structopt(short, long, default_value = "4")]
    count: u32,
    #[structopt(long, default_value = "0")]
    trip_count: u32,
    #[structopt(long, default_value = "1")]
    trip_multiple: u32,
    #[structopt(long, default_value = "0")]
    peel_count: u32,
    #[structopt(long)]
    allow_runtime: bool,
    #[structopt(long)]
    allow_expensive_trip_count: bool,
    #[structopt(short, long)]
    force: bool,
    #[structopt(long)]
    unroll_remainder: bool,
    #[structopt(long)]
    forget_all_scev: bool,
}

#[derive(Debug, PartialEq)]
enum LoopUnrollResult {
    Unmodified,
    PartiallyUnrolled,
    FullyUnrolled,
}

#[derive(Debug)]
struct UnrolledValue {
    iteration: u32,
    original: BasicValueEnum,
}

static mut UNROLL_RUNTIME_EPILOG: bool = false;

fn need_to_insert_phis_for_lcssa(l: &Loop, blocks: &[BasicBlock], li: &LoopInfo) -> bool {
    blocks.iter().any(|bb| {
        if li.get_loop_for(bb) == Some(l) {
            false
        } else {
            bb.get_instructions().any(|inst| {
                inst.get_operands().iter().any(|op| {
                    if let Some(def_inst) = op.left().and_then(BasicValueEnum::as_instruction_value) {
                        let def_loop = li.get_loop_for(&def_inst.get_parent().unwrap());
                        def_loop.map_or(false, |dl| dl.contains(l))
                    } else {
                        false
                    }
                })
            })
        }
    })
}

fn simplify_loop_after_unroll2<'ctx>(
    l: &Loop,
    simplify_ivs: bool,
    li: &LoopInfo,
    se: Option<&ScalarEvolution>,
    dt: Option<&DominatorTree>,
    ac: Option<&AssumptionCache>,
    tti: Option<&TargetTransformInfo>,
    context: &'ctx Context,
    builder: &Builder<'ctx>,
) {
    if simplify_ivs {
        if let Some(se) = se {
            let mut dead_insts = Vec::new();
            simplify_loop_ivs(l, se, dt.unwrap(), li, tti.unwrap(), &mut dead_insts);
            for inst in dead_insts {
                unsafe { inst.as_instruction_value().unwrap().erase_from_basic_block(); }
            }
        }
    }

    let dl = l.header.get_parent().unwrap().get_module().get_data_layout();
    for bb in &l.blocks {
        let mut i = 0;
        while i < bb.get_instruction_count() {
            let inst = bb.get_instruction(i).unwrap();
            if let Some(simplified) = simplify_instruction(&inst, &dl, None, dt, ac) {
                if li.replacement_preserves_lcssa_form(&inst, &simplified) {
                    inst.replace_all_uses_with(simplified);
                }
            }
            if is_instruction_trivially_dead(&inst) {
                unsafe { inst.erase_from_basic_block(); }
            } else {
                i += 1;
            }
        }
    }
}

fn is_epilog_profitable(l: &Loop) -> bool {
    let preheader = l.header.get_previous_basic_block().unwrap();
    l.header.get_first_instruction()
        .and_then(|inst| inst.as_instruction_value())
        .and_then(PhiValue::try_from_instruction_value)
        .map_or(false, |phi| {
            phi.get_incoming().iter().any(|(value, block)| {
                *block == preheader && value.is_constant()
            })
        })
}

fn unroll_loop_with_vmap<'ctx>(
    l: &Loop,
    ulo: &UnrollLoopOptions,
    li: &mut LoopInfo,
    se: Option<&mut ScalarEvolution>,
    dt: Option<&mut DominatorTree>,
    ac: Option<&mut AssumptionCache>,
    tti: Option<&TargetTransformInfo>,
    preserve_lcssa: bool,
    unroll_to_orig_map: &mut HashMap<BasicValueEnum<'ctx>, UnrolledValue>,
    remainder_loop: Option<&mut Loop>,
    context: &'ctx Context,
    builder: &Builder<'ctx>,
) -> LoopUnrollResult {
    if l.get_loop_preheader().is_none() || l.get_loop_latch().is_none() || !l.is_safe_to_clone() {
        return LoopUnrollResult::Unmodified;
    }

    let mut ulo = ulo.clone();
    if ulo.trip_count != 0 && ulo.count > ulo.trip_count {
        ulo.count = ulo.trip_count;
    }

    if ulo.trip_count == 0 && ulo.count < 2 && ulo.peel_count == 0 {
        return LoopUnrollResult::Unmodified;
    }

    let completely_unroll = ulo.count == ulo.trip_count;
    let runtime_trip_count = ulo.trip_count == 0 && ulo.count > 0 && ulo.allow_runtime;

    let mut peeled = false;
    if ulo.peel_count > 0 {
        peeled = peel_loop(l, ulo.peel_count, li, se, dt, ac, preserve_lcssa);
        if peeled {
            let exiting_block = l.get_loop_latch().unwrap();
            ulo.trip_count = se.as_ref().unwrap().get_small_constant_trip_count(l, &exiting_block);
            ulo.trip_multiple = se.as_ref().unwrap().get_small_constant_trip_multiple(l, &exiting_block);
        }
    }

    let orig_loop_blocks = l.blocks.clone();

    if let Some(se) = se {
        if ulo.forget_all_scev {
            se.forget_all_loops();
        } else {
            se.forget_topmost_loop(l);
        }
    }

    let breakout_trip = if ulo.trip_count != 0 {
        ulo.trip_count % ulo.count
    } else {
        ulo.trip_multiple = std::cmp::min(ulo.count, ulo.trip_multiple);
        ulo.trip_multiple
    };

    let mut last_value_map = HashMap::new();
    let orig_phi_nodes: Vec<_> = l.header.get_instructions()
        .filter_map(|inst| PhiValue::try_from_instruction_value(inst))
        .collect();

    let mut headers = vec![l.header];
    let mut latches = vec![l.get_loop_latch().unwrap()];
    let mut unrolled_loop_blocks = l.blocks.clone();

    let mut loops_to_simplify = HashSet::new();
    for sub_loop in l.get_sub_loops() {
        loops_to_simplify.insert(sub_loop);
    }

    for it in 1..ulo.count {
        let mut new_blocks = Vec::new();
        let mut new_loops = HashMap::new();
        new_loops.insert(l, l);

        for bb in &orig_loop_blocks {
            let new_bb = clone_basic_block(bb, &format!(".{}", it), context, builder);
            l.header.get_parent().unwrap().add_basic_block_after(&new_bb, bb);

            unroll_to_orig_map.insert(new_bb.as_basic_value_enum(), UnrolledValue {
                iteration: it,
                original: bb.as_basic_value_enum(),
            });

            if let Some(old_loop) = add_cloned_block_to_loop_info(bb, &new_bb, li, &mut new_loops) {
                loops_to_simplify.insert(new_loops[old_loop]);
            }

            if *bb == l.header {
                for (i, orig_phi) in orig_phi_nodes.iter().enumerate() {
                    let new_phi = new_bb.get_first_instruction().unwrap().as_instruction_value().unwrap();
                    let in_val = new_phi.get_operand(new_phi.get_num_operands() - 1).unwrap().left().unwrap();
                    if it > 1 && l.contains(&in_val.get_instruction().unwrap().get_parent().unwrap()) {
                        last_value_map.insert(orig_phi.as_basic_value_enum(), last_value_map[&in_val]);
                    } else {
                        last_value_map.insert(orig_phi.as_basic_value_enum(), in_val);
                    }
                    unsafe { new_phi.erase_from_basic_block(); }
                }
            }

            for (old_inst, new_inst) in bb.get_instructions().zip(new_bb.get_instructions()) {
                last_value_map.insert(old_inst.as_basic_value_enum(), new_inst.as_basic_value_enum());
                unroll_to_orig_map.insert(new_inst.as_basic_value_enum(), UnrolledValue {
                    iteration: it,
                    original: old_inst.as_basic_value_enum(),
                });
            }

            for succ in bb.get_successors() {
                if !l.contains(succ) {
                    for phi in succ.get_instructions().filter_map(PhiValue::try_from_instruction_value) {
                        let incoming = phi.get_incoming_value_for_block(bb).unwrap();
                        let new_incoming = last_value_map.get(&incoming).unwrap_or(&incoming);
                        phi.add_incoming(&[(&new_incoming, &new_bb)]);
                    }
                }
            }

            if *bb == l.header {
                headers.push(new_bb);
            }
            if Some(*bb) == l.get_loop_latch() {
                latches.push(new_bb);
            }

            new_blocks.push(new_bb);
            unrolled_loop_blocks.push(new_bb);

            if let Some(dt) = dt {
                if *bb == l.header {
                    dt.add_new_block(&new_bb, &latches[it as usize - 1]);
                } else {
                    let bb_dom_node = dt.get_node(bb);
                    let bb_idom = bb_dom_node.get_idom();
                    let original_bb_idom = bb_idom.get_block();
                    dt.add_new_block(&new_bb, last_value_map[&original_bb_idom.as_basic_value_enum()].as_basic_block().unwrap());
                }
            }
        }

        remap_instructions_in_blocks(&new_blocks, &mut last_value_map);
        for new_block in &new_blocks {
            for inst in new_block.get_instructions() {
                if let Some(call_inst) = inst.as_instruction_value().as_call_instruction_value() {
                    if call_inst.get_called_function_value().get_name().to_str().unwrap() == "llvm.assume" {
                        ac.as_mut().unwrap().register_assumption(call_inst);
                    }
                }
            }
        }
    }

    for pn in &orig_phi_nodes {
        if completely_unroll {
            let incoming = pn.get_incoming_value_for_block(&l.get_loop_preheader().unwrap()).unwrap();
            pn.replace_all_uses_with(incoming);
            unsafe { pn.erase_from_basic_block(); }
        } else if ulo.count > 1 {
            let in_val = pn.remove_incoming_value(&l.get_loop_latch().unwrap());
            let in_val = if l.contains(&in_val.get_instruction().unwrap().get_parent().unwrap()) {
                last_value_map[&in_val]
            } else {
                in_val
            };
            pn.add_incoming(&[(&in_val, latches.last().unwrap())]);
        }
    }

    for (i, latch) in latches.iter().enumerate() {
        let dest = &headers[(i + 1) % headers.len()];
        let term = latch.get_terminator().unwrap();
        if term.get_num_successors() > 1 {
            term.set_successor(0, dest);
        } else {
            let new_br = context.create_builder().build_unconditional_branch(dest);
            term.replace_all_uses_with(new_br.as_instruction_value());
            unsafe { term.erase_from_basic_block(); }
        }
    }

    simplify_loop_after_unroll2(l, !completely_unroll && (ulo.count > 1 || peeled), li, se, dt, ac, tti, context, builder);

    let outer_l = l.get_parent_loop();
    if completely_unroll {
        li.erase(l);
    }

    if preserve_lcssa && outer_l.is_some() && completely_unroll {
        let need_to_fix_lcssa = need_to_insert_phis_for_lcssa(outer_l.unwrap(), &unrolled_loop_blocks, li);
        if need_to_fix_lcssa {
            let latch_loop = li.get_loop_for(&latches.last().unwrap());
            let mut fix_lcssa_loop = outer_l.unwrap();
            while fix_lcssa_loop.get_parent_loop() != latch_loop {
                fix_lcssa_loop = fix_lcssa_loop.get_parent_loop().unwrap();
            }
            form_lcssa_recursively(fix_lcssa_loop, dt.as_mut().unwrap(), li, se.as_mut().unwrap());
        }
    }

    if let Some(dt) = dt {
        if let Some(outer_l) = outer_l {
            simplify_loop(outer_l, dt, li, se.as_mut().unwrap(), ac, None, preserve_lcssa);
        } else {
            for loop_to_simplify in loops_to_simplify {
                simplify_loop(loop_to_simplify, dt, li, se.as_mut().unwrap(), ac, None, preserve_lcssa);
            }
        }
    }

    if completely_unroll {
        LoopUnrollResult::FullyUnrolled
    } else {
        LoopUnrollResult::PartiallyUnrolled
    }
}

fn clone_basic_block<'ctx>(bb: &BasicBlock<'ctx>, suffix: &str, context: &'ctx Context, builder: &Builder<'ctx>) -> BasicBlock<'ctx> {
    let new_name = format!("{}{}", bb.get_name().to_str().unwrap(), suffix);
    let new_bb = context.append_basic_block(bb.get_parent().unwrap(), &new_name);
    
    let mut value_map = HashMap::new();
    
    for inst in bb.get_instructions() {
        let new_inst = inst.clone();
        new_inst.set_name(&format!("{}{}", inst.get_name().to_str().unwrap(), suffix));
        builder.position_at_end(&new_bb);
        builder.insert(&new_inst);
        value_map.insert(inst.as_any_value_enum(), new_inst.as_any_value_enum());
    }
    
    remap_instructions_in_blocks(&[new_bb], &mut value_map);
    
    new_bb
}

fn add_cloned_block_to_loop_info<'ctx>(old_bb: &BasicBlock<'ctx>, new_bb: &BasicBlock<'ctx>, li: &mut LoopInfo<'ctx>, new_loops: &mut HashMap<Loop<'ctx>, Loop<'ctx>>) -> Option<Loop<'ctx>> {
    let old_loop = li.get_loop_for(old_bb)?;
    let new_loop = new_loops.entry(old_loop).or_insert_with(|| {
        let new_loop = Loop::new();
        li.add_loop(new_loop, Some(old_loop.get_parent_loop()));
        new_loop
    });
    
    new_loop.add_block(new_bb);
    
    if old_loop.get_header() == old_bb {
        new_loop.set_header(new_bb);
    }
    
    Some(old_loop)
}

fn remap_instructions_in_blocks<'ctx>(blocks: &[BasicBlock<'ctx>], value_map: &mut HashMap<AnyValueEnum<'ctx>, AnyValueEnum<'ctx>>) {
    for block in blocks {
        for inst in block.get_instructions() {
            for (i, op) in inst.get_operands().iter().enumerate() {
                if let Some(new_op) = value_map.get(&op) {
                    inst.set_operand(i, new_op);
                }
            }
        }
    }
}
use std::collections::{HashMap, HashSet};

fn simplify_loop<'ctx>(
    l: &Loop<'ctx>,
    dt: &mut DominatorTree<'ctx>,
    li: &mut LoopInfo<'ctx>,
    se: &mut ScalarEvolution<'ctx>,
    ac: Option<&mut AssumptionCache<'ctx>>,
    tti: Option<&TargetTransformInfo>,
    preserve_lcssa: bool
) {
    eliminate_dead_code(l);

    simplify_loop_exits(l, se);

    eliminate_redundant_instructions(l, se);

    simplify_induction_variables(l, se, li);

    if let Some(unswitched_loop) = try_unswitch_loop(l, li, dt, ac) {
        simplify_loop(unswitched_loop, dt, li, se, ac, tti, preserve_lcssa);
    }

    licm(l, li, dt, ac);

    se.forget_loop(l);

    if preserve_lcssa {
        form_lcssa_recursively(l, dt, li, se);
    }
}

fn eliminate_dead_code(l: &Loop) {
    for bb in &l.blocks {
        let mut i = 0;
        while i < bb.get_instruction_count() {
            let inst = bb.get_instruction(i).unwrap();
            if inst.get_num_uses() == 0 && !inst.may_have_side_effects() {
                unsafe { inst.erase_from_basic_block(); }
            } else {
                i += 1;
            }
        }
    }
}

fn simplify_loop_exits(l: &Loop, se: &mut ScalarEvolution) {
    for exit in &l.get_exits() {
        if let Some(branch) = exit.get_terminator().as_branch() {
            if let Some(condition) = branch.get_condition() {
                if let Some(simplified) = se.simplify(condition) {
                    branch.set_condition(simplified);
                }
            }
        }
    }
}

fn eliminate_redundant_instructions(l: &Loop, se: &mut ScalarEvolution) {
    let mut value_map = HashMap::new();
    for bb in &l.blocks {
        for inst in bb.get_instructions() {
            if let Some(simplified) = se.simplify(inst.as_any_value_enum()) {
                value_map.insert(inst.as_any_value_enum(), simplified);
            }
        }
    }
    for bb in &l.blocks {
        for inst in bb.get_instructions() {
            if let Some(simplified) = value_map.get(&inst.as_any_value_enum()) {
                inst.replace_all_uses_with(simplified);
                unsafe { inst.erase_from_basic_block(); }
            }
        }
    }
}

fn simplify_induction_variables(l: &Loop, se: &mut ScalarEvolution, li: &LoopInfo) {
    let mut induction_vars = Vec::new();
    for phi in l.get_header().get_phi_nodes() {
        if se.is_induction_variable(phi, l) {
            induction_vars.push(phi);
        }
    }
    for iv in induction_vars {
        if let Some(simplified) = se.get_simplified_induction_variable(iv, l) {
            iv.replace_all_uses_with(simplified);
            unsafe { iv.erase_from_basic_block(); }
        }
    }
}

fn try_unswitch_loop<'ctx>(
    l: &Loop<'ctx>,
    li: &mut LoopInfo<'ctx>,
    dt: &mut DominatorTree<'ctx>,
    ac: Option<&mut AssumptionCache<'ctx>>
) -> Option<&'ctx Loop<'ctx>> {
    for bb in &l.blocks {
        if let Some(branch) = bb.get_terminator().as_branch() {
            if branch.is_conditional() && is_loop_invariant(branch.get_condition().unwrap(), l) {
                // Perform unswitching
                let (true_loop, false_loop) = unswitch_loop(l, branch, li, dt, ac);
                return Some(true_loop); // Return one of the unswitched loops
            }
        }
    }
    None
}

fn licm(l: &Loop, li: &LoopInfo, dt: &DominatorTree, ac: Option<&AssumptionCache>) {
    let mut invariant_insts = Vec::new();
    for bb in &l.blocks {
        for inst in bb.get_instructions() {
            if is_loop_invariant(inst.as_any_value_enum(), l) {
                invariant_insts.push(inst);
            }
        }
    }
    let preheader = l.get_loop_preheader().unwrap();
    for inst in invariant_insts {
        unsafe { inst.move_before(preheader.get_terminator().unwrap()); }
    }
}

fn form_lcssa_recursively<'ctx>(
    l: &Loop<'ctx>,
    dt: &mut DominatorTree<'ctx>,
    li: &mut LoopInfo<'ctx>,
    se: &mut ScalarEvolution<'ctx>
) {
    // Form LCSSA for the current loop
    form_lcssa_for_loop(l, dt, li);

    // Recursively form LCSSA for all nested loops
    for nested_loop in l.get_sub_loops() {
        form_lcssa_recursively(nested_loop, dt, li, se);
    }

    // Update ScalarEvolution
    se.forget_loop(l);
}

fn form_lcssa_for_loop(l: &Loop, dt: &mut DominatorTree, li: &mut LoopInfo) {
    let mut values_to_phi = HashMap::new();

    // Find all values defined in the loop and used outside
    for bb in &l.blocks {
        for inst in bb.get_instructions() {
            for use_ in inst.get_uses() {
                let user = use_.get_user();
                if !l.contains(user.get_parent()) {
                    values_to_phi.entry(inst.as_any_value_enum()).or_insert_with(HashSet::new)
                        .insert(user.get_parent());
                }
            }
        }
    }

    // Create PHI nodes for these values
    for (value, use_blocks) in values_to_phi {
        for use_block in use_blocks {
            let phi = use_block.get_context().create_phi(value.get_type(), 0, "lcssa");
            for pred in use_block.get_predecessors() {
                if l.contains(pred) {
                    phi.add_incoming(&[(&value, pred)]);
                } else {
                    phi.add_incoming(&[(&phi.as_any_value_enum(), pred)]);
                }
            }
            value.replace_all_uses_with_in(phi.as_any_value_enum(), use_block);
        }
    }

    // Update DominatorTree and LoopInfo
    dt.recalculate_for_loop(l);
    li.update_for_loop(l);
}

fn is_loop_invariant(value: AnyValueEnum, l: &Loop) -> bool {
    match value {
        AnyValueEnum::Instruction(inst) => !l.contains(inst.get_parent()),
        AnyValueEnum::Argument(_) | AnyValueEnum::GlobalVariable(_) | AnyValueEnum::Constant(_) => true,
        _ => false,
    }
}

fn unswitch_loop<'ctx>(
    l: &Loop<'ctx>,
    branch: &BranchInst<'ctx>,
    li: &mut LoopInfo<'ctx>,
    dt: &mut DominatorTree<'ctx>,
    ac: Option<&mut AssumptionCache<'ctx>>
) -> (Loop<'ctx>, Loop<'ctx>) {
    let context = l.get_context();
    let function = l.get_header().get_parent().unwrap();
    
    // Create new basic blocks for the unswitched loops
    let true_preheader = context.append_basic_block(function, "true_preheader");
    let false_preheader = context.append_basic_block(function, "false_preheader");
    
    // Clone the loop for both branches
    let (true_loop, true_exit) = clone_loop(l, "true");
    let (false_loop, false_exit) = clone_loop(l, "false");
    
    // Update the branch instruction
    let condition = branch.get_condition().unwrap();
    branch.replace_with_new_branch(true_preheader, false_preheader, Some(condition));
    
    // Create branches from new preheaders to respective loop headers
    context.create_unconditional_branch(true_loop.get_header(), true_preheader);
    context.create_unconditional_branch(false_loop.get_header(), false_preheader);
    
    // Update phi nodes in the loop headers
    update_phi_nodes(&true_loop, l, true_preheader);
    update_phi_nodes(&false_loop, l, false_preheader);
    
    // Update loop exits
    redirect_exits(&true_loop, true_exit);
    redirect_exits(&false_loop, false_exit);
    
    // Update LoopInfo
    li.remove_loop(l);
    li.add_loop(true_loop, Some(l.get_parent_loop()));
    li.add_loop(false_loop, Some(l.get_parent_loop()));
    
    // Update DominatorTree
    dt.recalculate(function);
    
    // Update AssumptionCache if available
    if let Some(ac) = ac {
        ac.update_for_unswitch(l, &true_loop, &false_loop);
    }
    
    (true_loop, false_loop)
}

fn clone_loop<'ctx>(original: &Loop<'ctx>, suffix: &str) -> (Loop<'ctx>, BasicBlock<'ctx>) {
    let context = original.get_context();
    let function = original.get_header().get_parent().unwrap();
    
    let mut value_map = HashMap::new();
    let mut new_blocks = Vec::new();
    
    // Clone all blocks in the loop
    for &block in &original.get_blocks() {
        let new_name = format!("{}.{}", block.get_name().to_str().unwrap(), suffix);
        let new_block = context.append_basic_block(function, &new_name);
        value_map.insert(block.as_any_value_enum(), new_block.as_any_value_enum());
        new_blocks.push(new_block);
    }
    
    // Clone instructions and remap operands
    for (&old_block, &new_block) in original.get_blocks().iter().zip(new_blocks.iter()) {
        for instruction in old_block.get_instructions() {
            let new_instruction = instruction.clone();
            new_instruction.set_name(&format!("{}.{}", instruction.get_name().to_str().unwrap(), suffix));
            new_block.append_instruction(new_instruction);
            value_map.insert(instruction.as_any_value_enum(), new_instruction.as_any_value_enum());
        }
    }
    
    // Remap operands in the cloned instructions
    for new_block in &new_blocks {
        for instruction in new_block.get_instructions() {
            remap_operands(instruction, &value_map);
        }
    }
    
    // Create a new exit block
    let exit_block = context.append_basic_block(function, &format!("exit.{}", suffix));
    
    // Create and return the new Loop structure
    let new_loop = Loop::new(
        value_map[&original.get_header().as_any_value_enum()].into_basic_block(),
        value_map[&original.get_latch().as_any_value_enum()].into_basic_block(),
        new_blocks,
        vec![exit_block],
    );
    
    (new_loop, exit_block)
}

fn update_phi_nodes<'ctx>(new_loop: &Loop<'ctx>, original_loop: &Loop<'ctx>, new_preheader: BasicBlock<'ctx>) {
    for phi in new_loop.get_header().get_phi_instructions() {
        let original_phi = original_loop.get_header().get_phi_instructions()
            .find(|p| p.get_name() == phi.get_name())
            .unwrap();
        
        let incoming_value = original_phi.get_incoming_value_for_block(original_loop.get_loop_preheader().unwrap()).unwrap();
        phi.add_incoming(&[(&incoming_value, new_preheader)]);
    }
}

fn redirect_exits<'ctx>(new_loop: &Loop<'ctx>, new_exit: BasicBlock<'ctx>) {
    for block in &new_loop.get_blocks() {
        if let Some(branch) = block.get_terminator().as_branch() {
            if branch.is_conditional() {
                let true_dest = branch.get_successor(0);
                let false_dest = branch.get_successor(1);
                
                if !new_loop.contains(true_dest) {
                    branch.set_successor(0, new_exit);
                }
                if !new_loop.contains(false_dest) {
                    branch.set_successor(1, new_exit);
                }
            }
        }
    }
}

fn remap_operands<'ctx>(instruction: InstructionValue<'ctx>, value_map: &HashMap<BasicValueEnum<'ctx>, BasicValueEnum<'ctx>>) {
    for (i, operand) in instruction.get_operands().iter().enumerate() {
        if let Some(new_operand) = value_map.get(operand) {
            instruction.set_operand(i, *new_operand);
        }
    }
}