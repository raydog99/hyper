open Llvm
open Llvm_analysis
open Llvm_scalar_opts
open Llvm_target
open Llvm_transforms
open Llvm_ipo

module CL = Cmdliner

type loop = {
  header: llbasicblock;
  latch: llbasicblock;
  blocks: llbasicblock list;
  exits: llbasicblock list;
}

type unroll_loop_options = {
  count: int;
  trip_count: int;
  trip_multiple: int;
  peel_count: int;
  allow_runtime: bool;
  allow_expensive_trip_count: bool;
  force: bool;
  unroll_remainder: bool;
  forget_all_scev: bool;
}

type loop_unroll_result =
  | Unmodified
  | PartiallyUnrolled
  | FullyUnrolled

type unrolled_value = {
  iteration: int;
  original: llvalue;
}

let unroll_runtime_epilog = ref false

let need_to_insert_phis_for_lcssa l blocks li =
  List.exists (fun bb ->
    if Loop.contains l bb then false
    else
      Llvm.fold_left_instrs (fun acc instr ->
        acc || List.exists (fun op ->
          match value_as_instruction op with
          | Some def_instr ->
            let def_loop = Li.get_loop_for (instr_parent def_instr) in
            Option.is_some def_loop && Loop.contains def_loop l
          | None -> false
        ) (use_begin instr)
      ) false bb
  ) blocks

let simplify_loop_after_unroll2 l simplify_ivs li se dt ac tti =
  if se && simplify_ivs then begin
    let dead_insts = ref [] in
    simplify_loop_ivs l se dt li tti dead_insts;
    List.iter (fun v ->
      match value_as_instruction v with
      | Some inst -> delete_instruction inst
      | None -> ()
    ) !dead_insts
  end;
  
  let dl = module_data_layout (global_parent (block_parent l.header)) in
  List.iter (fun bb ->
    Llvm.iter_instrs (fun inst ->
      match simplify_instruction inst dl None dt ac with
      | Some v when Li.replacement_preserves_lcssa_form inst v ->
        replace_all_uses_with inst v
      | _ ->
        if is_instruction_trivially_dead inst then
          delete_instruction inst
    ) bb
  ) l.blocks

let is_epilog_profitable l =
  let preheader = Loop.get_loop_preheader l in
  let header = l.header in
  List.exists (fun phi ->
    match incoming_value phi preheader with
    | Some v -> is_constant v
    | None -> false
  ) (phi_nodes header)

let rec unroll_loop_with_vmap l ulo li se dt ac tti preserve_lcssa unroll_to_orig_map remainder_loop =
  let open Llvm in
  
  if not (Loop.get_loop_preheader l) then
    Unmodified
  else if not (Loop.get_loop_latch l) then
    Unmodified
  else if not (Loop.is_safe_to_clone l) then
    Unmodified
  else if has_address_taken l.header then
    Unmodified
  else begin
    let ulo = if ulo.trip_count <> 0 && ulo.count > ulo.trip_count then
      { ulo with count = ulo.trip_count }
    else ulo in

    if ulo.trip_count = 0 && ulo.count < 2 && ulo.peel_count = 0 then
      Unmodified
    else begin
      let completely_unroll = ulo.count = ulo.trip_count in
      let runtime_trip_count = ulo.trip_count = 0 && ulo.count > 0 && ulo.allow_runtime in

      let peeled = ref false in
      if ulo.peel_count > 0 then begin
        peeled := peel_loop l ulo.peel_count li se dt ac preserve_lcssa;
        if !peeled then begin
          let exiting_block = l.latch in
          let new_trip_count = Se.get_small_constant_trip_count l exiting_block in
          let new_trip_multiple = Se.get_small_constant_trip_multiple l exiting_block in
          ulo = { ulo with trip_count = new_trip_count; trip_multiple = new_trip_multiple }
        end
      end;

      let orig_loop_blocks = l.blocks in
      let breakout_trip = 
        if ulo.trip_count <> 0 then ulo.trip_count mod ulo.count
        else ulo.trip_multiple in

      if se then
        if ulo.forget_all_scev then Se.forget_all_loops se
        else Se.forget_topmost_loop se l;

      let rec clone_iterations count acc last_value_map =
        if count = 0 then (acc, last_value_map)
        else
          let new_blocks, new_value_map = 
            List.fold_left (fun (blocks, map) bb ->
              let new_bb, new_map = clone_basic_block_with_map bb in
              (new_bb :: blocks, ValueMap.union new_map map)
            ) ([], last_value_map) orig_loop_blocks
          in
          clone_iterations (count - 1) (new_blocks :: acc) new_value_map
      in
      let unrolled_blocks, last_value_map = clone_iterations ulo.count [] ValueMap.empty in

      List.iter (fun phi ->
        if completely_unroll then
          replace_all_uses_with phi (incoming_value phi (Loop.get_loop_preheader l))
        else if ulo.count > 1 then begin
          let in_val = remove_incoming phi l.latch in
          let in_val = 
            match value_as_instruction in_val with
            | Some instr when Loop.contains l (instr_parent instr) ->
              ValueMap.find in_val last_value_map
            | _ -> in_val
          in
          add_incoming phi in_val (List.hd (List.rev unrolled_blocks))
        end
      ) (phi_nodes l.header);

      List.iteri (fun i latch ->
        let dest = if i = List.length unrolled_blocks - 1 then l.header else List.nth unrolled_blocks (i + 1) in
        let term = terminator latch in
        match value_as_branch term with
        | Some br when is_conditional br ->
          set_successor br 0 dest
        | _ ->
          let new_br = build_br dest (instr_parent term) in
          replace_all_uses_with term new_br;
          delete_instruction term
      ) (List.map (fun blocks -> List.hd (List.rev blocks)) unrolled_blocks);

      simplify_loop_after_unroll2 l (not completely_unroll && (ulo.count > 1 || !peeled)) li se dt ac tti;

      if preserve_lcssa && completely_unroll then
        List.iter (fun exit_bb ->
          List.iter (fun phi ->
            List.iter (fun block ->
              if not (Loop.contains l block) then
                remove_incoming phi block
            ) (incoming_blocks phi)
          ) (phi_nodes exit_bb)
        ) l.exits;

      if completely_unroll then Li.erase l;

      if completely_unroll then FullyUnrolled else PartiallyUnrolled
    end
  end