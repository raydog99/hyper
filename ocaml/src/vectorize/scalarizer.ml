open Llvm

module StringSet = Set.Make(String)
module ValueMap = Map.Make(struct type t = llvalue let compare = compare end)

let scalarize_variable_insert_extract = ref true
let scalarize_load_store = ref false

type value_vector = llvalue array
type scatter_map = value_vector ValueMap.t
type gather_list = (llvalue * value_vector) list
type loop_unroll_result = Unmodified | PartiallyUnrolled | FullyUnrolled
type unrolled_value = { iteration : int; original : llvalue }
type vector_layout = {
  vec_ty : lltype;
  elem_ty : lltype;
  vec_align : int;
  elem_size : Int64.t;
}

let skip_past_phi_nodes_and_dbg iter =
  let rec skip iter =
    match value_name (iter_to_instref iter) with
    | "llvm.dbg.value" -> skip (instr_succ iter)
    | _ -> iter
  in
  match classify_value (iter_to_instref iter) with
  | Instruction PHI -> skip (instr_succ iter)
  | _ -> skip iter

let scatter point v scattered =
  let scatter_helper bb iter v =
    let ty = type_of v in
    let size = match classify_type ty with
      | Vector -> vector_size ty
      | Pointer ->
        let elem_ty = element_type ty in
        (match classify_type elem_ty with
         | Vector -> vector_size elem_ty
         | _ -> failwith "Unexpected type in scatter_helper")
      | _ -> failwith "Unexpected type in scatter_helper"
    in
    let cache = try ValueMap.find v scattered with Not_found ->
      let cache = Array.make size None in
      cache
    in
    Array.mapi (fun i _ ->
      match cache.(i) with
      | Some v -> v
      | None ->
        let builder = builder_at_end (global_context ()) bb in
        let v' = if classify_type ty = Pointer then
          if i = 0 then
            let new_ptr_ty = pointer_type (element_type (element_type ty)) in
            build_bitcast v new_ptr_ty (value_name v ^ ".i0") builder
          else
            let gep = build_gep v [| const_int (i32_type (global_context ())) i |]
              (value_name v ^ ".i" ^ string_of_int i) builder in
            build_load gep (value_name v ^ ".i" ^ string_of_int i) builder
        else
          build_extractelement v (const_int (i32_type (global_context ())) i)
            (value_name v ^ ".i" ^ string_of_int i) builder
        in
        cache.(i) <- Some v';
        v'
    ) cache
  in
  match classify_value v with
  | Argument arg ->
    let func = block_parent point in
    let entry = entry_block func in
    scatter_helper entry (instr_begin entry) v
  | Instruction _ ->
    let bb = instr_parent v in
    let iter = instr_succ (instr_begin bb) in
    scatter_helper bb (skip_past_phi_nodes_and_dbg iter) v
  | _ ->
    scatter_helper point (instr_begin point) v

let can_transfer_metadata tag parallel_loop_access_md_kind =
  tag = md_kind_id "tbaa"
  || tag = md_kind_id "fpmath"
  || tag = md_kind_id "tbaa.struct"
  || tag = md_kind_id "invariant.load"
  || tag = md_kind_id "alias.scope"
  || tag = md_kind_id "noalias"
  || tag = parallel_loop_access_md_kind
  || tag = md_kind_id "access_group"

let transfer_metadata_and_ir_flags op cv parallel_loop_access_md_kind =
  let metadata = get_metadata op in
  Array.iter (fun v ->
    match classify_value v with
    | Instruction _ ->
      List.iter (fun (kind, md) ->
        if can_transfer_metadata kind parallel_loop_access_md_kind then
          set_metadata v kind md
      ) metadata;
      copy_fast_math_flags v op;
      (match get_debug_loc op with
      | Some loc when get_debug_loc v = None -> set_debug_loc v loc
      | _ -> ())
    | _ -> ()
  ) cv

let gather op cv scattered gathered potentially_dead_instrs parallel_loop_access_md_kind =
  transfer_metadata_and_ir_flags op cv parallel_loop_access_md_kind;
  let scattered = 
    (match ValueMap.find_opt op scattered with
    | Some sv ->
      Array.iteri (fun i v ->
        match sv.(i), v with
        | Some old_v, new_v when old_v <> new_v ->
          replace_all_uses_with old_v new_v;
          potentially_dead_instrs := old_v :: !potentially_dead_instrs
        | _ -> ()
      ) cv;
      ValueMap.add op cv scattered
    | None -> ValueMap.add op cv scattered)
  in
  let gathered = (op, cv) :: gathered in
  scattered, gathered, !potentially_dead_instrs

let get_vector_layout ty alignment dl =
  match classify_type ty with
  | Vector ->
    let elem_ty = element_type ty in
    if size_of dl elem_ty = store_size dl elem_ty then
      Some {
        vec_ty = ty;
        elem_ty;
        vec_align = alignment;
        elem_size = store_size dl elem_ty;
      }
    else None
  | _ -> None

let visit_select_inst si scattered gathered potentially_dead_instrs parallel_loop_access_md_kind =
  match classify_type (type_of si) with
  | Vector ->
    let num_elems = vector_size (type_of si) in
    let builder = builder_at_end (global_context ()) (instr_parent si) in
    let vop1 = scatter (instr_parent si) (operand si 1) scattered in
    let vop2 = scatter (instr_parent si) (operand si 2) scattered in
    let res = Array.make num_elems (undef_value (element_type (type_of si))) in
    (match classify_type (type_of (operand si 0)) with
    | Vector ->
      let vop0 = scatter (instr_parent si) (operand si 0) scattered in
      for i = 0 to num_elems - 1 do
        res.(i) <- build_select vop0.(i) vop1.(i) vop2.(i) (value_name si ^ ".i" ^ string_of_int i) builder
      done
    | _ ->
      let op0 = operand si 0 in
      for i = 0 to num_elems - 1 do
        res.(i) <- build_select op0 vop1.(i) vop2.(i) (value_name si ^ ".i" ^ string_of_int i) builder
      done);
    let scattered, gathered, potentially_dead_instrs = 
      gather si res scattered gathered potentially_dead_instrs parallel_loop_access_md_kind in
    true, scattered, gathered, potentially_dead_instrs
  | _ -> false, scattered, gathered, potentially_dead_instrs

let visit_cmp_inst is_fcmp ci scattered gathered potentially_dead_instrs parallel_loop_access_md_kind =
  match classify_type (type_of ci) with
  | Vector ->
    let num_elems = vector_size (type_of ci) in
    let builder = builder_at_end (global_context ()) (instr_parent ci) in
    let vop0 = scatter (instr_parent ci) (operand ci 0) scattered in
    let vop1 = scatter (instr_parent ci) (operand ci 1) scattered in
    let res = Array.make num_elems (undef_value (element_type (type_of ci))) in
    for i = 0 to num_elems - 1 do
      res.(i) <- if is_fcmp then
        build_fcmp (fcmp_predicate ci) vop0.(i) vop1.(i) (value_name ci ^ ".i" ^ string_of_int i) builder
      else
        build_icmp (icmp_predicate ci) vop0.(i) vop1.(i) (value_name ci ^ ".i" ^ string_of_int i) builder
    done;
    let scattered, gathered, potentially_dead_instrs = 
      gather ci res scattered gathered potentially_dead_instrs parallel_loop_access_md_kind in
    true, scattered, gathered, potentially_dead_instrs
  | _ -> false, scattered, gathered, potentially_dead_instrs

let visit_binary_inst bi scattered gathered potentially_dead_instrs parallel_loop_access_md_kind =
  match classify_type (type_of bi) with
  | Vector ->
    let num_elems = vector_size (type_of bi) in
    let builder = builder_at_end (global_context ()) (instr_parent bi) in
    let vop0 = scatter (instr_parent bi) (operand bi 0) scattered in
    let vop1 = scatter (instr_parent bi) (operand bi 1) scattered in
    let res = Array.make num_elems (undef_value (element_type (type_of bi))) in
    for i = 0 to num_elems - 1 do
      res.(i) <- build_binop (instr_opcode bi) vop0.(i) vop1.(i) (value_name bi ^ ".i" ^ string_of_int i) builder
    done;
    let scattered, gathered, potentially_dead_instrs = 
      gather bi res scattered gathered potentially_dead_instrs parallel_loop_access_md_kind in
    true, scattered, gathered, potentially_dead_instrs
  | _ -> false, scattered, gathered, potentially_dead_instrs

let visit_gep_inst gepi scattered gathered potentially_dead_instrs parallel_loop_access_md_kind =
  match classify_type (type_of gepi) with
  | Vector ->
    let num_elems = vector_size (type_of gepi) in
    let builder = builder_at_end (global_context ()) (instr_parent gepi) in
    let vbase = scatter (instr_parent gepi) (operand gepi 0) scattered in
    let vindices = Array.init (num_operands gepi - 1) (fun i ->
      scatter (instr_parent gepi) (operand gepi (i + 1)) scattered
    ) in
    let res = Array.make num_elems (undef_value (element_type (type_of gepi))) in
    for i = 0 to num_elems - 1 do
      let indices = Array.map (fun vindex -> vindex.(i)) vindices in
      res.(i) <- build_gep vbase.(i) indices (value_name gepi ^ ".i" ^ string_of_int i) builder
    done;
    let scattered, gathered, potentially_dead_instrs = 
      gather gepi res scattered gathered potentially_dead_instrs parallel_loop_access_md_kind in
    true, scattered, gathered, potentially_dead_instrs
  | _ -> false, scattered, gathered, potentially_dead_instrs

let visit_cast_inst ci scattered gathered potentially_dead_instrs parallel_loop_access_md_kind =
  match classify_type (type_of ci) with
  | Vector ->
    let num_elems = vector_size (type_of ci) in
    let builder = builder_at_end (global_context ()) (instr_parent ci) in
    let vop = scatter (instr_parent ci) (operand ci 0) scattered in
    let res = Array.make num_elems (undef_value (element_type (type_of ci))) in
    for i = 0 to num_elems - 1 do
      res.(i) <- build_cast (instr_opcode ci) vop.(i) (element_type (type_of ci)) (value_name ci ^ ".i" ^ string_of_int i) builder
    done;
    let scattered, gathered, potentially_dead_instrs = 
      gather ci res scattered gathered potentially_dead_instrs parallel_loop_access_md_kind in
    true, scattered, gathered, potentially_dead_instrs
  | _ -> false, scattered, gathered, potentially_dead_instrs

let visit_load_inst li scattered gathered potentially_dead_instrs parallel_loop_access_md_kind =
  if not !scalarize_load_store then false, scattered, gathered, potentially_dead_instrs
  else if not (is_load_atomic li) then
    match get_vector_layout (type_of li) (alignment li) (module_data_layout (global_module ())) with
    | Some layout ->
      let num_elems = vector_size layout.vec_ty in
      let builder = builder_at_end (global_context ()) (instr_parent li) in
      let vptr = scatter (instr_parent li) (operand li 0) scattered in
      let res = Array.make num_elems (undef_value layout.elem_ty) in
      for i = 0 to num_elems - 1 do
        res.(i) <- build_load vptr.(i) (value_name li ^ ".i" ^ string_of_int i) builder
      done;
      let scattered, gathered, potentially_dead_instrs = 
        gather li res scattered gathered potentially_dead_instrs parallel_loop_access_md_kind in
      true, scattered, gathered, potentially_dead_instrs
    | None -> false, scattered, gathered, potentially_dead_instrs
  else false, scattered, gathered, potentially_dead_instrs

let visit_store_inst si scattered gathered potentially_dead_instrs parallel_loop_access_md_kind =
  if not !scalarize_load_store then false, scattered, gathered, potentially_dead_instrs
  else if not (is_store_atomic si) then
    match get_vector_layout (type_of (operand si 0)) (alignment si) (module_data_layout (global_module ())) with
    | Some layout ->
      let num_elems = vector_size layout.vec_ty in
      let builder = builder_at_end (global_context ()) (instr_parent si) in
      let vptr = scatter (instr_parent si) (operand si 1) scattered in
      let vval = scatter (instr_parent si) (operand si 0) scattered in
      let stores = Array.make num_elems (undef_value (void_type (global_context ()))) in
      for i = 0 to num_elems - 1 do
        stores.(i) <- build_store vval.(i) vptr.(i) builder
      done;
      transfer_metadata_and_ir_flags si stores parallel_loop_access_md_kind;
      true, scattered, gathered, potentially_dead_instrs
    | None -> false, scattered, gathered, potentially_dead_instrs
  else false, scattered, gathered, potentially_dead_instrs

let visit_call_inst ci scattered gathered potentially_dead_instrs parallel_loop_access_md_kind =
  match classify_type (type_of ci) with
  | Vector ->
    let num_elems = vector_size (type_of ci) in
    let builder = builder_at_end (global_context ()) (instr_parent ci) in
    let vargs = Array.init (num_operands ci - 1) (fun i ->
      scatter (instr_parent ci) (operand ci i) scattered
    ) in
    let res = Array.make num_elems (undef_value (element_type (type_of ci))) in
    let callee = operand ci (num_operands ci - 1) in
    for i = 0 to num_elems - 1 do
      let args = Array.map (fun varg -> varg.(i)) vargs in
      res.(i) <- build_call callee args (value_name ci ^ ".i" ^ string_of_int i) builder
    done;
    let scattered, gathered, potentially_dead_instrs = 
      gather ci res scattered gathered potentially_dead_instrs parallel_loop_access_md_kind in
    true, scattered, gathered, potentially_dead_instrs
  | _ -> false, scattered, gathered, potentially_dead_instrs

let visit_phi_node phi scattered gathered potentially_dead_instrs parallel_loop_access_md_kind =
  match classify_type (type_of phi) with
  | Vector ->
    let num_elems = vector_size (type_of phi) in
    let builder = builder_at_end (global_context ()) (instr_parent phi) in
    let num_incoming = num_incoming phi in
    let res = Array.make num_elems (undef_value (element_type (type_of phi))) in
    for i = 0 to num_elems - 1 do
      let new_phi = build_phi (element_type (type_of phi)) (value_name phi ^ ".i" ^ string_of_int i) builder in
      for j = 0 to num_incoming - 1 do
        let incoming_value = incoming_value phi j in
        let incoming_block = incoming_block phi j in
        let vop = scatter incoming_block incoming_value scattered in
        add_incoming (vop.(i), incoming_block) new_phi
      done;
      res.(i) <- new_phi
    done;
    let scattered, gathered, potentially_dead_instrs = 
      gather phi res scattered gathered potentially_dead_instrs parallel_loop_access_md_kind in
    true, scattered, gathered, potentially_dead_instrs
  | _ -> false, scattered, gathered, potentially_dead_instrs

let visit_instruction instr scattered gathered potentially_dead_instrs parallel_loop_access_md_kind =
  match instr_opcode instr with
  | Trunc | ZExt | SExt | FPToUI | FPToSI | UIToFP | SIToFP | FPTrunc | FPExt | PtrToInt | IntToPtr | BitCast | AddrSpaceCast ->
    visit_cast_inst instr scattered gathered potentially_dead_instrs parallel_loop_access_md_kind
  | Select -> visit_select_inst instr scattered gathered potentially_dead_instrs parallel_loop_access_md_kind
  | ICmp -> visit_cmp_inst false instr scattered gathered potentially_dead_instrs parallel_loop_access_md_kind
  | FCmp -> visit_cmp_inst true instr scattered gathered potentially_dead_instrs parallel_loop_access_md_kind
  | Add | FAdd | Sub | FSub | Mul | FMul | UDiv | SDiv | FDiv | URem | SRem | FRem ->
    visit_binary_inst instr scattered gathered potentially_dead_instrs parallel_loop_access_md_kind
  | GetElementPtr -> visit_gep_inst instr scattered gathered potentially_dead_instrs parallel_loop_access_md_kind
  | Load -> visit_load_inst instr scattered gathered potentially_dead_instrs parallel_loop_access_md_kind
  | Store -> visit_store_inst instr scattered gathered potentially_dead_instrs parallel_loop_access_md_kind
  | Call -> visit_call_inst instr scattered gathered potentially_dead_instrs parallel_loop_access_md_kind
  | PHI -> visit_phi_node instr scattered gathered potentially_dead_instrs parallel_loop_access_md_kind
  | _ -> false, scattered, gathered, potentially_dead_instrs

let visit_basic_block bb scattered gathered potentially_dead_instrs parallel_loop_access_md_kind =
  let rec visit_instructions instr (changed, scattered, gathered, potentially_dead_instrs) =
    let instr_changed, scattered, gathered, potentially_dead_instrs =
      visit_instruction instr scattered gathered potentially_dead_instrs parallel_loop_access_md_kind
    in
    let changed = changed || instr_changed in
    match instr_succ instr with
    | None -> changed, scattered, gathered, potentially_dead_instrs
    | Some next_instr -> visit_instructions next_instr (changed, scattered, gathered, potentially_dead_instrs)
  in
  match instr_begin bb with
  | None -> false, scattered, gathered, potentially_dead_instrs
  | Some first_instr -> visit_instructions first_instr (false, scattered, gathered, potentially_dead_instrs)

let visit_function func parallel_loop_access_md_kind =
  let rec visit_basic_blocks bb (changed, scattered, gathered, potentially_dead_instrs) =
    let bb_changed, scattered, gathered, potentially_dead_instrs =
      visit_basic_block bb scattered gathered potentially_dead_instrs parallel_loop_access_md_kind
    in
    let changed = changed || bb_changed in
    match block_succ bb with
    | None -> changed, scattered, gathered, potentially_dead_instrs
    | Some next_bb -> visit_basic_blocks next_bb (changed, scattered, gathered, potentially_dead_instrs)
  in
  match entry_block func with
  | None -> false
  | Some entry -> 
      let changed, scattered, gathered, potentially_dead_instrs =
        visit_basic_blocks entry (false, ValueMap.empty, [], ref [])
      in
      let final_changed = changed || finish gathered scattered potentially_dead_instrs in
      final_changed

and finish gathered scattered potentially_dead_instrs =
  if gathered = [] && ValueMap.is_empty scattered then false
  else begin
    List.iter (fun (op, cv) ->
      if not (is_undef op) then begin
        let ty = type_of op in
        let res = ref (undef_value ty) in
        (match classify_type ty with
        | Vector ->
          let bb = instr_parent op in
          let count = vector_size ty in
          let builder = builder_at_end (global_context ()) bb in
          (match classify_value op with
          | Instruction PHI ->
            position_builder_at_end (instr_begin bb) builder
          | _ -> ());
          for i = 0 to count - 1 do
            res := build_insertelement !res cv.(i) (const_int (i32_type (global_context ())) i)
              (value_name op ^ ".upto" ^ string_of_int i) builder
          done;
          set_value_name (value_name op) !res
        | _ ->
          assert (Array.length cv = 1 && type_of op = type_of cv.(0));
          res := cv.(0);
          if op <> !res then replace_all_uses_with op !res);
        if op <> !res then
          potentially_dead_instrs := op :: !potentially_dead_instrs
      end
    ) gathered;
    List.iter (fun instr ->
      if is_constant instr || use_empty instr then
        delete_instruction instr
    ) !potentially_dead_instrs;
    true
  end

let run_on_function func =
  let parallel_loop_access_md_kind = md_kind_id "llvm.mem.parallel_loop_access" in
  visit_function func parallel_loop_access_md_kind

let run_on_module m =
  iter_functions (fun f -> ignore (run_on_function f)) m