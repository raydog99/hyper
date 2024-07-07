open Llvm
open Llvm_analysis
open Vector_pack_context

let is_less_than se a b =
  is_known_negative se (get_minus_scev se a b)

let refine_with_range se expr cr =
  let smin = Apint.get_signed_min_value (get_bit_width cr) in
  let umin = Apint.get_min_value (get_bit_width cr) in
  let smax = Apint.get_signed_max_value (get_bit_width cr) in
  let umax = Apint.get_max_value (get_bit_width cr) in
  let expr = if get_signed_min cr <> smax && contains cr (Constantrange.create (get_signed_min cr) smax)
             then get_smax_expr se expr (get_constant se (get_signed_min cr))
             else expr in
  let expr = if get_unsigned_min cr <> umax && contains cr (Constantrange.create (get_unsigned_min cr) umax)
             then get_umax_expr se expr (get_constant se (get_unsigned_min cr))
             else expr in
  let expr = if get_upper cr <> smin && contains cr (Constantrange.create smin (get_upper cr))
             then get_smin_expr se expr (get_constant se (get_signed_max cr))
             else expr in
  let expr = if umin <> get_upper cr && contains cr (Constantrange.create umin (get_upper cr))
             then get_umin_expr se expr (get_constant se (get_unsigned_max cr))
             else expr in
  expr

let refine_with_ranges se expr ranges =
  let value_to_scev = Hashtbl.create (Hashtbl.length ranges) in
  Hashtbl.iter (fun v r -> Hashtbl.add value_to_scev v (refine_with_range se (get_scev se v) r)) ranges;
  Scev_parameter_rewriter.rewrite expr se value_to_scev

module UnknownScevCollector = struct
  type t = {
    se : llscalarevoution;
    values : (llvalue, unit) Hashtbl.t;
  }

  let create se =
    { se; values = Hashtbl.create 16 }

  let visit_unknown t expr =
    if is_integer_ty (type_of (value expr)) then
      Hashtbl.add t.values (value expr) ();
    expr
end

let get_location i =
  match value_classification i with
  | Store -> MemoryLocation.get_store i
  | Load -> MemoryLocation.get_load i
  | _ -> MemoryLocation.create ()

let is_simple i =
  match value_classification i with
  | Load -> is_simple_load i
  | Store -> is_simple_store i
  | MemIntrinsic -> not (is_volatile i)
  | _ -> true

let get_loop_for_pointer li ptr =
  match value_classification ptr with
  | Instruction i -> get_loop_for li (parent i)
  | _ -> None

let get_base_value s =
  match scev_classification s with
  | AddRec ar -> get_base_value (get_start ar)
  | Add a ->
      let last = get_operand a ((get_num_operands a) - 1) in
      if is_pointer_ty (type_of last) then get_base_value last else None
  | Unknown u -> Some (value u)
  | _ -> None

let is_aliased i1 i2 aa se li lvi =
  let loc1 = get_location i1 in
  let loc2 = get_location i2 in
  let f = block_parent (instr_parent i1) in
  if loc1.ptr <> None && loc2.ptr <> None && is_simple i1 && is_simple i2 then
    let result = alias aa loc1 loc2 in
    if result <> MayAlias then result else
    let ptr1 = get_load_store_pointer_operand i1 in
    let ptr2 = get_load_store_pointer_operand i2 in
    match ptr1, ptr2 with
    | Some ptr1, Some ptr2 ->
        let ptr1_scev = get_scev se ptr1 in
        let ptr2_scev = get_scev se ptr2 in
        let base1 = get_base_value ptr1_scev in
        let base2 = get_base_value ptr2_scev in
        match base1, base2 with
        | Some base1, Some base2 when base1 <> base2 ->
            alias aa (MemoryLocation.get_before_or_after base1) (MemoryLocation.get_before_or_after base2)
        | _ ->
            let loops = ref [] in
            let collect_loops s =
              match scev_classification s with
              | AddRec ar -> loops := get_loop ar :: !loops; true
              | _ -> true
            in
            ignore (Scev_traversal.visit_all collect_loops ptr1_scev);
            ignore (Scev_traversal.visit_all collect_loops ptr2_scev);
            let compare_loops l1 l2 =
              l1 = l2 || contains l1 l2
            in
            List.stable_sort compare_loops !loops;
            if List.exists2 (fun l1 l2 -> not (compare_loops l1 l2)) !loops (List.tl !loops) then
              true
            else
              let lt = is_less_than se ptr1_scev ptr2_scev in
              let gt = is_less_than se ptr2_scev ptr1_scev in
              if not lt && not gt then true
              else
                let (ptr1_scev, ptr2_scev) = if gt then (ptr2_scev, ptr1_scev) else (ptr1_scev, ptr2_scev) in
                let ty = element_type (type_of ptr1) in
                let as_ = address_space (type_of ptr1) in
                let dl = data_layout (global_parent f) in
                let index_width = index_size_in_bits dl as_ in
                let size = Apint.of_int (store_size dl ty) index_width in
                is_known_positive se (get_minus_scev se (get_add_expr se [ptr1_scev; get_constant se size]) ptr2_scev)
    | _ -> true
  else
    true

class global_dependence_analysis aa se li lvi f vp_ctx no_alias = object(self)
  val mutable transitive_closure = Hashtbl.create 16

  method private add_dependences i deps =
    assert (is_known_value vp_ctx i);
    let depended = Bitvector.create (get_num_values vp_ctx) in
    List.iter (fun dep ->
      Bitvector.set depended (get_scalar_id vp_ctx dep);
      match Hashtbl.find_opt transitive_closure dep with
      | Some bits -> Bitvector.or_ depended bits
      | None -> ()
    ) deps;
    Hashtbl.add transitive_closure i depended

  method add_instruction i =
    let deps = List.filter_map (fun o -> match value_classification o with Instruction i -> Some i | _ -> None) (operands i) in
    self#add_dependences i deps

  initializer
    let mem_refs = ref [] in
    let dependences = Hashtbl.create 16 in
    let rpo = ref [] in
    compute_rpo f li rpo;
    let processed = Hashtbl.create 16 in
    List.iter (fun bb ->
      let l = get_loop_for li bb in
      let is_header = is_loop_header li bb in
      assert (not is_header || l <> None);
      iter_instrs (fun i ->
        Hashtbl.add processed i ();
        List.iter (fun v ->
          match value_classification v with
          | Instruction op_inst ->
              let is_loop_carried_dep = is_header && value_classification i = PHINode && contains l op_inst in
              if not is_loop_carried_dep then
                Hashtbl.add dependences i (op_inst :: (Hashtbl.find_opt dependences i |> Option.value ~default:[]))
          | _ -> ()
        ) (operands i);
        (match value_classification i with
        | PHINode when is_header ->
            (match l with
            | Some l ->
                let pn_loop = get_loop_for li (instr_parent i) in
                (match pn_loop with
                | Some pn_loop when get_header pn_loop = instr_parent i && not (contains pn_loop i) ->
                    (match get_latch pn_loop with
                    | Some latch ->
                        (match get_incoming_value i latch with
                        | Some (Instruction i2) ->
                            Hashtbl.add dependences i (i2 :: (Hashtbl.find_opt dependences i |> Option.value ~default:[]))
                        | _ -> ())
                    | None -> ())
                | _ -> ())
            | None -> ())
        | _ -> ());
        if not (is_intrinsic i Intrinsic.ExperimentalNoAliasScopeDecl ||
                is_intrinsic i Intrinsic.LifetimeStart ||
                is_intrinsic i Intrinsic.LifetimeEnd) &&
           (value_classification i = ReturnInst || no_alias || may_read_or_write_memory i) then
          List.iter (fun prev_ref ->
            if value_classification prev_ref = ReturnInst || may_write_to_memory prev_ref || may_write_to_memory i then
              if is_aliased i prev_ref aa se li lvi then
                Hashtbl.add dependences i (prev_ref :: (Hashtbl.find_opt dependences i |> Option.value ~default:[]))
          ) !mem_refs;
          mem_refs := i :: !mem_refs
      ) bb
    ) !rpo;
    List.iter (fun bb ->
      iter_instrs (fun i ->
        self#add_dependences i (Hashtbl.find_opt dependences i |> Option.value ~default:[])
      ) bb
    ) !rpo
end