open Llvm
open Llvm_analysis

let get_loop_nest li v =
  match value_as_instruction v with
  | Some i ->
    let rec loop_nest l acc =
      match l with
      | None -> List.rev acc
      | Some l -> loop_nest (loop_parent l) (l :: acc)
    in
    loop_nest (loop_for_instruction li i) []
  | None -> []

let get_address_space_operand i =
  match value_classification i with
  | Load -> load_pointer_address_space i
  | Store -> store_pointer_address_space i
  | _ -> -1

module AddRecLoopRewriter = struct
  type t = {
    se: llscalarevoution;
    loops: (llloop, llloop) Hashtbl.t;
    mutable success: bool;
  }

  let create se loops =
    { se; loops; success = true }

  let rec rewrite t expr =
    match scev_classification expr with
    | AddRec ar ->
      let old_loop = scev_addrec_loop ar in
      let new_loop = Hashtbl.find_opt t.loops old_loop |> Option.value ~default:old_loop in
      let operands = List.map (rewrite t) (scev_addrec_operands ar) in
      if List.for_all (fun op -> scev_is_available_at_loop_entry t.se op new_loop) operands then
        scev_add_rec t.se operands new_loop (scev_addrec_flags ar)
      else (
        t.success <- false;
        expr
      )
    | _ -> expr

  let rewrite se expr loops =
    let t = create se loops in
    let result = rewrite t expr in
    if t.success then result else expr
end

let is_equivalent ptr_a ptr_b se li =
  incr num_equiv_checks;
  if scev_eq (get_scev se ptr_a) (get_scev se ptr_b) then true
  else
    match value_as_instruction ptr_a, value_as_instruction ptr_b with
    | Some a, Some b ->
      if type_of ptr_a <> type_of ptr_b then false
      else
        let loop_nest1 = get_loop_nest li a in
        let loop_nest2 = get_loop_nest li b in
        if List.length loop_nest1 <> List.length loop_nest2 then false
        else
          let loops = Hashtbl.create (List.length loop_nest1) in
          List.iter2 (fun l1 l2 ->
            if not (have_identical_trip_counts l1 l2 se) then false
            else Hashtbl.add loops l2 l1
          ) loop_nest1 loop_nest2;
          let ptr_scev_a = get_scev se ptr_a in
          let ptr_scev_b = AddRecLoopRewriter.rewrite se (get_scev se ptr_b) loops in
          scev_eq ptr_scev_a ptr_scev_b
    | _ -> false

let is_consecutive a b dl se li =
  incr num_consec_checks;
  let ptr_a = get_load_store_pointer_operand a in
  let ptr_b = get_load_store_pointer_operand b in
  match ptr_a, ptr_b with
  | Some ptr_a, Some ptr_b ->
    let asa = get_address_space_operand a in
    let asb = get_address_space_operand b in
    let loop_nest1 = get_loop_nest li ptr_a in
    let loop_nest2 = get_loop_nest li ptr_b in
    if List.length loop_nest1 <> List.length loop_nest2 then false
    else
      let loops = Hashtbl.create (List.length loop_nest1) in
      List.iter2 (fun l1 l2 ->
        if l1 <> l2 && not (have_identical_trip_counts l1 l2 se) then false
        else if l1 <> l2 then Hashtbl.add loops l2 l1
      ) loop_nest1 loop_nest2;
      if asa <> asb || type_of ptr_a <> type_of ptr_b || ptr_a = ptr_b then false
      else
        let idx_width = index_size_in_bits dl asa in
        let ty = element_type (type_of ptr_a) in
        let offset_a, stripped_ptr_a = strip_and_accumulate_inbounds_constant_offsets dl ptr_a in
        let offset_b, stripped_ptr_b = strip_and_accumulate_inbounds_constant_offsets dl ptr_b in
        let asa = address_space (type_of stripped_ptr_a) in
        let asb = address_space (type_of stripped_ptr_b) in
        if asa <> asb then false
        else
          let idx_width = index_size_in_bits dl asa in
          let offset_a = const_int_sext_or_trunc offset_a idx_width in
          let offset_b = const_int_sext_or_trunc offset_b idx_width in
          let size = const_int (i64_type (global_context ())) (store_size dl ty) in
          let offset_scev_a = get_constant se offset_a in
          let offset_scev_b = get_constant se offset_b in
          let offset_delta_scev = get_minus_scev se offset_scev_b offset_scev_a in
          let offset_delta = scev_constant_value offset_delta_scev in
          if stripped_ptr_a = stripped_ptr_b then
            const_int_eq offset_delta size
          else
            let size_scev = get_constant se size in
            let base_delta = get_minus_scev se size_scev offset_delta_scev in
            let ptr_scev_a = get_scev se stripped_ptr_a in
            let ptr_scev_b = get_scev se stripped_ptr_b in
            let ptr_scev_b = if Hashtbl.length loops > 0 then AddRecLoopRewriter.rewrite se ptr_scev_b loops else ptr_scev_b in
            let x = get_add_expr se [ptr_scev_a; base_delta] in
            scev_eq x ptr_scev_b
  | _ -> false

let find_consecutive_accesses se dl li accesses equivalent_accesses num_fingerprints =
  if Array.length accesses = 0 then []
  else
    let fingerprints_to_accesses = Hashtbl.create 16 in
    let access_to_fingerprints = Hashtbl.create 16 in
    let ptr = get_load_store_pointer_operand accesses.(0) |> Option.get in
    let ty = element_type (type_of ptr) in
    let size = store_size dl ty in
    let consecutive_accesses = ref [] in
    let sg = SizeGenerator.create () in
    let igs = Array.init num_fingerprints (fun i -> IterationGenerator.create i) in
    Array.iter (fun i ->
      let ptr = get_load_store_pointer_operand i |> Option.get in
      let ptr_scev = get_scev se ptr in
      let fingerprints = fingerprint_scev se ptr_scev sg igs num_fingerprints in
      let left = fingerprints.(0) - size in
      Hashtbl.find_opt fingerprints_to_accesses left
      |> Option.iter (fun left_accesses ->
        List.iter (fun left_i ->
          let left_fingerprints = Hashtbl.find access_to_fingerprints left_i in
          if Array.for_all2 (fun a b -> a + size = b) left_fingerprints fingerprints &&
             is_consecutive left_i i dl se li then
            consecutive_accesses := (left_i, i) :: !consecutive_accesses
        ) left_accesses
      );
      Hashtbl.find_opt fingerprints_to_accesses fingerprints.(0)
      |> Option.iter (fun equiv_accesses ->
        List.iter (fun i2 ->
          let fingerprints2 = Hashtbl.find access_to_fingerprints i2 in
          if Array.for_all2 (fun a b -> a = b) fingerprints2 fingerprints &&
             is_equivalent (get_load_store_pointer_operand i |> Option.get)
                           (get_load_store_pointer_operand i2 |> Option.get)
                           se li then
            EquivalenceClasses.union_sets equivalent_accesses i i2
        ) equiv_accesses
      );
      let right = fingerprints.(0) + size in
      Hashtbl.find_opt fingerprints_to_accesses right
      |> Option.iter (fun right_accesses ->
        List.iter (fun right_i ->
          let right_fingerprints = Hashtbl.find access_to_fingerprints right_i in
          if Array.for_all2 (fun a b -> a = b + size) right_fingerprints fingerprints &&
             is_consecutive i right_i dl se li then
            consecutive_accesses := (i, right_i) :: !consecutive_accesses
        ) right_accesses
      );
      Hashtbl.add fingerprints_to_accesses fingerprints.(0) (i :: Hashtbl.find_opt fingerprints_to_accesses fingerprints.(0) |> Option.value ~default:[]);
      Hashtbl.add access_to_fingerprints i fingerprints
    ) accesses;
    !consecutive_accesses