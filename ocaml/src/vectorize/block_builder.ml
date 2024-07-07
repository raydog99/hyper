open Llvm

module ControlDependence = struct
  type t
  type condition_and
  type condition_or
end

type block_builder = {
  ctx : llcontext;
  f : llvalue; 
  emit_condition : llvalue -> llvalue;
  mutable active_conds : (ControlDependence.t option * llbasicblock) list;
  mutable semi_active_conds : (ControlDependence.t * ControlDependence.t list) list;
  mutable dummy_counter : int;
}

let create_block_builder entry_bb emit_condition =
  let ctx = basic_block_context entry_bb in
  let f = block_parent entry_bb in
  {
    ctx;
    f;
    emit_condition;
    active_conds = [(None, entry_bb)];
    semi_active_conds = [];
    dummy_counter = 0;
  }

let create_block bb = append_block (basic_block_context bb) "" (block_parent bb)

module ConditionEmitter = struct
  type t = {
    builder : llbuilder;
    common : ControlDependence.t option;
    emit_condition : llvalue -> llvalue;
    mutable emitted : (ControlDependence.t, llvalue) Hashtbl.t;
  }

  let create bb common emit_condition =
    {
      builder = builder_at_end (basic_block_context bb) bb;
      common;
      emit_condition;
      emitted = Hashtbl.create 16;
    }

  let rec emit ce c =
    match c with
    | None -> const_int (i1_type ce.builder.context) 1
    | Some c when c = ce.common -> const_int (i1_type ce.builder.context) 1
    | Some (ControlDependence.ConditionAnd and_c) -> emit_and ce and_c
    | Some (ControlDependence.ConditionOr or_c) -> emit_or ce or_c

  let emit_and ce and_c =
    match Hashtbl.find_opt ce.emitted and_c with
    | Some v -> v
    | None ->
        let v =
          build_and
            (emit ce and_c.parent)
            (if and_c.is_true then
               ce.emit_condition and_c.cond
             else
               build_not (ce.emit_condition and_c.cond) "" ce.builder)
            "" ce.builder
        in
        Hashtbl.add ce.emitted and_c v;
        v

  let emit_or ce or_c =
    match Hashtbl.find_opt ce.emitted or_c with
    | Some v -> v
    | None ->
        let v = emit_disjunction ce or_c.conds in
        Hashtbl.add ce.emitted or_c v;
        v

  let emit_disjunction ce conds =
    let values = List.map (emit ce) conds in
    build_or (Array.of_list values) "" ce.builder
end

let get_block_for bb c =
  let rec find_active_conds c conds =
    let visited = Hashtbl.create 16 in
    let rec dfs c2 =
      if not (Hashtbl.mem visited c2) then begin
        Hashtbl.add visited c2 ();
        if List.mem_assoc (Some c2) bb.active_conds then
          Hashtbl.add conds c2 ()
        else
          match List.assoc_opt c2 bb.semi_active_conds with
          | Some children -> List.iter dfs children
          | None -> ()
      end
    in
    dfs c;
    Hashtbl.to_seq_keys conds |> List.of_seq
  in

  match List.assoc_opt (Some c) bb.active_conds with
  | Some block -> block
  | None ->
      if List.mem_assoc c bb.semi_active_conds then
        let conds = find_active_conds c (Hashtbl.create 16) in
        let new_bb = create_block bb.f in
        List.iter
          (fun c2 ->
            let old_bb = List.assoc (Some c2) bb.active_conds in
            build_br new_bb (builder_at_end bb.ctx old_bb);
            bb.active_conds <- List.remove_assoc (Some c2) bb.active_conds)
          conds;
        bb.active_conds <- (Some c, new_bb) :: bb.active_conds;
        new_bb
      else
        match c with
        | ControlDependence.ConditionAnd and_c ->
            let if_true = create_block bb.f in
            let if_false = create_block bb.f in
            let parent_bb = get_block_for bb and_c.parent in
            let cond = bb.emit_condition and_c.cond in
            build_cond_br cond if_true if_false (builder_at_end bb.ctx parent_bb);
            let result_bb = if and_c.is_true then if_true else if_false in
            bb.active_conds <- List.remove_assoc (Some and_c.parent) bb.active_conds;
            bb.semi_active_conds <- (and_c.parent, [and_c; and_c.complement]) :: bb.semi_active_conds;
            bb.active_conds <- (Some and_c, result_bb) :: bb.active_conds;
            bb.active_conds <- (Some and_c.complement, if and_c.is_true then if_false else if_true) :: bb.active_conds;
            result_bb
        | ControlDependence.ConditionOr or_c ->
            let common_c = or_c.greatest_common_cond in
            let conds =
              if not (List.mem_assoc common_c bb.semi_active_conds) then begin
                ignore (get_block_for bb common_c);
                [common_c]
              end else
                find_active_conds common_c (Hashtbl.create 16)
            in
            let new_bb = create_block bb.f in
            let aux_bb = create_block bb.f in
            let conds_to_join = Hashtbl.create 16 in
            List.iter (fun c -> Hashtbl.add conds_to_join c ()) or_c.conds;
            let joined = Hashtbl.create 16 in
            List.iter
              (fun c2 ->
                let old_bb = List.assoc (Some c2) bb.active_conds in
                if Hashtbl.mem conds_to_join c2 then begin
                  build_br new_bb (builder_at_end bb.ctx old_bb);
                  Hashtbl.add joined c2 ()
                end else
                  build_br aux_bb (builder_at_end bb.ctx old_bb);
                bb.active_conds <- List.remove_assoc (Some c2) bb.active_conds)
              conds;
            let unjoined_conds =
              List.filter (fun c -> not (Hashtbl.mem joined c)) or_c.conds
            in
            let drain_bb = create_block bb.f in
            if unjoined_conds = [] then
              build_br drain_bb (builder_at_end bb.ctx aux_bb)
            else
              let ce = ConditionEmitter.create aux_bb (Some common_c) bb.emit_condition in
              let cond = ConditionEmitter.emit_disjunction ce unjoined_conds in
              build_cond_br cond new_bb drain_bb (builder_at_end bb.ctx aux_bb);
            let dummy_c = get_dummy_condition bb in
            bb.active_conds <- (Some c, new_bb) :: bb.active_conds;
            bb.active_conds <- (Some dummy_c, drain_bb) :: bb.active_conds;
            bb.semi_active_conds <- (common_c, [c; dummy_c]) :: bb.semi_active_conds;
            new_bb

let get_dummy_condition bb =
  bb.dummy_counter <- bb.dummy_counter + 1;
  Obj.magic bb.dummy_counter

let set_block_for_condition bb block c =
  assert (List.mem_assoc (Some c) bb.active_conds);
  bb.active_conds <- (Some c, block) :: List.remove_assoc (Some c) bb.active_conds