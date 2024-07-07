open Llvm
open ControlDependence
open DependenceAnalysis
open VLoop

module ControlReifier = struct
  type t = {
    ctx : llcontext;
    mutable reified_values : (control_condition * vloop, llvalue) Hashtbl.t;
    mutable inserted_insts : llvalue list;
  }

  let create ctx =
    { ctx; reified_values = Hashtbl.create 10; inserted_insts = [] }

  let rec reify cr c vl =
    match c with
    | None -> const_int (i1_type cr.ctx) 1
    | Some c ->
        match Hashtbl.find_opt cr.reified_values (c, vl) with
        | Some v -> v
        | None ->
            let reified =
              match c with
              | ConditionAnd and_c ->
                  reify cr and_c.parent vl |> ignore;
                  let cond =
                    if and_c.is_true then and_c.cond
                    else
                      let not_inst = create_not and_c.cond "not" in
                      cr.inserted_insts <- not_inst :: cr.inserted_insts;
                      VLoop.add_instruction vl not_inst and_c.parent;
                      not_inst
                  in
                  VLoop.create_one_hot_phi vl and_c.parent cond
                    (const_int (i1_type cr.ctx) 0) "reified.onehot"
              | ConditionOr or_c ->
                  List.fold_left
                    (fun acc c2 ->
                      let tmp = create_or acc (reify cr c2 vl) "or" in
                      cr.inserted_insts <- tmp :: cr.inserted_insts;
                      VLoop.add_instruction vl tmp None;
                      tmp)
                    (reify cr (List.hd or_c.conds) vl)
                    (List.tl or_c.conds)
            in
            Hashtbl.add cr.reified_values (c, vl) reified;
            (match c with
            | ConditionAnd and_c -> reify cr and_c.complement vl |> ignore
            | _ -> ());
            reified

  let has_value cr c vl =
    match c with None -> true | Some c -> Hashtbl.mem cr.reified_values (c, vl)

  let get_value cr c vl =
    assert (has_value cr c vl);
    match c with
    | None -> const_int (i1_type cr.ctx) 1
    | Some c -> Hashtbl.find cr.reified_values (c, vl)
end