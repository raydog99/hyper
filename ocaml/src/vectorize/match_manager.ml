open Llvm

type t = {
  op_matches: (Operation.t, Operation.match_t list) Hashtbl.t;
}

let sort_by_output a b =
  compare a.Operation.output b.Operation.output

let match_value t v =
  Hashtbl.iter (fun op matches ->
    Operation.match_ op v matches
  ) t.op_matches

let create insts f =
  let t = { op_matches = Hashtbl.create 16 } in
  List.iter (fun inst ->
    List.iter (fun lane_op ->
      let op = LaneOp.get_operation lane_op in
      if not (Hashtbl.mem t.op_matches op) then
        Hashtbl.add t.op_matches op []
    ) (InstBinding.get_lane_ops inst)
  ) insts;
  iter_instrs (fun i -> match_value t i) f;
  Hashtbl.iter (fun _ matches ->
    List.sort sort_by_output matches
  ) t.op_matches;
  t

let create_with_instructions insts to_match =
  let t = { op_matches = Hashtbl.create 16 } in
  List.iter (fun inst ->
    List.iter (fun lane_op ->
      let op = LaneOp.get_operation lane_op in
      if not (Hashtbl.mem t.op_matches op) then
        Hashtbl.add t.op_matches op []
    ) (InstBinding.get_lane_ops inst)
  ) insts;
  List.iter (match_value t) to_match;
  Hashtbl.iter (fun _ matches ->
    List.sort sort_by_output matches
  ) t.op_matches;
  t

let get_matches t op =
  Hashtbl.find t.op_matches op

let get_matches_for_output t op output =
  let matches = get_matches t op in
  let dummy_match = { Operation.live_in = false; operands = []; output } in
  let lower = List.find_opt (fun m -> sort_by_output m dummy_match >= 0) matches in
  let upper = List.find_opt (fun m -> sort_by_output m dummy_match > 0) matches in
  match lower, upper with
  | Some l, Some u ->
      let start = List.index_of l matches in
      let end_ = List.index_of u matches in
      List.sub matches start (end_ - start)
  | _ -> []