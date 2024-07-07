open Llvm

let compute_rpo f li rpo =
  let worklist = ref [entry_block f] in
  let visited = Hashtbl.create 32 in
  let rec process_block bb =
    if not (Hashtbl.mem visited bb) then begin
      Hashtbl.add visited bb ();
      let succs = block_successors bb in
      List.iter (fun succ ->
        if Option.is_some (loop_for li succ) then
          worklist := succ :: !worklist
        else
          process_block succ
      ) succs;
      rpo := bb :: !rpo
    end
  in
  while !worklist <> [] do
    let bb = List.hd !worklist in
    worklist := List.tl !worklist;
    process_block bb
  done

let compute_loop_rpo li l rpo =
  let header = loop_header l in
  let worklist = ref [header] in
  let visited = Hashtbl.create 32 in
  let rec process_block bb =
    if not (Hashtbl.mem visited bb) then begin
      Hashtbl.add visited bb ();
      let succs = block_successors bb in
      List.iter (fun succ ->
        if contains l succ then
          if succ != header then
            process_block succ
          else
            ()
        else
          worklist := succ :: !worklist
      ) succs;
      rpo := bb :: !rpo
    end
  in
  while !worklist <> [] do
    let bb = List.hd !worklist in
    worklist := List.tl !worklist;
    process_block bb
  done

let compute_nested_loop_rpo li l rpo =
  let sub_loops = ref [] in
  let header = loop_header l in
  let worklist = ref [header] in
  let visited = Hashtbl.create 32 in
  let rec process_block bb =
    if not (Hashtbl.mem visited bb) then begin
      Hashtbl.add visited bb ();
      let succs = block_successors bb in
      List.iter (fun succ ->
        if contains l succ then
          let succ_loop = loop_for li succ in
          if Option.is_some succ_loop && get_loop_for li (Option.get succ_loop) = Some l then
            sub_loops := Option.get succ_loop :: !sub_loops
          else if succ != header then
            process_block succ
        else
          worklist := succ :: !worklist
      ) succs;
      rpo := bb :: !rpo
    end
  in
  while !worklist <> [] do
    let bb = List.hd !worklist in
    worklist := List.tl !worklist;
    process_block bb
  done;
  List.iter (fun sub_loop -> compute_loop_rpo li sub_loop rpo) !sub_loops

let la_rpo_recurse li l rpo =
  if is_outermost_loop li l then
    compute_rpo (loop_header l) li rpo
  else if has_nested_loops l then
    compute_nested_loop_rpo li l rpo
  else
    compute_loop_rpo li l rpo

let compute_la_rpo f li rpo =
  let top_level_loops = get_top_level_loops li in
  compute_rpo f li rpo;
  List.iter (fun l -> la_rpo_recurse li l rpo) top_level_loops