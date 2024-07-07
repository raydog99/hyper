open Llvm

let create intrinsic_builder name operands imm8 =
  let wrapper_name = Printf.sprintf "intrinsic_wrapper_%s_%d" name imm8 in
  let f = intrinsic_builder.inst_wrappers
          |> Function.lookup wrapper_name
          |> Option.get in
  assert (List.length (Function.basic_blocks f) = 1);
  let bb = Function.entry_block f in
  let num_args = Array.length (Function.params f) in
  assert (Array.length operands = num_args);
  let v_map = Hashtbl.create num_args in
  Array.iteri (fun i arg ->
    let operand = operands.(i) in
    assert (Llvm_analysis.cast_is_valid Bitcast operand (Type.type_of arg));
    let operand = Llvm.constrain_cast Bitcast operand (Type.type_of arg) "" intrinsic_builder.builder in
    Hashtbl.add v_map arg operand
  ) (Function.params f);
  let ret_val = ref None in
  List.iter (fun i ->
    match i with
    | Ret ret -> ret_val := Some (Return.value ret)
    | _ ->
      let new_i = Instruction.clone i in
      Instruction.insert new_i "" intrinsic_builder.builder;
      Hashtbl.add v_map i new_i;
      Instruction.remap_instruction new_i v_map [NoModuleLevelChanges; IgnoreMissingLocals];
      match new_i with
      | Call ci ->
        let callee = Call.called_function ci in
        assert (Function.is_intrinsic callee);
        let m = Instruction.parent new_i |> Basic_block.parent |> Function.parent in
        let intrinsic_decl = Module.declare_function m (Function.name callee) (Function.type_of callee) in
        Call.set_called_function ci intrinsic_decl
      | _ -> ()
  ) (Basic_block.instructions bb);
  match !ret_val with
  | Some ret_val ->
    let output = Hashtbl.find v_map ret_val in
    assert (Option.is_some output);
    Option.get output
  | None -> failwith "Wrapper not returning explicitly"