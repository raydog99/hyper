open Llvm
open Llvm_analysis
open Llvm_target

let is_float = function
  | Fadd | Fsub | Fmul | Fdiv | Frem -> true
  | _ -> false

module BinaryIROperation = struct
  type t = {
    opcode: Opcode.t;
    bitwidth: int;
  }

  let match op v matches =
    match value_kind v with
    | Instruction (Binop bin_op) when bin_op = op.opcode && has_bit_width v op.bitwidth ->
      let operands = [| get_operand v 0; get_operand v 1 |] in
      matches := { live_in = false; operands; result = v } :: !matches;
      true
    | _ -> false

  let get_maximum_vf op tti =
    load_store_vec_reg_bit_width tti 0 / op.bitwidth

  let get_name op =
    Printf.sprintf "%s-i%d" (string_of_opcode op.opcode) op.bitwidth
end

module UnaryIROperation = struct
  type t = {
    opcode: Opcode.t;
    bitwidth: int;
  }

  let match op v matches =
    match value_kind v with
    | Instruction i when instruction_opcode i = op.opcode && has_bit_width v op.bitwidth ->
      let operands = [| get_operand v 0 |] in
      matches := { live_in = false; operands; result = v } :: !matches;
      true
    | _ -> false

  let get_maximum_vf op tti =
    load_store_vec_reg_bit_width tti 0 / op.bitwidth

  let get_name op =
    Printf.sprintf "%s-%d" (string_of_opcode op.opcode) op.bitwidth
end

module IRVectorBinding = struct
  type t = {
    op: BinaryIROperation.t;
    name: string;
    sig_: InstSignature.t;
    lane_ops: BoundOperation.t list;
  }

  let get_cost b tti ctx =
    let scalar_ty =
      if is_float b.op.opcode then
        if b.op.bitwidth = 32 then float_type ctx
        else double_type ctx
      else integer_type ctx b.op.bitwidth
    in
    let num_elems = List.length b.lane_ops in
    let vec_ty = vector_type scalar_ty num_elems in
    arithmetic_instr_cost tti b.op.opcode vec_ty

  let create op vector_width =
    let sig_ = {
      input_widths = [vector_width; vector_width];
      output_widths = [vector_width];
      has_imm8 = false;
    } in
    let elem_width = op.bitwidth in
    let num_lanes = vector_width / elem_width in
    let lane_ops = List.init num_lanes (fun i ->
      let lo = i * elem_width in
      let hi = lo + elem_width in
      BoundOperation.create op [|(0, lo, hi); (1, lo, hi)|]
    ) in
    { op; name = BinaryIROperation.get_name op; sig_; lane_ops }

  let emit b operands builder =
    match operands with
    | [|op1; op2|] -> build_binop b.op.opcode op1 op2 "" builder
    | _ -> failwith "Invalid number of operands"

  let is_supported b tti =
    List.length b.lane_ops <= BinaryIROperation.get_maximum_vf b.op tti
end

module UnaryIRVectorBinding = struct
  type t = {
    op: UnaryIROperation.t;
    name: string;
    sig_: InstSignature.t;
    lane_ops: BoundOperation.t list;
  }

  let get_cost b tti ctx =
    let elem_width = b.op.bitwidth in
    let num_elems = List.length b.lane_ops in
    match b.op.opcode with
    | Fneg ->
      let ty = if elem_width = 32 then float_type ctx else double_type ctx in
      let vec_ty = vector_type ty num_elems in
      arithmetic_instr_cost tti b.op.opcode vec_ty
    | _ -> 1.0

  let create op vector_width =
    let sig_ = {
      input_widths = [vector_width];
      output_widths = [vector_width];
      has_imm8 = false;
    } in
    let elem_width = op.bitwidth in
    let num_lanes = vector_width / elem_width in
    let lane_ops = List.init num_lanes (fun i ->
      let lo = i * elem_width in
      let hi = lo + elem_width in
      BoundOperation.create op [|(0, lo, hi)|]
    ) in
    { op; name = UnaryIROperation.get_name op; sig_; lane_ops }

  let emit b operands builder =
    match operands with
    | [|op|] ->
      let ctx = context builder in
      let elem_width = b.op.bitwidth in
      let num_elems = List.length b.lane_ops in
      let float_ty = if elem_width = 32 then float_type ctx else double_type ctx in
      let int_ty = integer_type ctx elem_width in
      let vec_float_ty = vector_type float_ty num_elems in
      let vec_int_ty = vector_type int_ty num_elems in
      let v = op in
      match b.op.opcode with
      | Sitofp -> build_sitofp (try_cast builder v vec_int_ty) vec_float_ty "" builder
      | Fptosi -> build_fptosi (try_cast builder v vec_float_ty) vec_int_ty "" builder
      | Fneg -> build_fneg (try_cast builder v vec_float_ty) "" builder
      | _ -> failwith "Unsupported unary opcode"
    | _ -> failwith "Invalid number of operands"

  let is_supported b tti =
    List.length b.lane_ops <= UnaryIROperation.get_maximum_vf b.op tti
end