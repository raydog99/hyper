open Llvm
open Llvm_analysis

module VectorPack = struct
  type t = {
    mutable kind : pack_kind;
    mutable producer : InstBinding.t option;
    mutable matches : Operation.Match.t option array;
    mutable loads : llvalue array;
    mutable stores : llvalue array;
    mutable phis : llvalue array;
    mutable rdx : ReductionInfo.t option;
    mutable rdx_len : int;
    mutable geps : llvalue array;
    mutable gammas : Gamma.t array;
    mutable cmps : llvalue array;
    mutable cp : ConditionPack.t option;
    mutable is_gather_scatter : bool;
    mutable operand_packs : OperandPack.t list;
    mutable ordered_values : llvalue list;
    mutable cost : float;
    mutable producing_cost : float;
    vp_ctx : VectorPackContext.t;
  }

  let compute_operand_packs_for_general vp =
    let sig_ = InstBinding.get_signature vp.producer in
    let num_inputs = InstSignature.num_inputs sig_ in
    let lane_ops = InstBinding.get_lane_ops vp.producer in
    let num_lanes = Array.length lane_ops in
    let operand_packs = Array.make num_inputs (OperandPack.create ()) in
    
    for i = 0 to num_inputs - 1 do
      let input_values = ref [] in
      let element_size = ref 0 in
      for j = 0 to num_lanes - 1 do
        let bound_slices = LaneOp.get_bound_slices lane_ops.(j) in
        Array.iteri (fun k bs ->
          if InputSlice.get_input_id bs = i then begin
            element_size := InputSlice.size bs;
            let v = match vp.matches.(j) with
              | Some m -> m.inputs.(k)
              | None -> null
            in
            input_values := { BoundInput.slice = bs; v } :: !input_values
          end
        ) bound_slices
      done;
      assert (!element_size <> 0);
      
      let input_values = List.sort (fun a b -> compare a.slice b.slice) !input_values in
      let cur_offset = ref 0 in
      let stride = InputSlice.size (List.hd input_values).slice in
      let op = operand_packs.(i) in
      List.iter (fun bv ->
        while !cur_offset < InputSlice.get_lo bv.slice do
          OperandPack.push op null;
          cur_offset := !cur_offset + stride
        done;
        assert (!cur_offset = InputSlice.get_lo bv.slice);
        OperandPack.push op bv.v;
        cur_offset := !cur_offset + stride
      ) input_values;
      let input_size = InstSignature.get_input_bitwidth sig_ i in
      while !cur_offset < input_size do
        OperandPack.push op null;
        cur_offset := !cur_offset + stride
      done;
      assert (OperandPack.size op * stride = input_size);
      
      if is_null (OperandPack.front op) && OperandPack.is_splat op then
        OperandPack.set_type op (vector_type (integer_type (VectorPackContext.get_context vp.vp_ctx) !element_size) (OperandPack.size op))
    done;
    
    Array.to_list operand_packs

end