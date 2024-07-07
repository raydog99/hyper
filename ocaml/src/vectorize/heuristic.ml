open Llvm
open VectorPack
open Packer
open Solver

let c_splat = 1.0
let c_perm = 2.0
let c_insert = 2.0
let c_shuffle = 2.0
let c_extract = 1.0

let allow_deinterleave = ref false
let allow_transpose = ref false

module Heuristic = struct
  type solution = {
    cost : float;
    packs : VectorPack.t list;
  }

  type t = {
    mutable solutions : (OperandPack.t, solution) Hashtbl.t;
    mutable scalar_costs : (llvalue, float) Hashtbl.t;
    pkr : Packer.t;
  }

  let create pkr =
    { solutions = Hashtbl.create 16;
      scalar_costs = Hashtbl.create 16;
      pkr;
    }

  let rec get_cost t vp =
    let cost = VectorPack.get_producing_cost vp in
    VectorPack.get_operand_packs vp
    |> List.fold_left (fun acc op ->
      if List.for_all (fun v -> match value_kind v with
        | Instruction CmpInst -> true
        | _ -> false) (OperandPack.to_list op)
      then acc
      else acc +. get_cost t op
    ) cost

  let deinterleave vp_ctx op stride =
    []

  let transpose vp_ctx op n =
    if OperandPack.size op mod n <> 0 then None
    else
      let m = OperandPack.size op / n in
      let t = List.init m (fun i ->
        List.init n (fun j ->
          OperandPack.get op (j * m + i)
        )
      ) |> List.flatten in
      Some (VectorPackContext.get_canonical_operand_pack vp_ctx t)

  let rec solve t op =
    match Hashtbl.find_opt t.solutions op with
    | Some sol -> sol
    | None ->
      Hashtbl.add t.solutions op { cost = 0.0; packs = [] };
      let cost = ref 0.0 in
      let inserted = Hashtbl.create 8 in
      OperandPack.iteri (fun i v ->
        match value_kind v with
        | Constant _ -> ()
        | _ ->
          if not (Hashtbl.mem inserted v) then begin
            Hashtbl.add inserted v ();
            cost := !cost +. get_cost_value t v +. c_insert
          end
      ) op;
      let sol = ref { cost = !cost; packs = [] } in
      if !cost = 0.0 then begin
        Hashtbl.replace t.solutions op !sol;
        !sol
      end else begin
        let broadcast_cost = get_cost_value t (OperandPack.front op) +. c_splat in
        if OperandPack.is_splat op && !cost > broadcast_cost then begin
          sol := { cost = broadcast_cost; packs = [] };
          Hashtbl.replace t.solutions op !sol;
          !sol
        end else begin
          let vp_ctx = Packer.get_context t.pkr in
          let deduped = VectorPackContext.dedup vp_ctx op in
          let extra_cost = if deduped <> op then c_shuffle else 0.0 in
          let opi = Packer.get_producer_info t.pkr deduped in
          List.iter (fun vp ->
            let new_sol = { cost = get_cost t vp +. extra_cost; packs = [vp] } in
            if new_sol.cost < !sol.cost then sol := new_sol
          ) (OperandPackInfo.get_producers opi);
          if !allow_transpose then
            List.iter (fun n ->
              match transpose vp_ctx op n with
              | Some t' ->
                let opi = Packer.get_producer_info t.pkr t' in
                List.iter (fun vp ->
                  let new_sol = { cost = get_cost t vp +. c_perm; packs = [vp] } in
                  if new_sol.cost < !sol.cost then sol := new_sol
                ) (OperandPackInfo.get_producers opi)
              | None -> ()
            ) [2; 4; 8];
          if !allow_deinterleave then
            List.iter (fun stride ->
              if OperandPack.size deduped mod stride = 0 then
                let ops = deinterleave vp_ctx deduped stride in
                let cost = ref (c_shuffle *. float_of_int (List.length ops)) in
                let packs = ref [] in
                List.iter (fun op2 ->
                  let sol2 = solve t op2 in
                  packs := !packs @ sol2.packs;
                  cost := !cost +. sol2.cost;
                  if !cost > !sol.cost then raise Exit
                ) ops;
                let new_sol = { cost = !cost; packs = !packs } in
                if new_sol.cost < !sol.cost then sol := new_sol
            ) [2; 4; 8];
          Hashtbl.replace t.solutions op !sol;
          !sol
        end
      end

  let rec get_cost_value t v =
    match value_kind v with
    | Instruction _ ->
      (match Hashtbl.find_opt t.scalar_costs v with
      | Some cost -> cost
      | None ->
        Hashtbl.add t.scalar_costs v 0.0;
        let cost = Packer.get_scalar_cost t.pkr v in
        let cost = List.fold_left (fun acc op ->
          acc +. get_cost_value t op
        ) cost (operands v) in
        Hashtbl.replace t.scalar_costs v cost;
        cost)
    | _ -> 0.0
end