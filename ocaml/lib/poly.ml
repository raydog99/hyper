module Poly = struct
  type 'a t = 'a list

  let create coeffs = coeffs

  let eval p x =
    let rec eval' p x acc =
      match p with
      | [] -> acc
      | c :: rest -> eval' rest x (acc +. (c *. x))
    in
    eval' p x 0.0

  let map f p =
    List.map f p

  let sum p q =
    List.rev_map2 (+) (List.rev p) (List.rev q)

  let product p q =
    let rec product' p q =
      match q with
      | [] -> []
      | c :: rest ->
        let mul_p = map (fun x -> x *. c) p in
        List.rev_append mul_p (0.0 :: product' p rest)
    in
    product' p q
end