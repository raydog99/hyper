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

module PolyComonoid (C : Comonoid) : Comonoid with type 'a comonoid = 'a poly = struct
  type 'a comonoid = 'a poly

  let counit p =
    match p with
    | [] -> ()
    | (c, 0) :: _ -> C.counit c
    | _ -> failwith "Polynomial must be constant to have a counit"

  let comult p =
    let rec aux p xs ys =
      match p with
      | [] -> (xs, ys)
      | (c, 0) :: rest ->
         let (c1, c2) = C.comult c in
         aux rest ((c1, 0) :: xs) ((c2, 0) :: ys)
      | (c, n) :: rest ->
         aux rest ((c, n) :: xs) ((C.counit c, n) :: ys)
    in
    aux p [] []
end

type poly = { coef : float list; const : float }

let corolla p =
  { coef = p.const :: p.coef; const = 0.0 }

module PolyCoproduct = struct
  type 'a poly = 'a list

  let coproduct (p : 'a poly) (q : 'a poly) : 'a poly poly =
    let rec aux p q xs ys =
      match p, q with
      | [], [] -> (xs, ys)
      | [], y::ys -> aux [] ys xs (y::ys)
      | x::xs, [] -> aux xs [] (x::xs) ys
      | x::xs, y::ys -> aux xs ys (P [x]::xs) (Q [y]::ys)
    in
    let (ps, qs) = aux p q [] [] in
    ps @ qs

  and P x = x
  and Q x = x
end