type 'a pu = 'a -> unit
let pair pa pb = fun (a, b) -> pa a; pb b
let triple pa pb pc = fun (a, b, c) -> pa a; pb b; pc c
let quad pa pb pc pd = fun (a, b, c, d) -> pa a; pb b; pc c; pd d

let wrap (i, j) pa = fun x -> pa (j x); i x
let zero_to n = 
  let rec aux n = if n = 0 then (fun () -> 0) else wrap (fun x -> x/256, fun x -> x mod 256) (aux (n/256)) in
  aux n

let unit = fun () -> ()
let char = wrap (Char.code, Char.chr) (zero_to 255)
let bool = wrap (function true -> 1 | false -> 0, function 0 -> false | _ -> true) (zero_to 1)

let nat =
  let half = 128 in
  let rec aux n =
    if n < half then (fun x -> x)
    else wrap (fun x -> half + (x mod half), fun x -> x / half - 1) aux
  in
  aux

let fixed_list pa n =
  let rec aux n =
    if n = 0 then (fun () -> [])
    else wrap (fun (a, b) -> a :: b, fun l -> match l with a :: b -> (a, b) | [] -> failwith "impossible") (pair pa (aux (n-1)))
  in
  aux n

let list pa =
  let rec aux n =
    wrap (fun (n, l) -> l, fun l -> (List.length l, l)) (pair (nat n) (fixed_list pa n))
  in
  aux

let string = list char

let alt tag ps =
  let rec aux = function
    | [] -> failwith "alt: empty list"
    | p :: ps -> wrap (fun x -> (tag x, x), function (0, x) -> x | (n, x) -> aux ps (n-1, x)) p
  in
  aux (List.mapi (fun i p -> (i+1, p)) ps)

let p_maybe pa =
  alt (function None -> 0 | Some _ -> 1) [
    (fun () -> None);
    wrap (function x -> Some x, function None -> failwith "impossible" | Some x -> x) pa
  ]

let p_either pa pb =
  alt (function Left _ -> 0 | Right _ -> 1) [
    wrap (function x -> Left x, function Left x -> x | Right _ -> failwith "impossible") pa;
    wrap (function x -> Right x, function Left _ -> failwith "impossible" | Right x -> x) pb
  ]