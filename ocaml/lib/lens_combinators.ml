type 're = Str of string | Re of string

type 'a lens =
  | Del of 're * string
  | Store of 're
  | Value of string
  | Counter of string
  | Seq of string
  | Key of 're
  | Label of string

let rec concat l1 l2 = function
  | [] -> []
  | s :: rest ->
     (match l1 s with
      | [] -> concat l2 (s :: rest)
      | l1_res ->
         match concat l2 l1_res with
         | [] -> l1_res
         | l2_res -> l2_res @ concat (concat l1 l2) rest)

let union l1 l2 s =
  match l1 s with
  | [] -> l2 s
  | res -> res

let rec repetition op l = function
  | [] -> []
  | s :: rest ->
     (match op with
      | '*' -> (match l s with
                | [] -> repetition op l rest
                | res -> res @ repetition op l rest)
      | '+' -> (match l s with
                | [] -> []
                | res -> res @ repetition op l rest)
      | '?' -> (match l s with
                | [] -> repetition op l rest
                | res -> res @ repetition op l rest)
      | _ -> invalid_arg "Invalid repetition operator")

let subtree l =
  let rec process_subtree = function
    | [] -> []
    | s :: rest ->
       (match l s with
        | [] -> process_subtree rest
        | res -> res :: process_subtree rest)
  in
  process_subtree

let square left body right s =
  let rec process_square = function
    | [] -> []
    | s :: rest ->
       (match left s with
        | [] -> process_square rest
        | left_res ->
           match right left_res with
           | [] -> process_square rest
           | right_res ->
              let mid = List.rev left_res @ right_res in
              (body mid) @ process_square rest)
  in
  process_square s