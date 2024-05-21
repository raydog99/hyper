type 'a term =
  | Var of int
  | App of 'a term * 'a term
  | Lam of string * 'a term

let rec eval (t: 'a term) (env: (string * 'a) list): 'a =
  match t with
    | Var (n) -> List.nth env n
    | App (t1, t2) ->
      let v1 = eval t1 env in
      let v2 = eval t2 env in
      v1 (v2)
    | Lam (x, t) ->
      fun v -> eval t ((x, v) :: env)

let fix (t: 'a term -> 'a term) : 'a term =
  let x = Var 0 in
  Lam ("x", App (t x, x))

let id = Lam ("x", Var 0)

let k = Lam ("x", Lam ("y", x))

let b = Lam ("x", Lam ("y", Lam ("z", y)))