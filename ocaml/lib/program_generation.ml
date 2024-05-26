type ident = string

type envcomp =
  | Var of ident
  | Lam of ident * envcomp
  | App of envcomp * envcomp
  | LamD of ident * envcomp
  | AppD of envcomp * envcomp

type exp =
  | Var' of ident
  | Lam' of ident * exp
  | App' of exp * exp
  | LamD' of ident * exp
  | AppD' of exp * exp

type value =
  | Fun of (value -> value)
  | Code of exp

let rec spec e r =
  match e with
  | Var' x -> lookup r x
  | Lam' (x, e) -> Fun (fun y -> spec e (upd r x y))
  | App' (f, a) ->
     let Fun ff = spec f r in
     ff (spec a r)
  | LamD' (x, e) ->
     let xx = gensym () in
     let Code body = spec e (upd r x (Code (Var' xx))) in
     Code (Lam' (xx, body))
  | AppD' (f, a) ->
     let Code ff = spec f r in
     let Code aa = spec a r in
     Code (App' (ff, aa))