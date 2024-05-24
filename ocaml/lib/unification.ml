type id = string

type term =
  | Var of id
  | Term of id * term list

type substitution = (id * term) list

let rec occurs (x : id) (t : term) : bool =
  match t with
  | Var y -> x = y
  | Term (_, s) -> List.exists (occurs x) s

let rec subst (s : term) (x : id) (t : term) : term =
  match t with
  | Var y -> if x = y then s else t
  | Term (f, u) -> Term (f, List.map (subst s x) u)

let apply (s : substitution) (t : term) : term =
  List.fold_right (fun (x, u) -> subst u x) s t

let rec unify_one (s : term) (t : term) : substitution =
  match (s, t) with
  | (Var x, Var y) -> if x = y then [] else [(x, t)]
  | (Term (f, sc), Term (g, tc)) ->
      if f = g && List.length sc = List.length tc
      then unify (List.combine sc tc)
      else failwith "not unifiable: head symbol conflict"
  | ((Var x, (Term (_, _) as t)) | ((Term (_, _) as t), Var x)) ->
      if occurs x t
      then failwith "not unifiable: circularity"
      else [(x, t)]

and unify (s : (term * term) list) : substitution =
  match s with
  | [] -> []
  | (x, y) :: t ->
      let t2 = unify t in
      let t1 = unify_one (apply t2 x) (apply t2 y) in
      t1 @ t2