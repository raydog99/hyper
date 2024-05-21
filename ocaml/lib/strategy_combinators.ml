(* Strategy types *)
type 'a tp = TP of ('a -> 'a)
type 'a tu = TU of ('a -> 'a)

(* Strategy application *)
let applyTP (TP m) t = m t
let applyTU (TU m) a = m a

(* Strategy construction *)
let polyTP m = TP (fun x -> m x)
let polyTU m = TU (fun x -> m x)
let adhocTP m t = TP (fun x -> applyTP m (t x))
let adhocTU m t = TU (fun x -> applyTU m (t x))

(* Sequential composition *)
let seqTP m = TP (fun x -> applyTP m x)
let letTP m = TU (fun a -> applyTP m a)
let seqTU m = TP (fun x -> applyTU m x)
let letTU m = TU (fun a -> applyTU m (TU (fun b -> applyTU (TU a) b)))

(* Choice *)
let choiceTP m = TP (fun x -> applyTP m x)
let choiceTU m = TU (fun x -> applyTU m x)

(* Traversal combinators *)
let allTP m = TP (fun x -> x)
let oneTP m = TP (fun x -> x)
let allTU m a = TU (fun x -> x)
let oneTU m = TU (fun x -> x)