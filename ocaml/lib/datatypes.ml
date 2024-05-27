(* Booleans *)
type BoolC = True | False

(* Natural Numbers *)
type Nat = Zero | Succ of Nat

(* Finite Lists *)
type ('a) ListC = Nil | Cons of 'a * ('a) ListC

(* Finite Binary Trees *)
type ('a) Btree = Bleaf of 'a | Bnode of ('a Btree) * ('a Btree)

(* Infinite Binary Trees *)
type ('a) Inftree = Node of ('a Inftree) * ('a Inftree)

(* Infinite Lists *)
type ('a) Inflist = Head of 'a * ('a) Inflist

(* Finite Database Trees *)
type ('a, 'b) DBtree = Dbleaf of 'a | Dbnode of 'b * ('a DBtree * 'a DBtree)

(* Non-empty Trees *)
type ('a) NEtree = Neleaf of 'a | Nebranch of 'a * ('a NEtree * 'a NEtree)

(* Non-empty Lists *)
type ('a) NElist = Necons of 'a * ('a) NElist | Neunit of 'a

(* CoLists *)
type ('a) coListC = Split of ('a) coListC * 'a

(* Co-Non-empty Trees *)
type ('a) coNEtree = Nenfone of ('a) coNEtree | Nefork of ('a) coNEtree * ('a) coNEtree

(* Co-Non-empty Lists *)
type ('a) coNElist = Nehead of ('a) coNElist | Netail of ('a) coNElist