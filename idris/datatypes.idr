module CatDatatypes

data BoolC : Type where
  True  : BoolC
  False : BoolC

data Nat : Type where
  Zero  : Nat
  Succ : Nat -> Nat

data ListC (A : Type) : Type where
  Nil  : ListC A
  Cons : A -> ListC A -> ListC A

data Btree (A : Type) : Type where
  Bleaf : A -> Btree A
  Bnode : Btree A -> Btree A -> Btree A

data Inftree (A : Type) : Type where
  Node : Inftree A -> Inftree A -> Inftree A

data Inflist (A : Type) : Type where
  Head : A -> Inflist A -> Inflist A

data DBtree (A : Type) (B : Type) : Type where
  Dbleaf : A -> DBtree A B
  Dbnode : B -> DBtree A B -> DBtree A B -> DBtree A B

data NEtree (A : Type) : Type where
  | Neleaf : A -> NEtree A
  | Nebranch : A -> NEtree A -> NEtree A -> NEtree A

data NElist (A : Type) : Type where
  | Necons : A -> NElist A -> NElist A
  | Neunit : A -> NElist A

data coListC (A : Type) : Type where
  | Split : coListC A -> A -> coListC A

data coNEtree (A : Type) : Type where
  | Nenfone : coNEtree A -> A -> coNEtree A
  | Nefork : coNEtree A -> coNEtree A -> coNEtree A

data coNElist (A : Type) : Type where
  | Nehead : coNElist A -> coNElist A
  | Netail : A -> coNElist A