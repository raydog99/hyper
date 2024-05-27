data BoolC = True | False

data Nat = Zero | Succ Nat

data a ListC = Nil | Cons a (ListC a)

data a Btree = Bleaf a | Bnode (Btree a) (Btree a)

data a Inftree = Node (Inftree a) (Inftree a)

data a Inflist = Head a (Inflist a)

data a b DBtree = Dbleaf a | Dbnode b (DBtree a, DBtree a)

data a NEtree = Neleaf a | Nebranch a (NEtree a, NEtree a)

data a NElist = Necons a (NElist a) | Neunit a

data a coListC = Split (coListC a) a

data a coNEtree = Nenfone (coNEtree a) | Nefork (coNEtree a) (coNEtree a)

data a coNElist = Nehead (coNElist a) | Netail a
