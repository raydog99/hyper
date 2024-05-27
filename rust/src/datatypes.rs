enum BoolC {
    True,
    False,
}

enum Nat {
    Zero,
    Succ(Box<Nat>),
}

enum<T> ListC {
    Nil,
    Cons(T, Box<ListC<T>>),
}

enum<T> Btree {
    Bleaf(T),
    Bnode(Box<Btree<T>>, Box<Btree<T>>),
}

enum<T> Inftree {
    Node(Box<Inftree<T>>, Box<Inftree<T>>),
}

enum<T> Inflist {
    Head(T, Box<Inflist<T>>),
}

struct<A, B> DBtree {
    kind: Kind,
    data: Data<A, B>,
}

enum<A, B> Kind {
    Dbleaf,
    Dbnode,
}

enum<A, B> Data<A, B> {
    Leaf(A),
    Node(B, Box<DBtree<A, B>>, Box<DBtree<A, B>>),
}

enum<T> NEtree {
    Neleaf(T),
    Nebranch(T, Box<NEtree<T>>, Box<NEtree<T>>),
}

enum<T> NElist {
    Necons(T, Box<NElist<T>>),
    Neunit(T),
}

enum<T> coListC {
    Split(Box<coListC<T>>, T),
}

enum<T> coNEtree {
    Nenfone(Box<coNEtree<T>>, T),
    Nefork(Box<coNEtree<T>>, Box<coNEtree<T>>),
}

enum<T> coNElist {
    Nehead(Box<coNElist<T>>),
    Netail(T),
}

impl<T> NEtree<T> {
    fn is_ne_tree(&self) -> bool {
        true
    }
}

impl<T> NElist<T> {
    fn is_ne_list(&self) -> bool {
        true
    }
}

impl<T> coNEtree<T> {
    fn is_co_ne_tree(&self) -> bool {
        true
    }
}

impl<T> coNElist<T> {
    fn is_co_ne_list(&self) -> bool {
        true
    }
}