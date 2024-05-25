from typing import Dict, List, Tuple

Id = str

class Term:
    def __init__(self, id: Id, args: List['Term'] = None):
        self.id = id
        self.args = args or []

    def isVar(self) -> bool:
        return not self.args

    def __repr__(self):
        if self.isVar():
            return self.id
        else:
            return f"{self.id}({', '.join(repr(arg) for arg in self.args)})"

Substitution = Dict[Id, Term]

def occurs(x: Id, t: Term) -> bool:
    if t.isVar():
        return x == t.id
    else:
        return any(occurs(x, arg) for arg in t.args)

def subst(s: Term, x: Id, t: Term) -> Term:
    if t.isVar():
        return s if x == t.id else t
    else:
        return Term(t.id, [subst(s, x, arg) for arg in t.args])

def apply(s: Substitution, t: Term) -> Term:
    if t.isVar():
        return s.get(t.id, t)
    else:
        return Term(t.id, [apply(s, arg) for arg in t.args])

def unifyOne(s: Term, t: Term) -> Substitution:
    if s.isVar():
        if t.isVar():
            return {s.id: t} if s.id != t.id else {}
        elif occurs(s.id, t):
            raise ValueError("Not unifiable: circularity")
        else:
            return {s.id: t}
    elif t.isVar():
        if occurs(t.id, s):
            raise ValueError("Not unifiable: circularity")
        else:
            return {t.id: s}
    elif s.id != t.id or len(s.args) != len(t.args):
        raise ValueError("Not unifiable: head symbol conflict")
    else:
        sub = {}
        for s_arg, t_arg in zip(s.args, t.args):
            sub.update(unifyOne(apply(sub, s_arg), apply(sub, t_arg)))
        return sub

def unify(pairs: List[Tuple[Term, Term]]) -> Substitution:
    sub = {}
    for s, t in pairs:
        sub.update(unifyOne(apply(sub, s), apply(sub, t)))
    return sub