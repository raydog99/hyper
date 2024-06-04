module dyn

import Data.Vect
import Data.List

record Arena where
       constructor MkArena
       pos : Type
       dis : pos -> Type

record Lens (dom : Arena) (cod : Arena) where
       constructor MkLens
       observe   : pos dom -> pos cod
       interpret : (p : pos dom) -> dis cod (observe p) -> dis dom p

idLens : (a : Arena) -> Lens a a
idLens a = MkLens id (\_ => id)

infixr 4 <.>
(<.>) : Lens a2 a3 -> Lens a1 a2 -> Lens a1 a3
(<.>) lens23 lens12 = MkLens obs int
      where
        obs : pos a1 -> pos a3
        obs = (observe lens23) . (observe lens12)

        int : (p : pos a1) -> (dis a3 (obs p)) -> dis a1 p
        int p = (interpret lens12 p) . (interpret lens23 (observe lens12 p))

Display : Arena -> Type  
Display a = (p : pos a ** dis a p)

AsFunctor : Arena -> Type -> Type
AsFunctor a y = (p : pos a ** dis a p -> y)

duoidal : {a1, a2, b1, b2 : Arena} -> Lens ((a1 :<>: a2) :*: (b1 :<>: b2)) ((a1 :*: b1) :<>: (a2 :*: b2))
duoidal {a1} {a2} {b1} {b2} =
  let x = (a1 :<>: a2) :*: (b1 :<>: b2)
      y = (a1 :*: b1) :<>: (a2 :*: b2)
      o : Pos x -> Pos y
      o ((p1 ** p2), (q1 ** q2)) = ((p1, q1), \d => (fst d $> p2, snd d $> q2))
      i : (p : Pos x) -> Dis y (o p) -> Dis x p
      i ((p1 ** p2), (q1 ** q2)) ((de1 ** de2) : Dis y (o ((p1 ** p2), (q1 ** q2)))) =
        ((fst de1 :** fst de2), (snd de1 :** snd de2))
  in MkLens o i