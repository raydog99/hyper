import Data.Bifunctor (second)

data Arena a = MkArena { pos :: a, dis :: a -> b }

data Lens dom cod = MkLens { observe :: pos dom -> pos cod, interpret :: pos dom -> (pos cod -> dis cod) -> dis dom }

idLens :: Arena a -> Lens a a
idLens a = MkLens id (\ p f -> f (id p))

(<.>) :: Lens a2 a3 -> Lens a1 a2 -> Lens a1 a3
lens23 <.> lens12 = MkLens obs int
  where
    obs p = observe lens23 (observe lens12 p)
    int p f = interpret lens12 p (interpret lens23 (observe lens12 p) . f . observe lens23)

type Display a = (pos a, dis a (pos a))
type AsFunctor a b = (pos a, dis a (pos a) -> b)

duoidal :: Arena a1 -> Arena a2 -> Arena b1 -> Arena b2 -> Lens ((a1 :<>: a2) :*: (b1 :<>: b2)) ((a1 :*: b1) :<>: (a2 :*: b2))
duoidal a1 a2 b1 b2 =
  let x = (a1 :<>: a2) :*: (b1 :<>: b2)
      y = (a1 :*: b1) :<>: (a2 :*: b2)
      o ((p1, p2), (q1, q2)) = ((p1, q1), \d -> (fst d $> p2, snd d $> q2))
      i ((p1, p2), (q1, q2)) ((de1, de2) : Dis y (o ((p1, p2), (q1, q2)))) = ((de1 :** fst de2), (snd de1 :** snd de2))
  in Lens o i