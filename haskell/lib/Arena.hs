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