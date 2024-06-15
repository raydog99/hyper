module LeftAdjoint (G : Functor.FUNCTOR) (H : Functor.FUNCTOR) = struct
  module GA = Functor.Apply(G)
  module HA = Functor.Apply(H)

  type 'a t = forall r. G.t (r -> 'a) -> H.t r

  let fmap f (c : 'a t) : 'b t =
    fun g -> H.fmap (fun x -> f (x g)) (c (G.fmap (fun f x -> f x) g))

  let liftF2 f (c1 : 'a t) (c2 : 'b t) : ('a -> 'b -> 'c) t =
    fun g ->
      HA.liftA2 (fun f g x y -> f (g x y))
                (c1 (G.fmap (fun f x y -> f (x, y)) g))
                (c2 (G.fmap (fun f x y -> f (y, x)) g))

  let apply (c1 : ('b -> 'c) t) (c2 : 'b t) : 'c t =
    fun g ->
      HA.apply (c1 (G.fmap (fun f x -> f x) g))
               (c2 (G.fmap (fun x f -> f x) g))

  let liftA2 = liftF2

  let liftA = apply

  let pure a : 'a t = fun g -> H.fmap (fun f -> f a) (G.fmap (fun x _ -> x) g)
end

module Adjoint (F : Functor.FUNCTOR) (U : Functor.FUNCTOR) (Adj : Adjunction.ADJUNCTION with module F = F and module U = U) = struct
  module LA = LeftAdjoint(F)(Identity)

  let adjointToLeftAdjoint : U.t 'a -> 'a LA.t = fun ua ->
    LA.LeftAdjoint (fun g -> Identity.Functor (Adj.right_adjunct (U.fmap (fun f -> f ()) ua) g))

  let leftAdjointToAdjoint : 'a LA.t -> U.t 'a = fun c ->
    Adj.left_adjunct (fun g -> LA.LeftAdjoint c (F.fmap (fun f x -> f (x ())) g)) ()
end