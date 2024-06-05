type lan g h a = (g b -> a) -> h b -> lan g h a

let fmap f (lan g h a) = lan (f . g) h a

let (<.>*) (lan1: lan g h a) (lan2: lan g h b) =
  lan (\k -> k (fmap fst k) (fmap snd k)) ((,) <$> lan1 <.> lan2)

let pure a = lan (const a) (pure ())
let (<*>) (lan_f: lan g h a) (lan_x: lan g h b) =
  lan (\k -> k (fmap fst k) (fmap snd k)) (liftA2 (,) lan_f lan_x)

let to_lan (f: forall a. h a -> t a) (lan: lan g h b) = fmap f (s lan)

let from_lan (s: forall a. lan g h a -> t a) (h_val: h a) = s (glan h_val)

let adjoint_to_lan (adj: Adjunction.t t g) (g_val: g a) = lan (Adjunction.counit adj) (Identity.unit a)

let lan_to_adjoint (adj: Adjunction.t t g) (lan: lan t Identity a) = Adjunction.left_adjoint adj (Identity.run lan)

let lan_to_composed_adjoint (adj: Adjunction.t t g) (lan: lan t h a) =
  fmap (Adjunction.left_adjoint adj) lan

let composed_adjoint_to_lan (adj: Adjunction.t t g) (h_val: h (g a)) =
  lan (Adjunction.counit adj) h_val

let compose_lan (compose: Functor.Composition.t) (lan: lan t (lan g h) a) =
  lan (fmap g . compose . decompose) h a

let decompose_lan (compose: Functor.Composition.t) (lan: lan (compose t g) h a) =
  lan (fmap compose . Identity.unit) (lan h)

let glan (h_val: h a) = lan Identity.id h_val