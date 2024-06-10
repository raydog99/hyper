type ran g h a = h a -> (a -> g b) -> ran g h b

let fmap f (ran g h a) = ran (f . g) h a  

let (<.*>) (ran1: ran g h a) (ran2: ran g h b) =
  ran (\k -> k (fmap fst k) (fmap snd k)) ((,) <$> ran1 <.> ran2)

let pure a = ran (const a) (pure ())
let (<*>) (ran_f: ran g h (a -> b)) (ran_x: ran g h a) =
  ran (\k -> k (fmap fst k) (fmap snd k)) (liftA2 (flip id) ran_f ran_x)

let to_ran (f: forall a. h a -> t a) (ran: ran g h b) = fmap f (s ran)

let from_ran (s: forall a. ran g h a -> t a) (h_val: h a) = s (gran h_val)

let adjoint_to_ran (adj: Adjunction.t t g) (g_val: g a) = ran (Adjunction.unit adj) (Identity.unit a)

let ran_to_adjoint (adj: Adjunction.t t g) (ran: ran t Identity a) = Adjunction.right_adjoint adj (Identity.run ran)

let ran_to_composed_adjoint (adj: Adjunction.t t g) (ran: ran t h a) =
  fmap (Adjunction.right_adjoint adj) ran

let composed_adjoint_to_ran (adj: Adjunction.t t g) (h_val: h (g a)) =
  ran (Adjunction.unit adj) h_val

let compose_ran (compose: Functor.Composition.t) (ran: ran t (ran g h) a) =
  ran (fmap g . compose . decompose) h a

let decompose_ran (compose: Functor.Composition.t) (ran: ran (compose t g) h a) =
  ran (fmap compose . Identity.unit) (ran h)

let gran (h_val: h a) = ran Identity.id h_val