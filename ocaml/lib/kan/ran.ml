type 'a ran g h = forall b. (b -> g a) -> h b -> 'a ran g h

let fmap f (ran g h a) = ran (\k -> f . g k) h a

let (<.>*) (ran1: 'a ran g h) (ran2: 'b ran g h) =
  ran (\k -> k (fmap fst k) (fmap snd k)) ((,) <$> ran1 <.> ran2)

let pure a = ran (const (Identity.unit a)) (pure ())
let (<*>) (ran_f: ('a -> 'b) ran g h) (ran_x: 'a ran g h) =
  ran (\k -> k (fmap fst k) (fmap snd k)) (liftA2 (fun f x -> f x) ran_f ran_x)

let to_ran (f: forall a. h a -> t a) (ran: 'a ran g h) = fmap f ran

let from_ran (s: forall a. 'a ran g h -> t a) (h_val: h a) = s (gran h_val)

let adjoint_to_ran (adj: Adjunction.t t g) (g_val: g a) = ran (Adjunction.unit adj) (Identity.unit a)

let ran_to_adjoint (adj: Adjunction.t t g) (ran: 'a ran t Identity) = Adjunction.right_adjoint adj (Identity.run ran)

let ran_to_composed_adjoint (adj: Adjunction.t t g) (ran: 'a ran t h) =
  fmap (Adjunction.right_adjoint adj) ran

let composed_adjoint_to_ran (adj: Adjunction.t t g) (h_val: h (g a)) =
  ran (Adjunction.unit adj) h_val

let compose_ran (compose: Functor.Composition.t) (ran: 'a ran t (ran g h)) =
  ran (fmap h . compose . decompose) g a

let decompose_ran (compose: Functor.Composition.t) (ran: 'a ran (compose t g) h) =
  ran (fmap compose . Identity.unit) (ran h)

let gran (h_val: h a) = ran Identity.id h_val