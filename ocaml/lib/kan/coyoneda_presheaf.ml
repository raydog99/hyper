module Coyoneda = struct
  type ('f, 'a) t = Coyoneda of ('a -> 'b) * 'f 'b

  let contramap f (Coyoneda (g, m)) =
    Coyoneda ((fun x -> g (f x)), m)

  let lift f =
    Coyoneda (Fun.id, f)

  let lower (Coyoneda (f, m)) =
    Contravariant.contramap f m

  let hoist f (Coyoneda (g, x)) =
    Coyoneda (g, f x)
end

module Presheaf = struct
  type 'a t = 'a -> int

  let contramap f g x =
    g (f x)

  let tabulate f =
    f
end