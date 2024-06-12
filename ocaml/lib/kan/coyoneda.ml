module type Contravariant = sig
  type 'a t
  val contramap : ('b -> 'a) -> 'a t -> 'b t
end

module Coyoneda (F : Contravariant) = struct
  type 'a t = {
    f : 'a -> 'b;
    m : 'b F.t;
  }

  let coyoneda f m = { f; m }

  let contramap f { f = g; m } = { f = f @@ g; m }
end