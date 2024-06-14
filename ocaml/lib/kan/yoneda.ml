module Yoneda = struct
  type ('f, 'a) t = forall 'r. ('r -> 'a) -> 'f

  let runYoneda (m : ('f, 'a) t) : 'r -> 'a = m

  let liftYoneda (fa : 'f) : ('f, 'a) t =
    fun (ra : 'r -> 'a) -> Contravariant.contramap fa ra

  let lowerYoneda (m : ('f, 'a) t) : 'f =
    m (fun x -> x)

  let contramap (ab : 'r -> 's) (m : ('f, 'a) t) : ('f, 'b) t =
    fun (rb : 'r -> 'b) -> m (fun x -> rb (ab x))
end

module Representable
         (F : Functor.Contravariant)
         (Rep : sig
            type 'a t
            val tabulate : ('a -> Rep.t F.t) -> 'a t
            val index : 'a t -> 'a -> F.t
            val contramapWithRep : ('b -> 'a) -> 'a t -> 'b t
          end) =
struct
  include Yoneda

  let tabulate (fa : 'a -> Rep.t F.t) : ('f, 'a) t =
    liftYoneda (Rep.tabulate fa)

  let index (m : ('f, 'a) t) (a : 'a) : F.t =
    Rep.index (lowerYoneda m) a

  let contramapWithRep (beav : 'b -> 'a) (m : ('f, 'a) t) : ('f, 'b) t =
    liftYoneda (Rep.contramapWithRep beav (lowerYoneda m))
end

module Adjunction
         (F : Functor.Contravariant)
         (G : Functor.Contravariant)
         (Adj : sig
            val leftAdjunct : F.t -> G.t
            val rightAdjunct : G.t -> F.t
          end) =
struct
  include Yoneda

  let leftAdjunct (f : ('f, 'a) t) : ('g, 'a) t =
    liftYoneda (Adj.leftAdjunct (lowerYoneda f))

  let rightAdjunct (f : ('g, 'a) t) : ('f, 'a) t =
    liftYoneda (Adj.rightAdjunct (lowerYoneda f))
end
