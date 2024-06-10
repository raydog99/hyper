module Lift = struct
  type ('g, 'h, 'a) t = ('a -> 'b) -> 'g 'a -> 'h 'b

  let dimap (f : 'b -> 'a) (g : 'a -> 'c) (m : ('g, 'h, 'a) t) : ('g, 'h, 'c) t =
    fun bac r ->
      m (fun a -> bac (g a)) (f r)

  let divide (f : 'a -> 'b) (g : ('g, 'h, 'a) t) (h : ('g, 'h, 'b) t) : ('g, 'h, 'c) t =
    fun bac r ->
      h (fun b -> bac b) (g (fun a -> bac (f a)) r)
end