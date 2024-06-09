module Rift = struct
  type ('g, 'h, 'a) t = Rift of (('b -> 'a * 'c) -> 'g 'c -> 'h 'b)

  let contramap (f : 'b -> 'a) (Rift m : ('g, 'h, 'a) t) : ('g, 'h, 'b) t =
    Rift (fun (bac : 'b1 -> 'b * 'c) (gc : 'g 'c) ->
      m (fun (b : 'b1) -> let (a, c) = bac b in (f a, c)) gc)

  let divide (f : 'a -> 'b) (Rift g : ('g, 'g, 'a) t) (Rift h : ('g, 'g, 'b) t) : ('g, 'g, 'c) t =
    Rift (fun (bac : 'b1 -> 'c * 'd) (gc : 'g 'd) ->
      g (fun (a : 'b1) -> let (c, d) = bac a in (f c, d)) gc)
end