module Density = struct
  type 'a t = Density of (('b -> 'a) * 'b)

  let return a = Density (fun _ -> a, ())

  let bind (Density (f, x)) k =
    let (g, y) = k (f x) in
    Density (g, y)

  let map f (Density (g, x)) =
    Density (fun y -> f (g y), x)

  let extract (Density (f, x)) = f x

  let duplicate (Density (f, x)) =
    Density (fun g -> Density (fun y -> g (f y), y), x)

  let extend f (Density (g, x)) =
    Density (fun y -> f (Density (g, y)), x)
end