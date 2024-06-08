module Codensity = struct
  type 'a t = (('a -> 'b) -> 'b)

  let return x = fun k -> k x

  let bind m f = fun k -> m (fun x -> f x k)

  let map f m = fun k -> m (fun x -> k (f x))

  let lowerCodensity m = m (fun x -> x)

  let reset m = lowerCodensity m

  let shift f =
    let rec aux k' = f (fun x -> return x >>= k')
    in
    aux (fun x -> return x)
end