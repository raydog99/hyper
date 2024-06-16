module Day = struct
  type ('f, 'g, 'a) t = forall 'b 'c. 'f 'b * 'g 'c * ('b -> 'c -> 'a) * ('a -> ('b * 'c))

  let day (fa : 'a 'f) (gb : 'b 'g) : ('a, 'b, 'a * 'b) t =
    (fa, gb, (fun a b -> (a, b)), (fun (a, b) -> (a, b)))

  let invmap (f : 'a -> 'b) (g : 'c -> 'd) (Day (fb, gc, bca, abc)) : ('f, 'g, 'b) t =
    Day (fb, gc, (fun b c -> f (bca b c)), (fun a -> let (b, c) = abc (g a) in (b, c)))

  let assoc (Day (fb, Day (gd, he, dec, cde), bca, abc)) : (('f, 'g, 'c) t, 'h, 'a) t =
    let f a =
      let (b, c) = abc a in
      let (d, e) = cde c in
      ((b, d), e)
    and g (b, d) e = bca b (dec d e) in
    Day (Day (fb, gd, (fun (b, d) -> b), (fun c -> c)), he, g, f)

  let disassoc (Day (Day (fb, gc, deb, bde), hd, bca, abc)) : ('f, ('g, 'h, 'a) t, 'a) t =
    let f e (d, c) = bca (deb e d) c
    and g a =
      let (b, c) = abc a in
      let (d, e) = bde b in
      (d, (e, c)) in
    Day (fb, Day (gc, hd, (fun (c, e) -> c), (fun a -> a)), f, g)

  let swapped (Day (fb, gc, bca, abc)) : ('g, 'f, 'a) t =
    Day (gc, fb, (fun c b -> bca b c), (fun a -> let (b, c) = abc a in (c, b)))

  let intro1 (fa : 'a 'f) : (unit, 'f, 'a) t = Day (()), fa, (fun () a -> a), (fun a -> ((), a)))
  let intro2 (fa : 'a 'f) : ('f, unit, 'a) t = Day (fa, (), (fun a () -> a), (fun a -> (a, ())))
  let elim1 (Day (()), fc, bca, abc)) : 'c 'f = invmap (bca ()) (snd <@ abc) fc
  let elim2 (Day (fb, (), bca, abc)) : 'b 'f = invmap (flip bca ()) (fst <@ abc) fb

  let trans1 (fg : 'a 'f -> 'b 'g) (Day (fb, hc, bca, abc)) : ('g, 'h, 'a) t =
    Day (fg fb, hc, bca, abc)

  let trans2 (gh : 'a 'g -> 'b 'h) (Day (fb, gc, bca, abc)) : ('f, 'h, 'a) t =
    Day (fb, gh gc, bca, abc)

  let toContravariant (Day (fb, gc, _, abc)) = Contravariant.Day (fb, gc, abc)
  let toCovariant (Day (fb, gc, bca, _)) = Covariant.Day (fb, gc, bca)
end