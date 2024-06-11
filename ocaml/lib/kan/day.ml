type ('f, 'g) day = forall 'b 'c. 'f 'b * 'g 'c * ('b -> 'c -> 'a)

let day (f : 'a -> 'b) (g : 'c -> 'd) : ('a -> 'c, 'b, 'd) day =
  (f, g, fun a c -> (a, c))

let fmap (f : 'a -> 'b) (day : ('f, 'g) day) : ('f, 'g) day =
  let (fb, gc, bca) = day in
  (fb, gc, fun b c -> f (bca b c))

let pure (x : 'a) : ('f, 'g) day =
  ((), (), fun _ _ -> x)

let ap (day1 : ('f, 'g) day) (day2 : ('f, 'g) day) : ('f, 'g) day =
  let (fa, fb, u) = day1 in
  let (gc, gd, v) = day2 in
  ((fa, gc), (fb, gd), fun (a, c) (b, d) -> u a b (v c d))

let assoc (day : ('f, ('g, 'h) day) day) : (('f, 'g), 'h) day =
  let (fb, (gd, he, dec), bca) = day in
  ((fb, gd), he, fun (b, d) e -> bca b (dec d e))

let disassoc (day : (('f, 'g), 'h) day) : ('f, ('g, 'h) day) =
  let ((fb, gc), hd, eda) = day in
  (fb, (gc, hd, fun c d -> eda (fb, c) d))

let swapped (day : ('f, 'g) day) : ('g, 'f) day =
  let (fb, gc, abc) = day in
  (gc, fb, fun c b -> abc b c)

let intro1 (f : 'a -> 'b) : ('unit, 'a -> 'b) day =
  ((), f, fun _ a -> a)

let intro2 (f : 'a -> 'b) : ('a -> 'b, 'unit) day =
  (f, (), fun a _ -> a)

let elim1 (day : ('unit, 'f) day) : 'f =
  let ((), fc, bca) = day in
  bca () fc

let elim2 (day : ('f, 'unit) day) : 'f =
  let (fb, (), bca) = day in
  fb

let trans1 (fg : 'a -> 'b) (day : ('a, 'g) day) : ('b, 'g) day =
  let (fb, hc, bca) = day in
  (fg fb, hc, bca)

let trans2 (gh : 'a -> 'b) (day : ('f, 'a) day) : ('f, 'b) day =
  let (fb, gc, bca) = day in
  (fb, gh gc, bca)