type 'a arena = {
  pos : 'a;
  dis : 'a -> 'b;
}

type ('dom, 'cod) lens = {
  observe : 'dom -> 'cod;
  interpret : 'dom -> ('cod -> 'cod) -> ('dom -> 'dom);
}

let id_lens (a : 'a arena) : ('a arena, 'a arena) lens =
  {observe = (fun x -> x); interpret = (fun _ f x -> f x)}

let ( <.> ) (lens23 : ('a2 arena, 'a3 arena) lens) (lens12 : ('a1 arena, 'a2 arena) lens) : ('a1 arena, 'a3 arena) lens =
  let obs p = lens23.observe (lens12.observe p) in
  let int p f = lens12.interpret p (fun x -> lens23.interpret (lens12.observe p) (f (lens23.observe x))) in
  {observe = obs; interpret = int}

type 'a display = 'a * ('a -> 'b)
type 'a as_functor = 'a * (('a -> 'b) -> 'c)

module Lens = struct
  type ('a, 'b) t = Lens of ('a -> 'b * ('b -> 'a))

  let duoidal a1 a2 b1 b2 =
    let x = (Arena.op_@@_ a1 a2, Arena.op_*_ (Arena.op_@@_ b1 b2)) in
    let y = (Arena.op_*_ (a1, b1), Arena.op_@@_ (Arena.op_*_ (a2, b2))) in
    let o ((p1, p2), (q1, q2)) =
      let pp = ((p1, q1), fun d -> (Arena.get_fst d p2, Arena.get_snd d q2)) in
      pp
    in
    let i ((p1, p2), (q1, q2)) ((de1, de2) : Arena.dis y (o ((p1, p2), (q1, q2)))) =
      ((Arena.op_**_ (Arena.get_fst de1, Arena.get_fst de2)),
       (Arena.op_**_ (Arena.get_snd de1, Arena.get_snd de2)))
    in
    Lens (o, i)
end