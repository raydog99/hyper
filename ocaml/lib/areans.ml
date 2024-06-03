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