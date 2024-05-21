type 'a parser = 'a list -> ('a * 'a list)

let return value = fun input -> (value, input)

let satisfy predicate input =
  match input with
  | value :: rest when predicate value -> (value, rest)
  | _ -> raise (Failure "satisfy failed")