'a seq = 'a list
type 'a profile = 'a seq * 'a
type 'a operad = {
  colors : 'a list;
  operations : ('a profile * int) list;
  composition : ('a profile * int * 'a seq * int) -> int option;
  identity : 'a -> int;
}

let make_operad colors operations composition identity =
  {colors; operations; composition; identity}

let find_operation operad profile =
  List.assoc_opt profile operad.operations

let compose operad op_index operands =
  operad.composition (op_index, operands)

let identity_op operad color =
  operad.identity color