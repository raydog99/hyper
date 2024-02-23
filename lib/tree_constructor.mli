type node =
  | Element of string * (string * string) list * node list
  | Text of string
  | Comment of string
  | Document of node list

type insertion_mode =
  | Initial
  | BeforeHtml
  | BeforeHead
  | InHead

type tree_context = {
  mutable document: node list;
  mutable stack: node list;
  mutable insertion_mode: insertion_mode;
}

val create_tree_context : unit -> tree_context

val reconstruct_tree : tree_context -> token list -> unit

val initial_mode : tree_context -> token -> unit

val before_html_mode : tree_context -> token -> unit

val before_head_mode : tree_context -> token -> unit

val in_head_mode : tree_context -> token -> unit