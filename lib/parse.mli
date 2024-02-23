open Bytes

type insertion_mode =
  | Initial
  | BeforeHtml
  | InHead
  | InBody

type active_formatting_element = {
  tag_name: string;
  attributes: (string * string) list;
  token: string;
}

val parse_html : Bytes.t -> unit

val parse_start_tag : Bytes.t -> int -> string * (string * string) list
val handle_html_element : (string * (string * string) list) -> insertion_mode list -> unit
val handle_in_body : (string * (string * string) list) -> unit
val handle_in_head : (string * (string * string) list) -> unit

val push_active_formatting_element : active_formatting_element -> unit
val reconstruct_active_formatting_elements : unit -> unit