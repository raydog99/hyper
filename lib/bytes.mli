type value_result =
  | Value of int * int
  | None

val get_attribute : int -> string -> value_result option
val process_byte : int -> string -> value_result option
val value : int -> string -> value_result option
val loop : int -> string -> value_result option