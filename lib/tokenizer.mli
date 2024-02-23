type token =
  | Doctype of string * string * string * bool
  | StartTag of string * (string * string) list * bool
  | EndTag of string
  | Comment of string
  | Character of char
  | EOF

type state =
  | Data
  | TagOpen
  | EndTagOpen
  | TagName
  | BeforeAttributeName
  | AttributeName
  | AfterAttributeName
  | BeforeAttributeValue
  | AttributeValueDoubleQuoted
  | AttributeValueSingleQuoted
  | AttributeValueUnquoted
  | CharacterReference

type context = {
  mutable input: string;
  mutable current_char: char option;
  mutable pos: int;
  mutable state: state;
  mutable token_buffer: string;
  mutable result: token list;
}

val create_context : string -> context
val read_char : context -> char
val consume_char : context -> unit
val emit_token : context -> token -> unit
val tokenize : context -> unit
val data_state : context -> unit