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

let create_context input =
  {
    input;
    current_char = Some (String.get input 0);
    pos = 0;
    state = Data;
    token_buffer = "";
    result = [];
  }

let read_char ctx =
  match ctx.current_char with
  | Some c -> c
  | None -> failwith "Unexpected end of input"

let consume_char ctx =
  let len = String.length ctx.input in
  ctx.pos <- ctx.pos + 1;
  ctx.current_char <- if ctx.pos < len then Some (String.get ctx.input ctx.pos) else None

let emit_token ctx tok =
  ctx.result <- ctx.result @ [tok]

let rec tokenize ctx =
  match ctx.state with
  | Data -> data_state ctx
  | TagOpen -> tag_open_state ctx
  | EndTagOpen -> end_tag_open_state ctx
  | TagName -> tag_name_state ctx
  | BeforeAttributeName -> before_attribute_name_state ctx
  | AttributeName -> attribute_name_state ctx
  | AfterAttributeName -> after_attribute_name_state ctx
  | BeforeAttributeValue -> before_attribute_value_state ctx
  | AttributeValueDoubleQuoted -> attribute_value_double_quoted_state ctx
  | AttributeValueSingleQuoted -> attribute_value_single_quoted_state ctx
  | AttributeValueUnquoted -> attribute_value_unquoted_state ctx
  | CharacterReference -> character_reference_state ctx

and data_state ctx =
  let c = read_char ctx in
  match c with
  | '&' ->
    ctx.state <- Data;
    consume_char ctx;
    tokenize ctx (* Switch to the character reference state *)
  | '<' ->
    ctx.state <- Data;
    consume_char ctx;
    tokenize ctx (* Switch to the tag open state *)
  | '\000' ->
    ctx.state <- Data;
    consume_char ctx;
    emit_token ctx (Character '\xFFFD') (* Unexpected-null-character parse error *)
  | EOF -> process_eof ctx
  | _ ->
    consume_char ctx;
    ctx.token_buffer <- ctx.token_buffer ^ (String.make 1 c);
    tokenize ctx (* Emit the current input character as a character token *)

and rcdata_state ctx =
  let c = read_char ctx in
  match c with
  | '&' ->
    ctx.state <- RCDATA;
    consume_char ctx;
    tokenize ctx (* Switch to the character reference state *)
  | '<' ->
    ctx.state <- RCDATA;
    consume_char ctx;
    tokenize ctx (* Switch to the RCDATA less-than sign state *)
  | '\000' ->
    ctx.state <- RCDATA;
    consume_char ctx;
    emit_token ctx (Character '\xFFFD') (* Unexpected-null-character parse error *)
  | EOF -> process_eof ctx
  | _ ->
    consume_char ctx;
    ctx.token_buffer <- ctx.token_buffer ^ (String.make 1 c);
    tokenize ctx (* Emit the current input character as a character token *)

and rawtext_state ctx =
  let c = read_char ctx in
  match c with
  | '<' ->
    ctx.state <- RAWTEXT;
    consume_char ctx;
    tokenize ctx (* Switch to the RAWTEXT less-than sign state *)
  | '\000' ->
    ctx.state <- RAWTEXT;
    consume_char ctx;
    emit_token ctx (Character '\xFFFD') (* Unexpected-null-character parse error *)
  | EOF -> process_eof ctx
  | _ ->
    consume_char ctx;
    ctx.token_buffer <- ctx.token_buffer ^ (String.make 1 c);
    tokenize ctx (* Emit the current input character as a character token *)

and scriptdata_state ctx =
  let c = read_char ctx in
  match c with
  | '<' ->
    ctx.state <- ScriptData;
    consume_char ctx;
    tokenize ctx (* Switch to the script data less-than sign state *)
  | '\000' ->
    ctx.state <- ScriptData;
    consume_char ctx;
    emit_token ctx (Character '\xFFFD') (* Unexpected-null-character parse error *)
  | EOF -> process_eof ctx
  | _ ->
    consume_char ctx;
    ctx.token_buffer <- ctx.token_buffer ^ (String.make 1 c);
    tokenize ctx (* Emit the current input character as a character token *)