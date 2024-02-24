type token =
  | WhiteSpace
  | LineTerminator
  | Comment of string
  | CommonToken of string
  | DivPunctuator
  | RightBracePunctuator
  | RegularExpressionLiteral of string
  | TemplateSubstitutionTail
  | HashbangComment

let rec lexer input =
  let is_whitespace = function ' ' | '\t' -> true | _ -> false in
  let is_line_terminator = function '\n' | '\r' -> true | _ -> false in
  let is_digit = function '0' .. '9' -> true | _ -> false in

  let rec consume_whitespace input =
    match input with
    | c :: cs when is_whitespace c -> consume_whitespace cs
    | _ -> input
  in

  let rec consume_line_terminator input =
    match input with
    | '\n' :: cs -> cs
    | '\r' :: '\n' :: cs -> cs
    | _ -> input
  in

  let rec consume_comment input =
    let rec aux acc = function
      | '*' :: '/' :: cs -> (List.rev acc, cs)
      | c :: cs -> aux (c :: acc) cs
      | [] -> (List.rev acc, [])
    in
    match input with
    | '/' :: '*' :: cs ->
        let (comment, rest) = aux [] cs in
        (Comment (String.concat "" comment), rest)
    | _ -> (CommonToken "/", input)
  in

  let rec consume_common_token acc = function
    | c :: cs when is_whitespace c || is_line_terminator c || c = '/' || c = '}' ->
        (CommonToken (String.concat "" (List.rev acc)), c :: cs)
    | c :: cs -> consume_common_token (c :: acc) cs
    | [] -> (CommonToken (String.concat "" (List.rev acc)), [])
  in

  let consume_div_punctuator input =
    match input with
    | '/' :: cs -> (DivPunctuator, cs)
    | _ -> (CommonToken "/", input)
  in

  let consume_right_brace_punctuator input =
    match input with
    | '}' :: cs -> (RightBracePunctuator, cs)
    | _ -> (CommonToken "}", input)
  in

  let consume_regular_expression_literal input =
    let rec aux acc = function
      | '/' :: cs -> (List.rev acc, cs)
      | c :: cs -> aux (c :: acc) cs
      | [] -> (List.rev acc, [])
    in
    match input with
    | '/' :: cs ->
        let (literal, rest) = aux [] cs in
        (RegularExpressionLiteral (String.concat "" literal), rest)
    | _ -> (CommonToken "/", input)
  in

  match input |> consume_whitespace |> consume_line_terminator |> consume_comment with
  | (WhiteSpace, rest) -> lexer rest
  | (LineTerminator, rest) -> lexer rest
  | (Comment comment, rest) -> comment :: lexer rest
  | (CommonToken token, rest) -> token :: lexer rest
  | (DivPunctuator, rest) -> "/" :: lexer rest
  | (RightBracePunctuator, rest) -> "}" :: lexer rest
  | (RegularExpressionLiteral literal, rest) -> literal :: lexer rest
  | (TemplateSubstitutionTail, rest) -> "TemplateSubstitutionTail" :: lexer rest
  | (HashbangComment, rest) -> "HashbangComment" :: lexer rest
;;