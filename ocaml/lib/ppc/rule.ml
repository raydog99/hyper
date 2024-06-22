module Rule = struct
  type ast =
    | String of string
    | Template of template list
    | Equal of ast * ast
    | NotEqual of ast * ast
    | Rewrite of ast * (ast * ast)
    | Match of ast * (ast * ast list) list
    | True
    | False
    | Option of string

  and template = Template of string

  type rule = ast list

  type options = {
    nested: bool;
    strict: bool;
  }

  exception ParseError of string

  let rec parse_ast tokens =
    match tokens with
    | "true" :: rest -> (True, rest)
    | "false" :: rest -> (False, rest)
    | "option" :: name :: rest -> (Option name, rest)
    | "=" :: lhs :: "=" :: rhs :: rest ->
        let (lhs_ast, _) = parse_ast [lhs] in
        let (rhs_ast, _) = parse_ast [rhs] in
        (Equal (lhs_ast, rhs_ast), rest)
    | "!=" :: lhs :: rhs :: rest ->
        let (lhs_ast, _) = parse_ast [lhs] in
        let (rhs_ast, _) = parse_ast [rhs] in
        (NotEqual (lhs_ast, rhs_ast), rest)
    | "->" :: lhs :: "->" :: rhs :: rest ->
        let (lhs_ast, _) = parse_ast [lhs] in
        let (rhs_ast, _) = parse_ast [rhs] in
        (Rewrite (lhs_ast, (rhs_ast, rhs_ast)), rest)
    | token :: rest -> (String token, rest)
    | [] -> raise (ParseError "Unexpected end of input")

  let rec parse_rule tokens =
    match tokens with
    | [] -> []
    | "," :: rest -> parse_rule rest
    | _ ->
        let (ast, rest) = parse_ast tokens in
        ast :: parse_rule rest

  let create rule_text =
    let tokens = String.split_on_char ' ' rule_text |> List.filter ((<>) "") in
    match tokens with
    | "where" :: rest ->
        try
          Ok (parse_rule rest)
        with
        | ParseError msg -> Error ("Parse error: " ^ msg)
    | _ -> Error "Rule must start with 'where'"

  let options rule =
    List.fold_left
      (fun opts ast ->
         match ast with
         | Option "nested" -> { opts with nested = true }
         | Option "strict" -> { opts with strict = true }
         | _ -> opts)
      { nested = false; strict = false }
      rule

  let is_strict rule =
    (options rule).strict
end