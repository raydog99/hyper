module Match = struct
  open Parser

  type hole =
    | Hole of string * string * int * string
    | Constant of string

  let identifier =
    let identStart = word_sat (fun c -> Char.is_alpha c || c = '_' || c = '\'') in
    let identChar =
      word_sat
        (fun c -> Char.is_alpha c || Char.is_digit c || c = '_' || c = '\'')
    in
    identStart <*> (many identChar)

  let holePattern left right =
    let lp = option "" (string left) in
    let rp = option "" (string right) in
    let pat = lp <*> identifier <*> rp in
    pat >>| fun (l, v, r) -> (l ^ v ^ r, v)

  let regexPattern left sep right =
    string left
    >>= fun () ->
    let regexBody =
      identifier <*> (char sep <*> regexExpression (sep :: String.to_seq right))
    in
    regexBody >>= fun (v, expr) ->
    string right >>| fun () -> (v ^ String.make 1 sep ^ expr, v)

  and regexExpression suffix =
    let term = regexTerm (List.of_seq suffix) in
    many1 term >>| String.concat ""

  and regexTerm suffix =
    let charClass =
      char '['
      >>= fun () ->
      regexExpression (']' :: suffix) >>| fun expr -> "[" ^ expr
    in
    let escapedChar = char '\\' >>= fun () -> any_char >>| String.make 1 in
    charClass
    <|> escapedChar
    <|> (word_sat (fun c -> not (List.mem c suffix)) <|> return "")

  let attributeAccess =
    char '.'
    >>= fun () ->
    choice
      [
        "value";
        "length";
        "lines";
        "offset.start";
        "offset.end";
        "offset";
        "line.start";
        "line.end";
        "line";
        "column.start";
        "column.end";
        "column";
        "file.path";
        "file.name";
        "file.directory";
        "file";
        "lowercase";
        "UPPERCASE";
        "Capitalize";
        "uncapitalize";
        "UpperCamelCase";
        "lowerCamelCase";
        "UPPER_SNAKE_CASE";
        "lower_snake_case";
      ]

  let parseTemplate =
    let parseHole =
      get_offset
      >>= fun offset ->
      choice
        [
          holePattern "" "" >>| fun (pat, var) ->
            Hole (pat, var, offset, "value");
          holePattern "[" "]" >>| fun (pat, var) ->
            Hole (pat, var, offset, option "value" attributeAccess);
          holePattern "{" "}" >>| fun (pat, var) ->
            Hole (pat, var, offset, option "value" attributeAccess);
          holePattern "(" ")" >>| fun (pat, var) ->
            Hole (pat, var, offset, option "value" attributeAccess);
          holePattern "<" ">" >>| fun (pat, var) ->
            Hole (pat, var, offset, option "value" attributeAccess);
          regexPattern "$" '~' "$" >>| fun (pat, var) ->
            Hole (pat, var, offset, "value");
        ]
    in
    let parseConstant = word_sat (fun c -> c <> '$') >>| fun s -> Constant s in
    many (parseHole <|> parseConstant)

  let parseTemplate' input =
    let result = parse_string parseTemplate () input in
    match result with
    | Success (holes, _) -> Ok holes
    | Failed (error, _) -> Error error
end