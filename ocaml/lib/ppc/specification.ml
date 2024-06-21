module TemplateMatch = struct
  type rule = Rule

  type t = {
    match_template: string;
    rule: rule option;
    rewrite_template: string option;
  }

  let create rewrite_template rule match_template =
    { match_template; rule; rewrite_template }

  type extracted =
    | Regex of string
    | ContiguousWhitespace of string
    | NonSpace of string

  let escape s =
    let escape_char c =
      if String.contains "\\.|+*?()[]{}^$" c then
        "\\" ^ (String.make 1 c)
      else
        String.make 1 c
    in
    String.concat "" (List.map escape_char (String.to_seq s |> List.of_seq))

  let to_regex t =
    let extracted = extract t.match_template in
    let extracted_to_regex = function
      | Regex s -> s
      | NonSpace s -> escape s
      | ContiguousWhitespace _ -> "\\s+"
    in
    "(" ^ (String.concat "" (List.map extracted_to_regex extracted)) ^ ")"
end