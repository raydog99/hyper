type delimiter =
  | Delimited of (string option) * (string option)
  | ReservedIdentifiers of string list

type hole_type =
  | Everything
  | Expression
  | Alphanum
  | NonSpace
  | Line
  | Blank

type hole = Hole of hole_type * delimiter

type regex = Regex of string * char * string

type syntax = (hole, regex) Either.t list

let default_syntax =
  [ Right (Regex (":[", '~', "]"))
  ; Left (Hole (Everything, Delimited (Some ":[", Some "]")))
  ; Left (Hole (Expression, Delimited (Some ":[", Some ":e]")))
  ; Left (Hole (Alphanum, Delimited (Some ":[[", Some "]]")))
  ; Left (Hole (NonSpace, Delimited (Some ":[", Some ".]")))
  ; Left (Hole (Line, Delimited (Some ":[", Some "\\n]")))
  ; Left (Hole (Blank, Delimited (Some ":[ ", Some "]")))
  ; Left
      (Hole
         ( Expression
         , ReservedIdentifiers
             [ "α"
             ; "β"
             ; "γ"
             ; "δ"
             ; "ε"
             ; "ζ"
             ; "η"
             ; "θ"
             ; "ι"
             ; "κ"
             ; "λ"
             ; "μ"
             ; "ξ"
             ; "π"
             ; "ρ"
             ; "ς"
             ; "σ"
             ; "τ"
             ; "υ"
             ; "φ"
             ; "χ"
             ; "ψ"
             ; "ω" ] ))
  ; Left
      (Hole
         (Everything, ReservedIdentifiers ["Γ"; "Δ"; "Θ"; "Λ"; "Ξ"; "Π"; "Σ"; "Φ"; "Ψ"; "Ω"]))
  ]

let default_identifier = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"

type alias = {pattern: string; match_template: string; rule: string option}

let default_aliases = [{pattern = "..."; match_template = ":[_]"; rule = None}]

let default_metasyntax = (default_syntax, default_identifier, default_aliases)

let create metasyntax = metasyntax

let default = create default_metasyntax