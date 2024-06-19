module Metasyntax

import Data.String

%default total

data HoleType = Everything
              | Expression
              | Alphanum
              | NonSpace
              | Line
              | Blank

data Delimiter = Delimited (Maybe String) (Maybe String)
               | ReservedIdentifiers (List String)

data Hole : Type where
  MkHole : HoleType -> Delimiter -> Hole

data Regex : Type where
  MkRegex : String -> Char -> String -> Regex

Syntax : Type
Syntax = List (Either Hole Regex)

record Alias where
  constructor MkAlias
  pattern : String
  matchTemplate : String
  rule : Maybe String

record Metasyntax where
  constructor MkMetasyntax
  syntax : Syntax
  identifier : String
  aliases : List Alias

defaultSyntax : Syntax
defaultSyntax = [Right (MkRegex ":[" '~' "]"),
                 Left (MkHole Everything (Delimited (Just ":[") (Just "]"))),
                 Left (MkHole Expression (Delimited (Just ":[") (Just ":e]"))),
                 Left (MkHole Alphanum (Delimited (Just ":[[") (Just "]]"))),
                 Left (MkHole NonSpace (Delimited (Just ":[") (Just ".]"))),
                 Left (MkHole Line (Delimited (Just ":[") (Just "\\n]"))),
                 Left (MkHole Blank (Delimited (Just ":[ ") (Just "]"))),
                 Left (MkHole Expression (ReservedIdentifiers ["α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι", "κ", "λ", "μ", "ξ", "π", "ρ", "ς", "σ", "τ", "υ", "φ", "χ", "ψ", "ω"])),
                 Left (MkHole Everything (ReservedIdentifiers ["Γ", "Δ", "Θ", "Λ", "Ξ", "Π", "Σ", "Φ", "Ψ", "Ω"]))]

defaultIdentifier : String
defaultIdentifier = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"

defaultAliases : List Alias
defaultAliases = [MkAlias "..." ":[_]" Nothing]

defaultMetasyntax : Metasyntax
defaultMetasyntax = MkMetasyntax defaultSyntax defaultIdentifier defaultAliases

create : Syntax -> String -> List Alias -> Metasyntax
create syntax identifier aliases = MkMetasyntax syntax identifier aliases

default : Metasyntax
default = create defaultSyntax defaultIdentifier defaultAliases