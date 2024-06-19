module Metasyntax
    ( defaultSyntax
    , defaultIdentifier
    , defaultAliases
    , defaultMetasyntax
    , create
    , Default
    ) where

import qualified Data.Map as Map

data Delimiter
    = Delimited (Maybe String) (Maybe String)
    | ReservedIdentifiers [String]
    deriving (Show, Eq)

data HoleType
    = Everything
    | Expression
    | Alphanum
    | NonSpace
    | Line
    | Blank
    deriving (Show, Eq)

data Hole = Hole HoleType Delimiter deriving (Show, Eq)

data Regex = Regex String Char String deriving (Show, Eq)

type Syntax = [Either Hole Regex]

defaultSyntax :: Syntax
defaultSyntax =
    [ Right $ Regex ":[" '~' "]"
    , Left $
      Hole
          Everything
          (Delimited (Just ":[") (Just "]"))
    , Left $
      Hole
          Expression
          (Delimited (Just ":[") (Just ":e]"))
    , Left $
      Hole
          Alphanum
          (Delimited (Just ":[[") (Just "]]"))
    , Left $
      Hole
          NonSpace
          (Delimited (Just ":[") (Just ".]"))
    , Left $
      Hole
          Line
          (Delimited (Just ":[") (Just "\\n]"))
    , Left $
      Hole
          Blank
          (Delimited (Just ":[ ") (Just "]"))
    , Left $
      Hole
          Expression
          (ReservedIdentifiers
             [ "α"
             , "β"
             , "γ"
             , "δ"
             , "ε"
             , "ζ"
             , "η"
             , "θ"
             , "ι"
             , "κ"
             , "λ"
             , "μ"
             , "ξ"
             , "π"
             , "ρ"
             , "ς"
             , "σ"
             , "τ"
             , "υ"
             , "φ"
             , "χ"
             , "ψ"
             , "ω"
             ])
    , Left $
      Hole
          Everything
          (ReservedIdentifiers
             ["Γ", "Δ", "Θ", "Λ", "Ξ", "Π", "Σ", "Φ", "Ψ", "Ω"])
    ]

defaultIdentifier :: String
defaultIdentifier = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_"

data Alias = Alias
    { pattern :: String
    , matchTemplate :: String
    , rule :: Maybe String
    }

defaultAliases :: [Alias]
defaultAliases = [Alias "..." ":[_]" Nothing]

defaultMetasyntax :: (Syntax, String, [Alias])
defaultMetasyntax = (defaultSyntax, defaultIdentifier, defaultAliases)

create :: (Syntax, String, [Alias]) -> (Syntax, String, [Alias])
create metasyntax = metasyntax

Default :: (Syntax, String, [Alias])
Default = create defaultMetasyntax