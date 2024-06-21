module TemplateMatch

import Data.String
import Data.List

data Rule = MkRule

record T where
  constructor MkT
  matchTemplate : String
  rule : Maybe Rule
  rewriteTemplate : Maybe String

create : Maybe String -> Maybe Rule -> String -> T
create rewriteTemplate rule matchTemplate = MkT matchTemplate rule rewriteTemplate

data Extracted = Regex String | ContiguousWhitespace String | NonSpace String

toRegexExtracted : Extracted -> String
toRegexExtracted (Regex s) = s
toRegexExtracted (ContiguousWhitespace _) = "\\s+"
toRegexExtracted (NonSpace s) = escape s

escape : String -> String
escape = pack . concatMap escapeChar . unpack
  where
    escapeChar : Char -> List Char
    escapeChar c = if c `elem` ['\\', '.', '|', '+', '*', '?', '(', ')', '[', ']', '{', '}', '^', '$']
                   then ['\\', c]
                   else [c]

extract : String -> List Extracted
extract _ = []

toRegex : T -> String
toRegex t = "(" ++ (concat $ map toRegexExtracted $ extract t.matchTemplate) ++ ")"