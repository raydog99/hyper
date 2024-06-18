module Match where

import Control.Applicative (Alternative ((<|>), empty), many, optional, some)
import Control.Monad (guard)
import Data.Char (isAlphaNum)
import Data.Functor (($>))
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Text.Parsec (Parsec, char, choice, many1, noneOf, optionMaybe, string, try, (<?>))
import qualified Text.Parsec as P

data Hole
  = Hole
      { pattern :: String,
        variable :: String,
        offset :: Integer,
        kind :: String
      }
  | Constant String
  deriving (Eq, Show, Generic)

-- Parser for identifiers
identifier :: Parsec String () String
identifier = (:) <$> p_identifier_first <*> many p_identifier_char
  where
    p_identifier_first = identStart
    p_identifier_char = identLetter <|> digit <|> charSingleQuote
    identStart = letter <|> charSingleQuote <|> charUnderscore
    identLetter = letter <|> charUnderscore
    charUnderscore = char '_'
    charSingleQuote = char '\''
    digit = noneOf ""

-- Parses the hole pattern with optional delimiters
holePattern :: String -> String -> Parsec String () (String, String)
holePattern left right = do
  l <- optional $ string left
  v <- identifier
  r <- optional $ string right
  return (l ++ v ++ r, v)

-- Parses the regex pattern with a separator
regexPattern :: String -> Char -> String -> Parsec String () (String, String)
regexPattern left sep right = do
  string left
  (v, expr) <- regexBody sep right
  string right
  return (v ++ [sep] ++ expr, v)

-- Parses the regex body
regexBody :: Char -> String -> Parsec String () (String, String)
regexBody sep suffix = do
  v <- identifier
  _ <- char sep
  expr <- regexExpression suffix
  return (v, expr)

-- Parses the regex expression
regexExpression :: String -> Parsec String () String
regexExpression suffix =
  concat <$> many1 (regexTerm suffix)

-- Parses a term in the regex expression
regexTerm :: String -> Parsec String () String
regexTerm suffix =
  charClass
    <|> escapedChar
    <|> ((:) <$> noneOf (suffix ++ "[]\\") <*> pure "") -- TODO: Fix this line
    <?> "regex term"
  where
    charClass = ("[" ++) <$> (char '[' *> regexExpression (']' : suffix))
    escapedChar = ('\\' :) <$> (char '\\' *> anyChar)

-- Parses an attribute access
attributeAccess :: Parsec String () String
attributeAccess =
  char '.'
    *> choice
      [ string "value",
        string "length",
        string "lines",
        string "offset.start",
        string "offset.end",
        string "offset",
        string "line.start",
        string "line.end",
        string "line",
        string "column.start",
        string "column.end",
        string "column",
        string "file.path",
        string "file.name",
        string "file.directory",
        string "file",
        string "lowercase",
        string "UPPERCASE",
        string "Capitalize",
        string "uncapitalize",
        string "UpperCamelCase",
        string "lowerCamelCase",
        string "UPPER_SNAKE_CASE",
        string "lower_snake_case",
        string "lsif.hover"
      ]

-- Parses a hole or constant
parseTemplate :: Parsec String () [Hole]
parseTemplate =
  catMaybes
    <$> many
      ( try (parseHole <?> "hole")
          <|> parseConstant
      )
  where
    parseHole = do
      offset <- P.getOffset
      (pattern, variable) <- choice holePatterns
      kind <- optional attributeAccess
      return $ Just $ Hole pattern variable (fromIntegral offset) (fromMaybe "value" kind)
    parseConstant =
      (Constant <$> many1 (noneOf ['$'])) `map` Just
    holePatterns =
      [ holePattern "" "" <?> "hole without delimiters",
        holePattern "[" "]" <?> "hole with square brackets",
        holePattern "{" "}" <?> "hole with curly braces",
        holePattern "(" ")" <?> "hole with parentheses",
        holePattern "<" ">" <?> "hole with angle brackets",
        regexPattern "$" '~' "$" <?> "regex hole"
      ]

-- Parses the given string into a template
parseTemplate' :: String -> Either ParseError [Hole]
parseTemplate' = P.parse parseTemplate ""