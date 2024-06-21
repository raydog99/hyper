module TemplateMatch
  ( T(..)
  , create
  , toRegex
  ) where

import Data.Maybe (catMaybes)
import Data.List (intercalate)
import Text.ParserCombinators.Parsec
import qualified Text.Regex.PCRE as Regex

data T = T
  { matchTemplate :: String
  , rule :: Maybe Rule
  , rewriteTemplate :: Maybe String
  } deriving (Show, Read)

data Rule = Rule

create :: Maybe String -> Maybe Rule -> String -> T
create rewriteTemplate rule matchTemplate = T { matchTemplate, rule, rewriteTemplate }

identifierParser :: Parser String
identifierParser = many1 (alphaNum <|> char '_')

singleHoleParser :: Parser (Maybe String)
singleHoleParser = do
  string ":[["
  _ <- identifierParser
  string "]]"
  return $ Just "(\\w+)"

everythingHoleParser :: Parser (Maybe String)
everythingHoleParser = do
  string ":["
  _ <- identifierParser
  char ']'
  return $ Just "(\\n|.)*?"

expressionHoleParser :: Parser (Maybe String)
expressionHoleParser = do
  string ":["
  _ <- identifierParser
  string ":e]"
  return $ Just "(\\n|.)*?"

nonSpaceHoleParser :: Parser (Maybe String)
nonSpaceHoleParser = do
  string ":["
  _ <- identifierParser
  string ".]"
  return $ Just "([^ \\t\\s\\r\\n])+"

lineHoleParser :: Parser (Maybe String)
lineHoleParser = do
  string ":["
  _ <- identifierParser
  string "\\n]"
  return $ Just "(\\n|.)*?"

blankHoleParser :: Parser (Maybe String)
blankHoleParser = do
  string ":["
  _ <- many1 space
  _ <- identifierParser
  char ']'
  return $ Just "(\\ |\\t|\\s|\\r|\\n)+"

regexBodyParser :: Parser String
regexBodyParser = concat <$> many1 (charClassParser <|> escapedCharParser <|> regularCharParser)
  where
    charClassParser = between (char '[') (char ']') (many1 $ noneOf "]")
    escapedCharParser = (\c -> ['\\', c]) <$> (char '\\' *> anyChar)
    regularCharParser = (:[]) <$> noneOf "[]"

regexHoleParser :: Parser (Maybe String)
regexHoleParser = do
  string ":["
  _ <- identifierParser
  char '~'
  regex <- regexBodyParser
  char ']'
  return $ Just regex

data Extracted
  = Regex String
  | ContiguousWhitespace String
  | NonSpace String

extract :: Parser [Extracted]
extract = catMaybes <$> many (choice [holeParser, whitespaceParser, nonSpaceParser])
  where
    holeParser = fmap Regex <$> choice
      [ singleHoleParser
      , everythingHoleParser
      , expressionHoleParser
      , nonSpaceHoleParser
      , lineHoleParser
      , blankHoleParser
      , regexHoleParser
      ]
    whitespaceParser = fmap ContiguousWhitespace <$> (Just <$> many1 space)
    nonSpaceParser = fmap NonSpace <$> (Just <$> many1 (noneOf " \t\n\r"))

escape :: String -> String
escape = concatMap escapeChar
  where
    escapeChar c
      | c `elem` "\\.|+*?()[]{}^$" = ['\\', c]
      | otherwise = [c]

toRegex :: T -> String
toRegex T{matchTemplate} =
  case parse extract "" matchTemplate of
    Left err -> error $ "Parse error: " ++ show err
    Right extracted ->
      "(" ++ concatMap extractedToRegex extracted ++ ")"
  where
    extractedToRegex (Regex s) = s
    extractedToRegex (NonSpace s) = escape s
    extractedToRegex (ContiguousWhitespace _) = "\\s+"