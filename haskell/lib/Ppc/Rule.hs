{-# LANGUAGE OverloadedStrings #-}

module Rule (
    create,
    Options(..),
    options,
    isStrict
) where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative ((<|>))
import Data.Char (isAlphaNum)
import Data.List (isPrefixOf)

-- Types

data Ast
    = String Text
    | Template [Template]
    | Equal Ast Ast
    | NotEqual Ast Ast
    | Rewrite Ast (Ast, Ast)
    | Match Ast [(Ast, [Ast])]
    | True
    | False
    | Option Text
    deriving (Show, Eq)

type Rule = [Ast]

newtype Template = Template Text deriving (Show, Eq)

data Options = Options
    { nested :: Bool
    , strict :: Bool
    } deriving (Show, Eq)

-- Parsing

newtype Parser a = Parser { runParser :: Text -> Either String (a, Text) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
        (x, rest) <- p input
        return (f x, rest)

instance Applicative Parser where
    pure x = Parser $ \input -> Right (x, input)
    (Parser p1) <*> (Parser p2) = Parser $ \input -> do
        (f, rest1) <- p1 input
        (x, rest2) <- p2 rest1
        return (f x, rest2)

instance Monad Parser where
    return = pure
    (Parser p) >>= f = Parser $ \input -> do
        (x, rest) <- p input
        runParser (f x) rest

instance Alternative Parser where
    empty = Parser $ const $ Left "empty"
    (Parser p1) <|> (Parser p2) = Parser $ \input ->
        case p1 input of
            Right result -> Right result
            Left _ -> p2 input

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser $ \input ->
    case T.uncons input of
        Just (c, rest) | pred c -> Right (c, rest)
        _ -> Left "satisfy: no match"

char :: Char -> Parser Char
char c = satisfy (== c)

string :: Text -> Parser Text
string s = Parser $ \input ->
    if s `T.isPrefixOf` input
        then Right (s, T.drop (T.length s) input)
        else Left $ "Expected " ++ show s

isWhitespace :: Char -> Bool
isWhitespace c = c `elem` [' ', '\t', '\r', '\n']

spaces :: Parser ()
spaces = void $ many (satisfy isWhitespace)

spaces1 :: Parser ()
spaces1 = void $ some (satisfy isWhitespace)

alphanum :: Parser Char
alphanum = satisfy isAlphaNum

toAtom :: Text -> Ast
toAtom s = case parseTemplate s of
    []            -> String ""
    [Template c]  -> String c
    ts            -> Template ts

parseTemplate :: Text -> [Template]
parseTemplate = map Template . T.words