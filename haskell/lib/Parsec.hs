import Control.Applicative (Alternative(..))

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (x, rest) <- p input
    Just (f x, rest)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (f, rest1) <- p1 input
    (x, rest2) <- p2 rest1
    Just (f x, rest2)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> case p1 input of
    Just res -> Just res
    Nothing -> p2 input

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \input -> case input of
  (x:xs) | p x -> Just (x, xs)
  _ -> Nothing

return :: a -> Parser a
return = pure