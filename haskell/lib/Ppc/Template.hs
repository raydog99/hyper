module Template (
    parseTemplate,
    variables,
    toString,
    substituteKind,
    attributeToKind
) where

import Control.Monad (guard)
import Data.Char (isAlphaNum, isUpper, isLower, toLower, toUpper)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import System.Environment (lookupEnv)
import Text.Regex.Posix ((=~))

type Variable = String
type Pattern = String
type Offset = Int
type Kind = AttributeKind

data AttributeKind
    = Value
    | Length
    | Lines
    | OffsetStart
    | OffsetEnd
    | LineStart
    | LineEnd
    | ColumnStart
    | ColumnEnd
    | FilePath
    | FileName
    | FileDirectory
    | Lowercase
    | Uppercase
    | Capitalize
    | Uncapitalize
    | UpperCamelCase
    | LowerCamelCase
    | UpperSnakeCase
    | LowerSnakeCase
    | External String
    deriving (Show, Eq)

data TemplateElement
    = Constant String
    | Hole { pattern :: Pattern, variable :: Variable, offset :: Offset, kind :: Kind }
    deriving (Show)

debug :: Bool
debug = case lookupEnv "DEBUG_COMBY" of
    Nothing -> False
    Just _  -> True

attributeToKind :: String -> AttributeKind
attributeToKind = \case
    "value"          -> Value
    "length"         -> Length
    "lines"          -> Lines
    "offset"         -> OffsetStart
    "offset.start"   -> OffsetStart
    "offset.end"     -> OffsetEnd
    "line"           -> LineStart
    "line.start"     -> LineStart
    "line.end"       -> LineEnd
    "column"         -> ColumnStart
    "column.start"   -> ColumnStart
    "column.end"     -> ColumnEnd
    "file"           -> FilePath
    "file.path"      -> FilePath
    "file.name"      -> FileName
    "file.directory" -> FileDirectory
    "lowercase"      -> Lowercase
    "UPPERCASE"      -> Uppercase
    "Capitalize"     -> Capitalize
    "uncapitalize"   -> Uncapitalize
    "UpperCamelCase" -> UpperCamelCase
    "lowerCamelCase" -> LowerCamelCase
    "UPPER_SNAKE_CASE" -> UpperSnakeCase
    "lower_snake_case" -> LowerSnakeCase
    "lsif.hover"     -> External "lsif.hover"
    s                -> error $ "invalid attribute " ++ show s

parseTemplate :: String -> [TemplateElement]
parseTemplate = undefined

variables :: [TemplateElement] -> [TemplateElement]
variables = mapMaybe $ \case
    Hole h -> Just h
    _      -> Nothing

toString :: [TemplateElement] -> String
toString = concatMap $ \case
    Constant c -> c
    Hole { pattern } -> pattern

camelToSnake :: String -> String
camelToSnake = intercalate "_" . go 0 . map (\case
    c | isUpper c -> ['_', toLower c]
    c             -> [c])
  where
    go _ []     = []
    go n (x:xs) | isUpper x = x : go (n + 1) xs
                | otherwise = toLower x : go (n + 1) xs

substituteKind :: Maybe FilePath -> TemplateElement -> Environment -> Maybe String
substituteKind filepath (Hole { variable, kind }) env = case kind of
    Value         -> lookup env variable
    Length        -> fmap show . fmap length $ lookup env variable
    Lines         -> fmap (show . (+ 1) . length . filter (== '\n')) $ lookup env variable
    OffsetStart   -> fmap show . matchStart env variable
    OffsetEnd     -> fmap show . matchEnd env variable
    LineStart     -> lineStart filepath env variable
    LineEnd       -> lineEnd filepath env variable
    ColumnStart   -> columnStart filepath env variable
    ColumnEnd     -> columnEnd filepath env variable
    FilePath      -> filepath
    FileName      -> fmap fileName filepath
    FileDirectory -> fmap directory filepath
    Lowercase     -> fmap map toLower $ lookup env variable
    Uppercase     -> fmap map toUpper $ lookup env variable
    Capitalize    -> fmap (let (h:t) = id in toUpper h : map toLower t) $ lookup env variable
    Uncapitalize  -> fmap (let (h:t) = id in toLower h : t) $ lookup env variable
    UpperCamelCase -> fmap (concatMap capitalize . words . camelToSnake) $ lookup env variable
    LowerCamelCase -> fmap (let (h:t) = concat . map capitalize . words . camelToSnake in toLower h : t) $ lookup env variable
    UpperSnakeCase -> fmap (map toUpper . camelToSnake) $ lookup env variable
    LowerSnakeCase -> fmap camelToSnake $ lookup env variable
    External "lsif.hover" -> do
        filepath' <- filepath
        value <- lookup env variable
        offset <- matchStart env variable
        let source = readFile filepath'
            (line, column) = offsetToLineColumn source offset
        pure $ "lsif.hover for " ++ show (line, column) ++ ": " ++ value
    External _ -> error "Unsupported external attribute"
  where
    lookup env var = flip lookup env <$> stripVar var
    matchStart env var = fmap fst . matchRange env <$> stripVar var
    matchEnd env var = fmap snd . matchRange env <$> stripVar var
    lineStart filepath env var = do
        filepath' <- filepath
        offset <- matchStart env var
        let source = readFile filepath'
            (line, _) = offsetToLineColumn source offset
        pure $ show line
    lineEnd filepath env var = do
        filepath' <- filepath
        offset <- matchEnd env var
        let source = readFile filepath'
            (line, _) = offsetToLineColumn source offset
        pure $ show line
    columnStart filepath env var = do
        filepath' <- filepath
        offset <- matchStart env var
        let source = readFile filepath'
            (_, column) = offsetToLineColumn source offset
        pure $ show column
    columnEnd filepath env var = do
        filepath' <- filepath
        offset <- matchEnd env var
        let source = readFile filepath'
            (_, column) = offsetToLineColumn source offset
        pure $ show column
    fileName = takeWhileEnd (/= '/') . takeWhileEnd (/= '\\')
    directory = dropWhileEnd (/= '/') . dropWhileEnd (/= '\\')
    offsetToLineColumn source offset = (1, 1) -- Placeholder implementation
    stripVar var = case var =~ "^\\[(.*?)\\]$" of
        Just [_, v] -> Just v
        _           -> Nothing

-- Placeholder implementation
readFile :: FilePath -> String
readFile _ = ""

matchRange :: Environment -> Variable -> (Int, Int)
matchRange _ _ = (0, 0)

type Environment = [(Variable, String)]