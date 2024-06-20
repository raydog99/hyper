module Template

import Data.String
import Data.Char

%default total

public export
data AttributeKind =
  Value |
  Length |
  Lines |
  OffsetStart |
  OffsetEnd |
  LineStart |
  LineEnd |
  ColumnStart |
  ColumnEnd |
  FilePath |
  FileName |
  FileDirectory |
  Lowercase |
  Uppercase |
  Capitalize |
  Uncapitalize |
  UpperCamelCase |
  LowerCamelCase |
  UpperSnakeCase |
  LowerSnakeCase |
  External String

public export
data TemplateElement =
  Constant String |
  Hole Pattern Variable Offset AttributeKind

public export
Variable : Type
Variable = String

public export
Pattern : Type
Pattern = String

public export
Offset : Type
Offset = Int

public export
attributeToKind : String -> AttributeKind
attributeToKind "value" = Value
attributeToKind "length" = Length
attributeToKind "lines" = Lines
attributeToKind "offset" = OffsetStart
attributeToKind "offset.start" = OffsetStart
attributeToKind "offset.end" = OffsetEnd
attributeToKind "line" = LineStart
attributeToKind "line.start" = LineStart
attributeToKind "line.end" = LineEnd
attributeToKind "column" = ColumnStart
attributeToKind "column.start" = ColumnStart
attributeToKind "column.end" = ColumnEnd
attributeToKind "file" = FilePath
attributeToKind "file.path" = FilePath
attributeToKind "file.name" = FileName
attributeToKind "file.directory" = FileDirectory
attributeToKind "lowercase" = Lowercase
attributeToKind "UPPERCASE" = Uppercase
attributeToKind "Capitalize" = Capitalize
attributeToKind "uncapitalize" = Uncapitalize
attributeToKind "UpperCamelCase" = UpperCamelCase
attributeToKind "lowerCamelCase" = LowerCamelCase
attributeToKind "UPPER_SNAKE_CASE" = UpperSnakeCase
attributeToKind "lower_snake_case" = LowerSnakeCase
attributeToKind "lsif.hover" = External "lsif.hover"
attributeToKind s = error ("invalid attribute " ++ s)

public export
camelToSnake : String -> String
camelToSnake = pack . concatMap go . unpack
  where
    go : Char -> List Char
    go c =
      if isUpper c
        then ['_', toUpper c]
        else [toUpper c]

public export
substituteKind : Maybe String -> TemplateElement -> List (Variable, String) -> Maybe String
substituteKind _ (Constant _) _ = Nothing
substituteKind filepath (Hole pattern variable offset kind) env =
  case kind of
    Value => lookup variable env
    Length => map pack . map length . lookup variable $ env
    Lines => map (\s => show (length (filter (== '\n') (unpack s)) + 1)) . lookup variable $ env
    OffsetStart => matchStart variable env
    OffsetEnd => matchEnd variable env
    LineStart => lineStart filepath variable env
    LineEnd => lineEnd filepath variable env
    ColumnStart => columnStart filepath variable env
    ColumnEnd => columnEnd filepath variable env
    FilePath => filepath
    FileName => map (pack . lastName) filepath
    FileDirectory => map (pack . init . dropLast . unpack) filepath
    Lowercase => map toLower . lookup variable $ env
    Uppercase => map toUpper . lookup variable $ env
    Capitalize => map capitalize . lookup variable $ env
    Uncapitalize => map uncapitalize . lookup variable $ env
    UpperCamelCase => map (camelToUpper . camelToSnake) . lookup variable $ env
    LowerCamelCase => map (uncapitalize . camelToUpper . camelToSnake) . lookup variable $ env
    UpperSnakeCase => map (map toUpper . camelToSnake) . lookup variable $ env
    LowerSnakeCase => map camelToSnake . lookup variable $ env
    External "lsif.hover" => do
      filepath' <- filepath
      value <- lookup variable env
      offset <- matchStart variable env
      let source = ""  -- Placeholder implementation for readFile
          (line, column) = offsetToLineColumn source offset
      pure $ "lsif.hover for (" ++ show line ++ ", " ++ show column ++ "): " ++ value
    External _ => error "Unsupported external attribute"
  where
    lookup : Variable -> List (Variable, String) -> Maybe String
    lookup var [] = Nothing
    lookup var ((var', val) :: xs) =
      if var == var'
        then Just val
        else lookup var xs

    matchStart : Variable -> List (Variable, String) -> Maybe String
    matchStart var env = map (const "0") (lookup var env)  -- Placeholder implementation

    matchEnd : Variable -> List (Variable, String) -> Maybe String
    matchEnd var env = map (show . length) . lookup var $ env  -- Placeholder implementation

    lineStart : Maybe String -> Variable -> List (Variable, String) -> Maybe String
    lineStart _ var env = matchStart var env  -- Placeholder implementation

    lineEnd : Maybe String -> Variable -> List (Variable, String) -> Maybe String
    lineEnd _ var env = matchEnd var env  -- Placeholder implementation

    columnStart : Maybe String -> Variable -> List (Variable, String) -> Maybe String
    columnStart _ var env = matchStart var env  -- Placeholder implementation

    columnEnd : Maybe String -> Variable -> List (Variable, String) -> Maybe String
    columnEnd _ var env = matchEnd var env  -- Placeholder implementation

    offsetToLineColumn : String -> Offset -> (Int, Int)
    offsetToLineColumn _ _ = (1, 1)  -- Placeholder implementation

    lastName : String -> String
    lastName = foldl (\x, y => if isNL x then "" else y) ""

    capitalize : String -> String
    capitalize "" = ""
    capitalize (x :: xs) = pack (toUpper x :: map toLower (unpack xs))

    uncapitalize : String -> String
    uncapitalize "" = ""
    uncapitalize (x :: xs) = pack (toLower x :: xs)

    camelToUpper : String -> String
    camelToUpper = pack . concatMap capitalize . split isNL . camelToSnake

    isNL : Char -> Bool
    isNL x = x == '_'

public export
Environment : Type
Environment = List (Variable, String)