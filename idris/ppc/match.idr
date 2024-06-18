module Match

import Data.String

%default total

public export
data Hole : Type where
  Hole : (pattern : String)
      -> (variable : String)
      -> (offset : Nat)
      -> (kind : String)
      -> Hole
  Constant : String -> Hole

export
Show Hole where
  show (Hole pattern variable offset kind) =
    "Hole { pattern = " ++ show pattern ++
    ", variable = " ++ show variable ++
    ", offset = " ++ show offset ++
    ", kind = " ++ show kind ++ " }"
  show (Constant value) =
    "Constant (" ++ show value ++ ")"

export
parseTemplate : String -> Either String (List Hole)
parseTemplate input =
  let parseHole : String -> Either String (Hole, String)
      parseHole str =
        case (parseHolePattern "" "" str,
              parseHolePattern "[" "]" str,
              parseHolePattern "{" "}" str,
              parseHolePattern "(" ")" str,
              parseHolePattern "<" ">" str,
              parseRegexHole str) of
          ((Right (hole, rest)), _, _, _, _, _) => Right (hole, rest)
          (_, (Right (hole, rest)), _, _, _, _) => Right (hole, rest)
          (_, _, (Right (hole, rest)), _, _, _) => Right (hole, rest)
          (_, _, _, (Right (hole, rest)), _, _) => Right (hole, rest)
          (_, _, _, _, (Right (hole, rest)), _) => Right (hole, rest)
          (_, _, _, _, _, (Right (hole, rest))) => Right (hole, rest)
          _ => Left "Failed to parse hole"

      parseConstant : String -> Either String (Hole, String)
      parseConstant str =
        case span (/= '$') str of
          (value, rest) =>
            if length value > 0
              then Right (Constant value, rest)
              else Left "Failed to parse constant"

      go : String -> (List Hole, Nat, String) -> Either String (List Hole)
      go input (holes, offset, rest) =
        case parseHole rest of
          Right (hole, newRest) =>
            let hole' = case hole of
                            Hole pattern variable _ kind =>
                              Hole pattern variable offset kind
                            _ => hole
            in go input (hole' :: holes, offset + length (showHole hole'), newRest)

          Left _ =>
            case parseConstant rest of
              Right (constant, newRest) =>
                let offset' = offset + length (show constant)
                in go input (constant :: holes, offset', newRest)

              Left err => Left err

      showHole : Hole -> String
      showHole (Hole pattern variable _ _) = pattern
      showHole (Constant value) = value

   in go input ([], 0, input) >>= \holes => Right (reverse holes)

parseHolePattern : String -> String -> String -> Either String (Hole, String)
parseHolePattern left right input =
  case span (/= '$') input of
    (prefix, '$' :: rest) =>
      case span (\c => isAlphaNum c || c == '_' || c == '\'') rest of
        (variable, suffix) =>
          case (strCons left variable, strCons (cast suffix) right) of
            (Just (lvar, rvar), "") =>
              Right (Hole lvar variable 0 "value",
                     strCrop (length lvar + length variable) input)
            (Just (lvar, rvar), restStr) =>
              if strLength restStr >= length rvar
                 then Right (Hole (lvar ++ variable ++ rvar) variable 0 "value",
                             strCrop (length (lvar ++ variable ++ rvar)) input)
                 else Left "Failed to parse hole pattern"
            _ => Left "Failed to parse hole pattern"
        _ => Left "Failed to parse hole pattern"
    _ => Left "Failed to parse hole pattern"

parseRegexHole : String -> Either String (Hole, String)
parseRegexHole input =
  case span (/= '$') input of
    (prefix, '$' :: rest) =>
      case span (\c => isAlphaNum c || c == '_' || c == '\'') rest of
        (variable, '~' :: regexPart) =>
          case span (/= '$') regexPart of
            (regex, '$' :: suffix) =>
              Right (Hole ("$" ++ variable ++ "~" ++ regex ++ "$") variable 0 "value", suffix)
            _ => Left "Failed to parse regex hole"
        _ => Left "Failed to parse regex hole"
    _ => Left "Failed to parse regex hole"