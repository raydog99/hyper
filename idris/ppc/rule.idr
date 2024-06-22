module Rule

import Data.String
import Data.List
import Data.Maybe

mutual
  public export
  data Ast : Type where
    String : String -> Ast
    Template : List String -> Ast
    Equal : Ast -> Ast -> Ast
    NotEqual : Ast -> Ast -> Ast
    Rewrite : Ast -> (Ast, Ast) -> Ast
    Match : Ast -> List (Ast, List Ast) -> Ast
    True : Ast
    False : Ast
    Option : String -> Ast

public export
Rule : Type
Rule = List Ast

public export
record Options where
  constructor MkOptions
  nested : Bool
  strict : Bool

ParseError : Type
ParseError = String

parseAst : List String -> Either ParseError (Ast, List String)
parseAst [] = Left "Unexpected end of input"
parseAst ("true" :: rest) = Right (True, rest)
parseAst ("false" :: rest) = Right (False, rest)
parseAst ("option" :: name :: rest) = Right (Option name, rest)
parseAst ("=" :: lhs :: "=" :: rhs :: rest) = do
  (lhsAst, _) <- parseAst [lhs]
  (rhsAst, restAst) <- parseAst [rhs]
  pure (Equal lhsAst rhsAst, rest)
parseAst ("!=" :: lhs :: rhs :: rest) = do
  (lhsAst, _) <- parseAst [lhs]
  (rhsAst, restAst) <- parseAst [rhs]
  pure (NotEqual lhsAst rhsAst, rest)
parseAst ("->" :: lhs :: "->" :: rhs :: rest) = do
  (lhsAst, _) <- parseAst [lhs]
  (rhsAst, restAst) <- parseAst [rhs]
  pure (Rewrite lhsAst (rhsAst, rhsAst), rest)
parseAst (token :: rest) = Right (String token, rest)

parseRule : List String -> Either ParseError Rule
parseRule [] = Right []
parseRule ("," :: rest) = parseRule rest
parseRule tokens = do
  (ast, rest) <- parseAst tokens
  (::) ast <$> parseRule rest

export
create : String -> Either String Rule
create ruleText =
  let tokens = words ruleText in
  case tokens of
    "where" :: rest => 
      case parseRule rest of
        Left err => Left ("Parse error: " ++ err)
        Right rule => Right rule
    _ => Left "Rule must start with 'where'"

export
options : Rule -> Options
options = foldl updateOptions (MkOptions False False)
  where
    updateOptions : Options -> Ast -> Options
    updateOptions opts (Option "nested") = record { nested = True } opts
    updateOptions opts (Option "strict") = record { strict = True } opts
    updateOptions opts _ = opts

export
isStrict : Rule -> Bool
isStrict rule = strict (options rule)