type token =
  | WhiteSpace
  | LineTerminator
  | Comment of string
  | CommonToken of string
  | DivPunctuator
  | RightBracePunctuator
  | RegularExpressionLiteral of string
  | TemplateSubstitutionTail
  | HashbangComment

val lexer : char list -> token list