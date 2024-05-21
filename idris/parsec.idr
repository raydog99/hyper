data Parser : Type -> Type where
  Return : a -> Parser a
  Satisfy : (a -> Bool) -> Parser a

return : a -> Parser a
return = Return

satisfy : (a -> Bool) -> Parser a
satisfy = Satisfy

runParser : Parser a -> List a -> Maybe (a, List a)
runParser (Return x) input = Just (x, input)
runParser (Satisfy pred) [] = Nothing
runParser (Satisfy pred) (x :: xs) =
  if pred x
    then Just (x, xs)
    else Nothing