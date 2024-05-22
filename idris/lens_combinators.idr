import Data.String

%default total

data RE = StrRE String | RegexRE String

Show RE where
  show (StrRE s) = "StrRE \"" ++ s ++ "\""
  show (RegexRE r) = "RegexRE \"" ++ r ++ "\""

data Lens : Type -> Type where
  Del : RE -> String -> Lens String
  Store : RE -> Lens String
  Value : String -> Lens String
  Counter : String -> Lens String
  Seq : String -> Lens String
  Key : RE -> Lens String
  Label : String -> Lens String

concat : Lens a -> Lens a -> List a -> List a
concat l1 l2 input = concatMap (\x => concatMap (apply l2) (apply l1 x)) input

union : Lens a -> Lens a -> a -> a
union l1 l2 input =
  case apply l1 input of
    [] => head' (apply l2 input)
    res => head' res

repetition : String -> Lens a -> List a -> List a
repetition op l input =
  case op of
    "*" => concatMap (repetition op l) (rights input)
    "+" => concatMap (repetition op l) (rights (filter (not . null) input))
    "?" => rights input ++ concatMap (repetition op l) (lefts input)
    _ => []
  where
    rights : List a -> List a
    rights = concatMap (\x => apply l x)

    lefts : List a -> List a
    lefts = concatMap (\x => case apply l x of
                                [] => [x]
                                _ => [])

subtree : Lens a -> List a -> List (List a)
subtree l = map (apply l)

square : Lens a -> Lens a -> Lens a -> a -> a
square left body right input =
  case apply left input of
    [] => ""
    (l :: ls) =>
      case apply right l of
        [] => ""
        (r :: rs) => let mid = substr (length l) (length r - length l) r in
                     l ++ concatMap (apply body) mid ++ drop (length mid) r

apply : Lens a -> a -> List a
apply (Del re val) input = ?applyDel
apply (Store re) input = ?applyStore
apply (Value val) input = [val]
apply (Counter name) input = ?applyCounter
apply (Seq name) input = ?applySeq
apply (Key re) input = ?applyKey
apply (Label val) input = [val]

head' : List a -> a
head' [] = ?missingHead
head' (x :: _) = x

substr : Nat -> Nat -> String -> String
substr start len str = pack (substr' (unpack str) start len)
  where
    substr' : List Char -> Nat -> Nat -> List Char
    substr' str start len = take len (drop start str)