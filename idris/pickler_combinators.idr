Pu : Type -> Type
Pu a = a -> ()

pair : Pu a -> Pu b -> Pu (a, b)
pair pa pb = \(a, b) => do pa a; pb b

triple : Pu a -> Pu b -> Pu c -> Pu (a, b, c)
triple pa pb pc = \(a, b, c) => do pa a; pb b; pc c

quad : Pu a -> Pu b -> Pu c -> Pu d -> Pu (a, b, c, d)
quad pa pb pc pd = \(a, b, c, d) => do pa a; pb b; pc c; pd d

wrap : (a -> b) -> (b -> a) -> Pu b -> Pu a
wrap i j pb = do
  let a = j (pb ())
  pb (i a)

zeroTo : Nat -> Pu Nat
zeroTo 0 = \_ => ()
zeroTo n = wrap (\x => x `div` 256) (\x => x `mod` 256) (zeroTo (n `div` 256))

unit : Pu ()
unit = \_ => ()

char : Pu Char
char = wrap (chr . cast) (cast . ord) (zeroTo 255)

bool : Pu Bool
bool = wrap (cast . natToBool) (natToBool . cast) (zeroTo 1)
  where
    natToBool : Nat -> Bool
    natToBool 0 = False
    natToBool _ = True

nat : Pu Nat
nat = go 128
  where
    go : Nat -> Pu Nat
    go half n = if n < half
                  then unit n
                  else wrap (\x => half + (x `mod` half))
                            (\x => (x - half) `div` half)
                            (go half) n

fixedList : Pu a -> Nat -> Pu (List a)
fixedList pa 0 = \_ => []
fixedList pa n = wrap (\(x, xs) => x :: xs)
                      (\xs => case xs of
                                []        => (idiotic, [])
                                (x :: xs) => (x, xs))
                      (pair pa (fixedList pa (n - 1)))

list : Pu a -> Pu (List a)
list pa = \xs => do
  len <- nat (length xs)
  fixedList pa len xs

string : Pu String
string = list char

alt : (a -> Nat) -> List (Pu a) -> Pu a
alt tag [] = \_ => ()
alt tag (p :: ps) = wrap (\x => if tag x == 0 then 0 else 1 + (tag x - 1))
                          (\x => case x of
                                       0 => p ()
                                       n => alt tag ps (n - 1))
                          p

pMaybe : Pu a -> Pu (Maybe a)
pMaybe pa = alt tag [unit, wrap Just (fromMaybe undefined) pa]
  where
    tag : Maybe a -> Nat
    tag Nothing = 0
    tag (Just _) = 1

pEither : Pu a -> Pu b -> Pu (Either a b)
pEither pa pb = alt tag [wrap Left (either.1 . swap), wrap Right either.2 pb]
  where
    tag : Either a b -> Nat
    tag (Left _) = 0
    tag (Right _) = 1

    swap : Either a b -> (a, b)
    swap (Left x) = (x, idiotic)
    swap (Right y) = (idiotic, y)