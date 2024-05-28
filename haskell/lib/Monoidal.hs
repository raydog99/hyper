class MonoidalCategory obj mor | obj -> mor where
  id :: obj a -> mor a a
  compose :: mor b c -> mor a b -> mor a c
  tensor :: obj a -> obj b -> obj (a, b)
  tensorMor :: mor a a' -> mor b b' -> mor (a, b) (a', b')