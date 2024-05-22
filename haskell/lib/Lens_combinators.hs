import Data.List (intercalate)

data RE = Str String | Re String deriving (Show, Eq)

data Lens a
  = Del RE String
  | Store RE
  | Value String
  | Counter String
  | Seq String
  | Key RE
  | Label String
  deriving (Show, Eq)

concat :: Lens a -> Lens a -> [String] -> [String]
concat l1 l2 = concatMap (l2 . concat l1 l2)

union :: Lens a -> Lens a -> [String] -> [String]
union l1 l2 = (++) <$> l1 <*> l2

repetition :: String -> Lens a -> [String] -> [String]
repetition op l = case op of
  "*" -> concatMap (++ repetition op l) . l
  "+" -> concatMap (++ repetition op l) . filter (not . null) . l
  "?" -> (++ repetition op l) <$> l
  _ -> error "Invalid repetition operator"

subtree :: Lens a -> [String] -> [[String]]
subtree l = filter (not . null) . map l

square :: Lens a -> Lens a -> Lens a -> [String] -> [String]
square left body right = concatMap process
  where
    process s =
      case left s of
        [] -> []
        ls ->
          case right ls of
            [] -> []
            rs -> body (drop (length ls) $ init rs)