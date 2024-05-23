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

subtree :: Lens a -> [[String]]
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

data IndependComp a b = IndependComp a b

instance (FVisitor a, FVisitor b) => FVisitor (IndependComp a b) where
  before o (IndependComp v1 v2) = IndependComp (before o v1) (before o v2)
  after o (IndependComp v1 v2) = IndependComp (after o (v1' v1)) (after o (v2' v2))
    where
      v1' = after o v1
      v2' = after o v2

data ThreadedComp a b = ThreadedComp a b

instance (FVisitor a, CompReceiver b) => FVisitor (ThreadedComp a b) where
  before o (ThreadedComp v1 v2) = ThreadedComp (before o v1) (before o v2 (exportVisitor v1))
  after o (ThreadedComp v1 v2) = ThreadedComp (after o (v1' v1)) (after o (v2' v2))
    where
      v1' = after o v1
      v2' = after o v2

data ConditionalComp a b = ConditionalComp a b

instance (FVisitor a, FVisitor b) => FVisitor (ConditionalComp a b) where
  before o (ConditionalComp v1 v2)
    | continueVisit v1' = ConditionalComp v1' (before o v2)
    | otherwise = ConditionalComp v1' v2
    where
      v1' = before o v1
  after o (ConditionalComp v1 v2)
    | continueVisit v1' = ConditionalComp v1' (after o (v2' v2))
    | otherwise = ConditionalComp v1' v2
    where
      v1' = after o v1
      v2' = after o v2