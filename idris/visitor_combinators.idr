module VisitorCombinators

%access public export

data IndependComp : (Visitor a, Visitor b) => Visitor (IndependComp a b) where
  MkIndependComp : a -> b -> IndependComp a b

before : (Visitor a, Visitor b) => Object -> IndependComp a b -> IndependComp a b
before obj (MkIndependComp v1 v2) = MkIndependComp (before obj v1) (before obj v2)

after : (Visitor a, Visitor b) => Object -> IndependComp a b -> IndependComp a b
after obj (MkIndependComp v1 v2) = MkIndependComp (after obj (v1' v1)) (after obj (v2' v2))
  where
    v1' : a
    v1' = after obj v1
    v2' : b
    v2' = after obj v2

data ThreadedComp : (Visitor a, CompReceiver b) => Visitor (ThreadedComp a b) where
  MkThreadedComp : a -> b -> ThreadedComp a b

before : (Visitor a, CompReceiver b) => Object -> ThreadedComp a b -> ThreadedComp a b
before obj (MkThreadedComp v1 v2) = MkThreadedComp (before obj v1) (before obj v2 (exportVisitor v1))

after : (Visitor a, CompReceiver b) => Object -> ThreadedComp a b -> ThreadedComp a b
after obj (MkThreadedComp v1 v2) = MkThreadedComp (after obj (v1' v1)) (after obj (v2' v2))
  where
    v1' : a
    v1' = after obj v1
    v2' : b
    v2' = after obj v2

exportVisitor : (Visitor a, CompReceiver b) => ThreadedComp a b -> b
exportVisitor (MkThreadedComp v1 v2) = exportVisitor v2

data ConditionalComp : (Visitor a, Visitor b) => Visitor (ConditionalComp a b) where
  MkConditionalComp : a -> b -> ConditionalComp a b

before : (Visitor a, Visitor b) => Object -> ConditionalComp a b -> ConditionalComp a b
before obj (MkConditionalComp v1 v2) =
  case continueVisit v1' of
    True => MkConditionalComp v1' (before obj v2)
    False => MkConditionalComp v1' v2
  where
    v1' : a
    v1' = before obj v1

after : (Visitor a, Visitor b) => Object -> ConditionalComp a b -> ConditionalComp a b
after obj (MkConditionalComp v1 v2) =
  case continueVisit v1' of
    True => MkConditionalComp v1' (after obj (v2' v2))
    False => MkConditionalComp v1' v2
  where
    v1' : a
    v1' = after obj v1
    v2' : b
    v2' = after obj v2