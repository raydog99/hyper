module ControlReifier

import Data.SortedMap
import ControlDependence
import DependenceAnalysis
import VLoop

%default total

record ControlReifier where
  constructor MkControlReifier
  ctx : Context
  reifiedValues : SortedMap (ControlCondition, VLoop) Value
  insertedInsts : List Value

reify : ControlReifier -> Maybe ControlCondition -> VLoop -> Value
reify cr Nothing vl = ConstantInt (I1Type (ctx cr)) 1
reify cr (Just c) vl =
  case lookup (c, vl) (reifiedValues cr) of
    Just v => v
    Nothing =>
      let reified = case c of
            ConditionAnd and =>
              do
                _ <- reify cr (parent and) vl
                let cond = if isTrue and
                           then cond and
                           else let notInst = Not (cond and)
                                in do
                                  cr.insertedInsts := notInst :: cr.insertedInsts
                                  addInstruction vl notInst (parent and)
                                  pure notInst
                createOneHotPhi vl (parent and) cond (ConstantInt (I1Type (ctx cr)) 0) "reified.onehot"
            ConditionOr or =>
              foldl (\acc, c2 =>
                let tmp = Or acc (reify cr (Just c2) vl)
                in do
                  cr.insertedInsts := tmp :: cr.insertedInsts
                  addInstruction vl tmp Nothing
                  pure tmp
              ) (reify cr (Just (head (conds or))) vl) (tail (conds or))
      in do
        cr.reifiedValues := insert (c, vl) reified (reifiedValues cr)
        case c of
          ConditionAnd and => reify cr (complement and) vl
          _ => pure ()
        pure reified

hasValue : ControlReifier -> Maybe ControlCondition -> VLoop -> Bool
hasValue _ Nothing _ = True
hasValue cr (Just c) vl = isJust (lookup (c, vl) (reifiedValues cr))

getValue : ControlReifier -> Maybe ControlCondition -> VLoop -> Value
getValue cr c vl =
  assert (hasValue cr c vl) $
    case c of
      Nothing => ConstantInt (I1Type (ctx cr)) 1
      Just c' => fromMaybe (ConstantInt (I1Type (ctx cr)) 0) (lookup (c', vl) (reifiedValues cr))