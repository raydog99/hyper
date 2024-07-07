module BlockBuilder

import Data.SortedMap
import Data.Maybe
import Data.List

%default total

data LLVMType = I1 | I32 | Void | Pointer LLVMType

data LLVMValue = ConstInt LLVMType Integer
               | Var String LLVMType
               | FuncCall String (List LLVMValue) LLVMType

data Instruction = Br String
                 | CondBr LLVMValue String String
                 | BinOp String LLVMValue LLVMValue LLVMType
                 | Phi (List (LLVMValue, String)) LLVMType

record BasicBlock where
  constructor MkBasicBlock
  name : String
  instructions : List Instruction

mutual
  data ControlCondition = And ConditionAnd
                        | Or ConditionOr

  record ConditionAnd where
    constructor MkConditionAnd
    parent : Maybe ControlCondition
    cond : LLVMValue
    isTrue : Bool
    complement : Maybe ControlCondition

  record ConditionOr where
    constructor MkConditionOr
    conds : List ControlCondition
    greatestCommonCond : Maybe ControlCondition

record BlockBuilder where
  constructor MkBlockBuilder
  activeConds : SortedMap (Maybe ControlCondition) BasicBlock
  semiActiveConds : SortedMap ControlCondition (List ControlCondition)
  dummyCounter : Integer
  emitCondition : LLVMValue -> LLVMValue

createBlock : String -> BasicBlock
createBlock name = MkBasicBlock name []

addInstruction : Instruction -> BasicBlock -> BasicBlock
addInstruction instr (MkBasicBlock name instrs) = MkBasicBlock name (instrs ++ [instr])

createBlockBuilder : BasicBlock -> (LLVMValue -> LLVMValue) -> BlockBuilder
createBlockBuilder entryBB emitCond = 
  MkBlockBuilder (insert Nothing entryBB empty) empty 0 emitCond

getBlockFor : ControlCondition -> BlockBuilder -> (BasicBlock, BlockBuilder)
getBlockFor cond bb = 
  case lookup (Just cond) bb.activeConds of
    Just block -> (block, bb)
    Nothing -> 
      case lookup cond bb.semiActiveConds of
        Just _ -> handleSemiActiveCond cond bb
        Nothing -> 
          case cond of
            And andCond -> handleConditionAnd andCond bb
            Or orCond -> handleConditionOr orCond bb

handleSemiActiveCond : ControlCondition -> BlockBuilder -> (BasicBlock, BlockBuilder)
handleSemiActiveCond cond bb = 
  let conds = getActiveConds cond bb
      newBB = createBlock "semi_active"
      updatedBB = foldr (\c, accBB => 
        let oldBB = fromMaybe (createBlock "error") $ lookup (Just c) accBB.activeConds
            updatedOldBB = addInstruction (Br newBB.name) oldBB
            updatedActiveConds = insert (Just c) updatedOldBB accBB.activeConds
        in record { activeConds = updatedActiveConds } accBB
      ) bb conds
  in (newBB, record { activeConds = insert (Just cond) newBB updatedBB.activeConds } updatedBB)

handleConditionAnd : ConditionAnd -> BlockBuilder -> (BasicBlock, BlockBuilder)
handleConditionAnd andCond bb = 
  let ifTrue = createBlock "if_true"
      ifFalse = createBlock "if_false"
      (parentBB, updatedBB1) = case andCond.parent of
                                 Just parent -> getBlockFor parent bb
                                 Nothing -> (createBlock "error", bb)
      cond = bb.emitCondition andCond.cond
      updatedParentBB = addInstruction (CondBr cond ifTrue.name ifFalse.name) parentBB
      resultBB = if andCond.isTrue then ifTrue else ifFalse
      updatedBB2 = record { activeConds = insert (Just $ And andCond) resultBB updatedBB1.activeConds } updatedBB1
      updatedBB3 = case andCond.complement of
                     Just comp -> record { activeConds = insert (Just comp) (if andCond.isTrue then ifFalse else ifTrue) updatedBB2.activeConds } updatedBB2
                     Nothing -> updatedBB2
      updatedBB4 = case andCond.parent of
                     Just parent -> record { semiActiveConds = insert parent [And andCond, fromMaybe (And andCond) andCond.complement] updatedBB3.semiActiveConds } updatedBB3
                     Nothing -> updatedBB3
  in (resultBB, updatedBB4)

handleConditionOr : ConditionOr -> BlockBuilder -> (BasicBlock, BlockBuilder)
handleConditionOr orCond bb = 
  let commonC = orCond.greatestCommonCond
      (conds, updatedBB1) = case commonC of
                              Just c -> if isNothing (lookup c bb.semiActiveConds)
                                        then let (_, updatedBB) = getBlockFor (Or orCond) bb
                                             in ([c], updatedBB)
                                        else (getActiveConds c bb, bb)
                              Nothing -> ([], bb)
      newBB = createBlock "or_new"
      auxBB = createBlock "or_aux"
      (joined, updatedBB2) = foldr (\c, (joinedAcc, accBB) -> 
        let oldBB = fromMaybe (createBlock "error") $ lookup (Just c) accBB.activeConds
            (updatedOldBB, isJoined) = if c `elem` orCond.conds
                                       then (addInstruction (Br newBB.name) oldBB, True)
                                       else (addInstruction (Br auxBB.name) oldBB, False)
            updatedActiveConds = insert (Just c) updatedOldBB accBB.activeConds
        in (joinedAcc || isJoined, record { activeConds = updatedActiveConds } accBB)
      ) (False, updatedBB1) conds
      unjoinedConds = filter (\c -> not $ c `elem` joined) orCond.conds
      drainBB = createBlock "or_drain"
      updatedAuxBB = if null unjoinedConds
                     then addInstruction (Br drainBB.name) auxBB
                     else let ce = ConditionEmitter bb.emitCondition commonC
                              disjunction = emitDisjunction ce unjoinedConds
                          in addInstruction (CondBr disjunction newBB.name drainBB.name) auxBB
      dummyC = getDummyCondition bb
      updatedBB3 = record { activeConds = insert (Just $ Or orCond) newBB updatedBB2.activeConds } updatedBB2
      updatedBB4 = record { activeConds = insert (Just dummyC) drainBB updatedBB3.activeConds } updatedBB3
      updatedBB5 = case commonC of
                     Just c -> record { semiActiveConds = insert c [Or orCond, dummyC] updatedBB4.semiActiveConds } updatedBB4
                     Nothing -> updatedBB4
  in (newBB, updatedBB5)

getActiveConds : ControlCondition -> BlockBuilder -> List ControlCondition
getActiveConds c bb = 
  let visited = empty
      conds = []
      dfs : ControlCondition -> SortedMap ControlCondition () -> List ControlCondition -> (SortedMap ControlCondition (), List ControlCondition)
      dfs c2 visited conds = 
        if isJust $ lookup c2 visited
        then (visited, conds)
        else let updatedVisited = insert c2 () visited
             in if isJust $ lookup (Just c2) bb.activeConds
                then (updatedVisited, c2 :: conds)
                else case lookup c2 bb.semiActiveConds of
                       Just children -> foldl (\(accVisited, accConds), child -> 
                                          dfs child accVisited accConds
                                        ) (updatedVisited, conds) children
                       Nothing -> (updatedVisited, conds)
  in snd $ dfs c visited conds

getDummyCondition : BlockBuilder -> ControlCondition
getDummyCondition bb = 
  let updatedCounter = bb.dummyCounter + 1
  in And $ MkConditionAnd Nothing (ConstInt I1 updatedCounter) True Nothing

setBlockForCondition : BasicBlock -> ControlCondition -> BlockBuilder -> BlockBuilder
setBlockForCondition block cond bb = 
  if isJust $ lookup (Just cond) bb.activeConds
  then record { activeConds = insert (Just cond) block bb.activeConds } bb
  else bb

record ConditionEmitter where
  constructor MkConditionEmitter
  emitCondition : LLVMValue -> LLVMValue
  common : Maybe ControlCondition
  emitted : SortedMap ControlCondition LLVMValue

emitCE : ConditionEmitter -> Maybe ControlCondition -> LLVMValue
emitCE ce Nothing = ConstInt I1 1
emitCE ce (Just c) = 
  if c == fromMaybe c ce.common
  then ConstInt I1 1
  else case c of
         And andCond -> emitAnd ce andCond
         Or orCond -> emitOr ce orCond

emitAnd : ConditionEmitter -> ConditionAnd -> LLVMValue
emitAnd ce and = 
  case lookup (And and) ce.emitted of
    Just v -> v
    Nothing -> 
      let parent = emitCE ce and.parent
          cond = ce.emitCondition and.cond
          v = if and.isTrue
              then BinOp "and" parent cond I1
              else BinOp "and" parent (BinOp "not" cond (ConstInt I1 0) I1) I1
      in v

emitOr : ConditionEmitter -> ConditionOr -> LLVMValue
emitOr ce or = 
  case lookup (Or or) ce.emitted of
    Just v -> v
    Nothing -> 
      let v = emitDisjunction ce or.conds
      in v

emitDisjunction : ConditionEmitter -> List ControlCondition -> LLVMValue
emitDisjunction ce conds = 
  case conds of
    [] -> ConstInt I1 0
    [x] -> emitCE ce (Just x)
    x::xs -> BinOp "or" (emitCE ce (Just x)) (emitDisjunction ce xs) I1