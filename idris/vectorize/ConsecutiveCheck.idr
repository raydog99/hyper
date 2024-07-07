module ConsecutiveCheck

import Data.SortedMap
import Data.SortedSet
import Data.Vect

%default total

data LLVMValue = MkLLVMValue Int
data LLVMType = MkLLVMType Int
data LLVMLoop = MkLLVMLoop Int
data LLVMSCEV = MkLLVMSCEV Int

record ScalarEvolution where
  constructor MkScalarEvolution
  getSCEV : LLVMValue -> LLVMSCEV
  isAvailableAtLoopEntry : LLVMSCEV -> LLVMLoop -> Bool
  getAddRecExpr : List LLVMSCEV -> LLVMLoop -> LLVMSCEV
  getConstant : Int -> LLVMSCEV
  getMinusSCEV : LLVMSCEV -> LLVMSCEV -> LLVMSCEV
  evaluateAtIteration : LLVMSCEV -> LLVMSCEV -> LLVMSCEV

record LoopInfo where
  constructor MkLoopInfo
  getLoopFor : LLVMValue -> Maybe LLVMLoop
  getLoopParent : LLVMLoop -> Maybe LLVMLoop

record DataLayout where
  constructor MkDataLayout
  getTypeStoreSize : LLVMType -> Int
  getIndexSizeInBits : Int -> Int

getLoopNest : LoopInfo -> LLVMValue -> List LLVMLoop
getLoopNest li v = 
  case getLoopFor li v of
    Nothing => []
    Just l => l :: loop l
  where
    loop : LLVMLoop -> List LLVMLoop
    loop l = case getLoopParent li l of
               Nothing => []
               Just parent => parent :: loop parent

getAddressSpaceOperand : LLVMValue -> Int
getAddressSpaceOperand _ = 0

record AddRecLoopRewriter where
  constructor MkAddRecLoopRewriter
  se : ScalarEvolution
  loops : SortedMap LLVMLoop LLVMLoop
  success : Bool

rewriteAddRec : AddRecLoopRewriter -> LLVMSCEV -> LLVMSCEV
rewriteAddRec rewriter expr = expr

isEquivalent : LLVMValue -> LLVMValue -> ScalarEvolution -> LoopInfo -> Bool
isEquivalent ptrA ptrB se li =
  let scevA = getSCEV se ptrA
      scevB = getSCEV se ptrB
      loopNest1 = getLoopNest li ptrA
      loopNest2 = getLoopNest li ptrB
      loops = SortedMap.fromList $ zip loopNest2 loopNest1
      rewriter = MkAddRecLoopRewriter se loops True
      newScevB = rewriteAddRec rewriter scevB
  in scevA == newScevB

isConsecutive : LLVMValue -> LLVMValue -> DataLayout -> ScalarEvolution -> LoopInfo -> Bool
isConsecutive a b dl se li = 
  let ptrA = getLoadStorePointerOperand a
      ptrB = getLoadStorePointerOperand b
      asA = getAddressSpaceOperand a
      asB = getAddressSpaceOperand b
      loopNest1 = getLoopNest li ptrA
      loopNest2 = getLoopNest li ptrB
      loops = SortedMap.fromList $ zip loopNest2 loopNest1
      rewriter = MkAddRecLoopRewriter se loops True
      idxWidth = getIndexSizeInBits dl asA
      ty = getElementType ptrA
      size = getTypeStoreSize dl ty
      offsetSCEVA = getConstant se 0 
      offsetSCEVB = getConstant se 0 
      offsetDeltaSCEV = getMinusSCEV se offsetSCEVB offsetSCEVA
      ptrSCEVA = getSCEV se ptrA
      ptrSCEVB = rewriteAddRec rewriter (getSCEV se ptrB)
      sizeSCEV = getConstant se size
      baseDelta = getMinusSCEV se sizeSCEV offsetDeltaSCEV
      x = getAddExpr se [ptrSCEVA, baseDelta]
  in x == ptrSCEVB

getLoadStorePointerOperand : LLVMValue -> LLVMValue
getLoadStorePointerOperand v = v

getElementType : LLVMValue -> LLVMType
getElementType _ = MkLLVMType 0

getAddExpr : ScalarEvolution -> List LLVMSCEV -> LLVMSCEV
getAddExpr se _ = getConstant se 0

record SizeGenerator where
  constructor MkSizeGenerator
  unknownToSize : SortedMap LLVMSCEV Int

getSize : SizeGenerator -> LLVMSCEV -> (Int, SizeGenerator)
getSize (MkSizeGenerator m) expr = 
  case SortedMap.lookup expr m of
    Just size => (size, MkSizeGenerator m)
    Nothing => let size = (SortedMap.size m * 2) + SortedMap.size m
               in (size, MkSizeGenerator (SortedMap.insert expr size m))

record IterationGenerator where
  constructor MkIterationGenerator
  iterations : SortedMap LLVMLoop Int
  offset : Int

getIteration : IterationGenerator -> LLVMLoop -> (Int, IterationGenerator)
getIteration (MkIterationGenerator m offset) l = 
  case SortedMap.lookup l m of
    Just i => (i, MkIterationGenerator m offset)
    Nothing => let i = SortedMap.size m
               in (i, MkIterationGenerator (SortedMap.insert l i m) offset)

record SCEVFingerprinter where
  constructor MkSCEVFingerprinter
  se : ScalarEvolution
  sg : SizeGenerator
  ig : IterationGenerator

visitSCEV : SCEVFingerprinter -> LLVMSCEV -> (LLVMSCEV, SCEVFingerprinter)
visitSCEV fp expr = (expr, fp)  -- Placeholder implementation

fingerprintSCEV : ScalarEvolution -> LLVMSCEV -> SizeGenerator -> Vect n IterationGenerator -> Vect n Int
fingerprintSCEV se expr sg igs = 
  map (\ig => let (result, _) = visitSCEV (MkSCEVFingerprinter se sg ig) expr
              in case result of
                   MkLLVMSCEV x => x) igs

record EquivalenceClasses a where
  constructor MkEquivalenceClasses
  parent : SortedMap a a
  size : SortedMap a Int

find : Ord a => EquivalenceClasses a -> a -> (a, EquivalenceClasses a)
find (MkEquivalenceClasses p s) x = 
  case SortedMap.lookup x p of
    Nothing => (x, MkEquivalenceClasses (SortedMap.insert x x p) (SortedMap.insert x 1 s))
    Just px => if px == x
               then (x, MkEquivalenceClasses p s)
               else let (root, ec') = find (MkEquivalenceClasses p s) px
                    in (root, MkEquivalenceClasses (SortedMap.insert x root (parent ec')) (size ec'))

unionSets : Ord a => EquivalenceClasses a -> a -> a -> EquivalenceClasses a
unionSets ec x y = 
  let (xroot, ec1) = find ec x
      (yroot, ec2) = find ec1 y
  in if xroot == yroot
     then ec2
     else let xsize = fromMaybe 0 (SortedMap.lookup xroot (size ec2))
              ysize = fromMaybe 0 (SortedMap.lookup yroot (size ec2))
          in if xsize < ysize
             then MkEquivalenceClasses 
                    (SortedMap.insert xroot yroot (parent ec2))
                    (SortedMap.insert yroot (xsize + ysize) (size ec2))
             else MkEquivalenceClasses
                    (SortedMap.insert yroot xroot (parent ec2))
                    (SortedMap.insert xroot (xsize + ysize) (size ec2))

findConsecutiveAccesses : ScalarEvolution -> DataLayout -> LoopInfo -> List LLVMValue -> EquivalenceClasses LLVMValue -> Nat -> List (LLVMValue, LLVMValue)
findConsecutiveAccesses se dl li accesses equivClasses numFingerprints = 
  let sg = MkSizeGenerator empty
      igs = replicate numFingerprints (MkIterationGenerator empty 0)
      (consec, _, _) = foldl (processAccess se dl li) ([], empty, empty) accesses
  in consec
where
  processAccess : ScalarEvolution -> DataLayout -> LoopInfo -> 
                  (List (LLVMValue, LLVMValue), SortedMap Int (List LLVMValue), SortedMap LLVMValue (Vect n Int)) -> 
                  LLVMValue -> 
                  (List (LLVMValue, LLVMValue), SortedMap Int (List LLVMValue), SortedMap LLVMValue (Vect n Int))
  processAccess se dl li (consec, fpToAcc, accToFp) i = 
    let ptr = getLoadStorePointerOperand i
        ptrSCEV = getSCEV se ptr
        fingerprints = fingerprintSCEV se ptrSCEV sg igs
        size = getTypeStoreSize dl (getElementType ptr)
        left = head fingerprints - size
        leftAccesses = fromMaybe [] (SortedMap.lookup left fpToAcc)
        newConsec = foldl (\acc leftI => 
                      if isConsecutive leftI i dl se li
                      then (leftI, i) :: acc
                      else acc) 
                    consec 
                    leftAccesses
        equivAccesses = fromMaybe [] (SortedMap.lookup (head fingerprints) fpToAcc)
        newEquivClasses = foldl (\ec i2 => 
                            if isEquivalent (getLoadStorePointerOperand i) (getLoadStorePointerOperand i2) se li
                            then unionSets ec i i2
                            else ec)
                          equivClasses
                          equivAccesses
        right = head fingerprints + size
        rightAccesses = fromMaybe [] (SortedMap.lookup right fpToAcc)
        finalConsec = foldl (\acc rightI =>
                        if isConsecutive i rightI dl se li
                        then (i, rightI) :: acc
                        else acc)
                      newConsec
                      rightAccesses
        newFpToAcc = SortedMap.insertWith (++) (head fingerprints) [i] fpToAcc
        newAccToFp = SortedMap.insert i fingerprints accToFp
    in (finalConsec, newFpToAcc, newAccToFp)