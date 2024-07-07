module MatchManager

import Data.SortedMap
import Data.Vect

%default total

record MatchManager where
  constructor MkMatchManager
  opMatches : SortedMap Operation (List Operation.Match)

sortByOutput : Operation.Match -> Operation.Match -> Ordering
sortByOutput a b = compare a.output b.output

matchValue : MatchManager -> Value -> MatchManager
matchValue mm v =
  let updateMatches : Operation -> List Operation.Match -> List Operation.Match
      updateMatches op matches = Operation.match op v matches
  in record { opMatches = SortedMap.map updateMatches mm.opMatches } mm

create : Vect n InstBinding -> Function -> MatchManager
create insts f =
  let initMatches = foldl (\acc, inst =>
                      foldl (\acc', laneOp =>
                        let op = LaneOp.getOperation laneOp
                        in case SortedMap.lookup op acc' of
                             Nothing => SortedMap.insert op [] acc'
                             Just _ => acc'
                      ) acc (InstBinding.getLaneOps inst)
                    ) empty insts
      mm = MkMatchManager initMatches
      mm' = foldl (flip matchValue) mm (concatMap blockInstructions (functionBlocks f))
  in record { opMatches = SortedMap.map (sortBy sortByOutput) mm'.opMatches } mm'

createWithInstructions : Vect n InstBinding -> Vect m Instruction -> MatchManager
createWithInstructions insts toMatch =
  let initMatches = foldl (\acc, inst =>
                      foldl (\acc', laneOp =>
                        let op = LaneOp.getOperation laneOp
                        in case SortedMap.lookup op acc' of
                             Nothing => SortedMap.insert op [] acc'
                             Just _ => acc'
                      ) acc (InstBinding.getLaneOps inst)
                    ) empty insts
      mm = MkMatchManager initMatches
      mm' = foldl (flip matchValue) mm toMatch
  in record { opMatches = SortedMap.map (sortBy sortByOutput) mm'.opMatches } mm'

getMatches : MatchManager -> Operation -> List Operation.Match
getMatches mm op = fromMaybe [] (SortedMap.lookup op mm.opMatches)

getMatchesForOutput : MatchManager -> Operation -> Value -> List Operation.Match
getMatchesForOutput mm op output =
  let matches = getMatches mm op
      dummyMatch = MkMatch False [] output
      (lower, rest) = span (\m => sortByOutput m dummyMatch == LT) matches
      (equal, _) = span (\m => sortByOutput m dummyMatch == EQ) rest
  in equal