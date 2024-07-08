#ifndef LOOP_UNROLLING_H
#define LOOP_UNROLLING_H

#include "llvm/IR/ValueMap.h"
#include "llvm/IR/ValueHandle.h"
#include "llvm/Transforms/Utils/UnrollLoop.h"


struct UnrolledValue {
  unsigned Iter;
  llvm::WeakTrackingVH V;
};

llvm::LoopUnrollResult UnrollLoopWithVMap(
    llvm::Loop *, llvm::UnrollLoopOptions, llvm::LoopInfo *,
    llvm::ScalarEvolution *, llvm::DominatorTree *, llvm::AssumptionCache *,
    const llvm::TargetTransformInfo *, bool PreserveLCSSA,
    llvm::ValueMap<llvm::Value *, UnrolledValue> &, llvm::Loop **);
#endif // LOOP_UNROLLING_H
