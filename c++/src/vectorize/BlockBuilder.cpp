#include "BlockBuilder.h"
#include "ControlDependence.h"
#include "llvm/IR/IRBuilder.h"

using namespace llvm;

BlockBuilder::BlockBuilder(
    BasicBlock *EntryBB,
    std::function<llvm::Value *(llvm::Value *)> EmitCondition)
    : Ctx(EntryBB->getContext()), F(EntryBB->getParent()),
      EmitCondition(EmitCondition), ActiveConds({{nullptr, EntryBB}}) {}

BasicBlock *BlockBuilder::createBlock() {
  return BasicBlock::Create(Ctx, "", F);
}

namespace {

class ConditionEmitter {
  IRBuilder<> IRB;
  const ControlCondition *Common;
  std::function<llvm::Value *(llvm::Value *)> EmitCondition;
  DenseMap<const ControlCondition *, Value *> Emitted;

  Value *emitAnd(const ConditionAnd *And) {
    if (auto *V = Emitted.lookup(And))
      return V;
    auto *V =
        IRB.CreateAnd(emit(And->Parent),
                      And->IsTrue ? EmitCondition(And->Cond)
                                  : IRB.CreateNot(EmitCondition(And->Cond)));
    return Emitted[And] = V;
  }

  Value *emitOr(const ConditionOr *Or) {
    if (auto *V = Emitted.lookup(Or))
      return V;
    return Emitted[Or] = emitDisjunction(Or->Conds);
  }

public:
  ConditionEmitter(BasicBlock *BB, const ControlCondition *Common,
                   decltype(EmitCondition) EmitCondition)
      : IRB(BB), Common(Common), EmitCondition(EmitCondition) {}
  Value *emit(const ControlCondition *C) {
    if (C == Common || !C)
      return IRB.getTrue();
    if (auto *And = dyn_cast<ConditionAnd>(C))
      return emitAnd(And);
    return emitOr(cast<ConditionOr>(C));
  }
  Value *emitDisjunction(ArrayRef<const ControlCondition *> Conds) {
    SmallVector<Value *, 8> Values;
    for (auto *C : Conds)
      Values.push_back(emit(C));
    return IRB.CreateOr(Values);
  }
};

} // namespace

BasicBlock *BlockBuilder::getBlockFor(const ControlCondition *C) {
  if (auto *BB = ActiveConds.lookup(C))
    return BB;

  // Get active conditions that use C and
  // unmark all intermediate semi-active conditions.
  auto GetActiveConds = [&](const ControlCondition *C,
                            SmallPtrSetImpl<const ControlCondition *> &Conds) {
    SmallPtrSet<const ControlCondition *, 4> Visited;
    SmallVector<const ControlCondition *> Worklist{C};
    assert(SemiActiveConds.count(C));
    while (!Worklist.empty()) {
      auto *C2 = Worklist.pop_back_val();
      if (!Visited.insert(C2).second)
        continue;

      if (ActiveConds.count(C2)) {
        Conds.insert(C2);
        continue;
      }

      auto It = SemiActiveConds.find(C2);
      assert(It != SemiActiveConds.end());
      Worklist.append(It->second.begin(), It->second.end());
      SemiActiveConds.erase(It);
    }
  };

  // If C is a semi-active condition,
  // join all of the blocks using C to the block for C
  if (SemiActiveConds.count(C)) {
    SmallPtrSet<const ControlCondition *, 4> Conds;
    GetActiveConds(C, Conds);
    auto *BB = createBlock();
    for (auto *C2 : Conds) {
      auto It = ActiveConds.find(C2);
      assert(It != ActiveConds.end());
      BranchInst::Create(BB, It->second);
      ActiveConds.erase(It);
    }
    ActiveConds[C] = BB;
    return BB;
  }

  if (auto *And = dyn_cast<ConditionAnd>(C)) {
    auto *IfTrue = createBlock();
    auto *IfFalse = createBlock();
    BranchInst::Create(IfTrue, IfFalse, EmitCondition(And->Cond),
                       getBlockFor(And->Parent));
    auto *BB = And->IsTrue ? IfTrue : IfFalse;

    assert(ActiveConds.count(And->Parent));
    ActiveConds.erase(And->Parent);
    SemiActiveConds[And->Parent].assign({And, And->Complement});
    ActiveConds[And] = BB;
    ActiveConds[And->Complement] = And->IsTrue ? IfFalse : IfTrue;
    return BB;
  }

  auto *Or = cast<ConditionOr>(C);
  SmallPtrSet<const ControlCondition *, 4> Conds;
  auto *CommonC = Or->GreatestCommonCond;
  if (!SemiActiveConds.count(CommonC)) {
    (void)getBlockFor(CommonC);
    Conds.insert(CommonC);
  } else {
    GetActiveConds(CommonC, Conds);
  }

  auto *BB = createBlock();
  auto *AuxBB = createBlock();
  SmallPtrSet<const ControlCondition *, 4> CondsToJoin, Joined;
  CondsToJoin.insert(Or->Conds.begin(), Or->Conds.end());
  for (auto *C2 : Conds) {
    auto It = ActiveConds.find(C2);
    assert(It != ActiveConds.end());
    auto *BB2 = It->second;

    if (CondsToJoin.count(C2)) {
      BranchInst::Create(BB, BB2);
      Joined.insert(C2);
    } else {
      BranchInst::Create(AuxBB, BB2);
    }
    ActiveConds.erase(It);
  }

  SmallVector<const ControlCondition *, 4> UnjoinedConds;
  for (auto *C : Or->Conds)
    if (!Joined.count(C))
      UnjoinedConds.push_back(C);

  // Branch conditionally from AuxBB to BB
  auto *DrainBB = createBlock();
  if (UnjoinedConds.empty())
    BranchInst::Create(DrainBB, AuxBB);
  else {
    ConditionEmitter CE(AuxBB, CommonC, EmitCondition);
    BranchInst::Create(BB, DrainBB,
                       CE.emitDisjunction(UnjoinedConds), AuxBB);
  }

  auto *DummyC = getDummyCondition();
  ActiveConds[C] = BB;
  ActiveConds[DummyC] = DrainBB;
  SemiActiveConds[CommonC] = {C, DummyC};
  assert(!ActiveConds.lookup(CommonC));
  return BB;
}

void BlockBuilder::setBlockForCondition(llvm::BasicBlock *BB,
                                        const ControlCondition *C) {
  assert(ActiveConds.count(C) && "can only set block for active condition");
  ActiveConds[C] = BB;
}

const ControlCondition *BlockBuilder::getDummyCondition() {
  return reinterpret_cast<const ControlCondition *>(++DummyCounter);
}