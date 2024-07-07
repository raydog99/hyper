#include "Heuristic.h"
#include "VectorPack.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"

using namespace llvm;

#define DEBUG_TYPE "heuristic"

static constexpr float C_Splat = 1.0;
static constexpr float C_Perm = 2;
static constexpr float C_Insert = 2;
static constexpr float C_Shuffle = 2;
static constexpr float C_Extract = 1.0;

static cl::opt<bool> AllowDeinterleave("allow-deinterleave", cl::init(false));
static cl::opt<bool> AllowTranspose("allow-transpose", cl::init(false));

float Heuristic::getCost(const VectorPack *VP) {
  float Cost = VP->getProducingCost();
  for (auto *OP : VP->getOperandPacks()) {
    if (all_of(*OP, [](Value *V) { return isa_and_nonnull<CmpInst>(V); }))
      continue;
    Cost += getCost(OP);
  }
  return Cost;
}

SmallVector<const OperandPack *> deinterleave(const VectorPackContext *VPCtx,
                                              const OperandPack *OP,
                                              unsigned Stride);

const OperandPack *transpose(const VectorPackContext *VPCtx,
                             const OperandPack *OP, unsigned N) {
  if (OP->size() % N)
    return nullptr;
  unsigned M = OP->size() / N;
  OperandPack T;
  for (unsigned i = 0; i < M; i++)
    for (unsigned j = 0; j < N; j++)
      T.push_back((*OP)[j * M + i]);
  return VPCtx->getCanonicalOperandPack(T);
}

float Heuristic::getCost(Value *V) {
  if (!V)
    return 0;
  auto *I = dyn_cast<Instruction>(V);
  if (!I)
    return 0;

  auto It = ScalarCosts.find(I);
  if (It != ScalarCosts.end())
    return It->second;

  ScalarCosts[I] = 0;

  float Cost = Pkr->getScalarCost(I);
  for (Value *V : I->operands())
    Cost += getCost(V);
  return ScalarCosts[I] = Cost;
}