#include "rv/analysis/costModel.h"

#include "rv/PlatformInfo.h"
#include "rv/vectorizationInfo.h"
#include "rv/region/Region.h"
#include "rv/annotations.h"

#include "llvm/IR/Instructions.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DerivedTypes.h"

using namespace llvm;

#define IF_DEBUG_CM if (true)

namespace rv {


CostModel::CostModel(PlatformInfo & _platInfo)
: platInfo(_platInfo)
, tti(*platInfo.getTTI())
{}


bool
CostModel::needsReplication(const Instruction & inst) const {
  return true; // TODO query vecInfo
}

static
size_t
CountDataPhis(const BasicBlock & block) {
  size_t t = 0;
  for (auto & inst : block) {
    if (!isa<PHINode>(inst)) continue;
    t += (size_t) inst.getType()->isFloatingPointTy();
  }
  return t;
}

size_t
CostModel::pickWidthForMapping(const VectorMapping & mapping) const {
  if (mapping.vectorFn) return mapping.vectorWidth;

  // default to vector of bytes
  size_t vecWidth = platInfo.getMaxVectorWidth();

  auto * scaFuncTy = mapping.scalarFn->getFunctionType();
  for (size_t i = 0; i < scaFuncTy->getNumParams(); ++i) {
    auto argShape = mapping.argShapes[i];
    if (argShape.isVarying()) {
      vecWidth = std::min(vecWidth, pickWidthForType(*scaFuncTy->getParamType(i), vecWidth));
    }
  }

  return vecWidth;
}

size_t
CostModel::pickWidthForInstruction(const Instruction & inst, size_t maxWidth) const {
  if (!needsReplication(inst)) return maxWidth; // remains scalar

// check call mappings, critical sections
  auto * call = dyn_cast<CallInst>(&inst);
  if (call) {
    auto * callee = dyn_cast_or_null<Function>(call->getCalledValue());
    if (!callee) return 1;

    // check if this is a critical section
    if (IsCriticalSection(*callee)) return maxWidth;

    // find widest available implementation
    size_t sampleWidth = maxWidth;
    StringRef calleeName = callee->getName();
    for (; sampleWidth > 1; sampleWidth /= 2) {
      if (platInfo.isFunctionVectorizable(calleeName, sampleWidth)) {
        break;
      }
    }

    IF_DEBUG_CM { errs() << "cm: max width for " << calleeName << " is " << sampleWidth << "\n"; }
    return sampleWidth;
  }

// TODO memory access pattern
  const auto & instTy = *inst.getType();
  if (instTy.isVoidTy()) {
    return maxWidth;
  }

// default to type based width
  return pickWidthForType(instTy, maxWidth);
}

size_t
CostModel::pickWidthForType(const Type & type, size_t maxWidth) const {
  // assume that only floating point values are vectorized
  if (type.isFloatingPointTy()) {
    size_t typeBits = type.isFloatTy() ? 32 : 64;
    maxWidth = std::min<size_t>(maxWidth, platInfo.getMaxVectorBits() / typeBits);
  }

  return maxWidth;
}

size_t
CostModel::pickWidthForBlock(const BasicBlock & block, size_t maxWidth) const {
  for (const auto & inst : block) maxWidth = pickWidthForInstruction(inst, maxWidth);
  size_t numDataPhis = CountDataPhis(block);

  if (numDataPhis > 2*maxWidth) {
    // do not vectorize if too many values are in flight
    // FIXME this is a crude method to estimate the hazard of being surpassed by plain SLP vectorization
    // this assumes that the number of data-bearing (as compared to address computation, control slice) phi nodes correlates with instruction level parallelism.
    // Hence, it might be sensible to relay on SLP vectorization instead of turning each one of them into a vector PHI.
    return 1;
  }

  return maxWidth;
}

size_t
CostModel::pickWidthForRegion(const Region & region, size_t maxWidth) const {
  size_t width = maxWidth;

  IF_DEBUG_CM { errs() << "cm: bounding vector width for region " << region.str() << ", initial max width " << maxWidth << "\n"; }

  region.for_blocks([&](const BasicBlock & block) {
      width = pickWidthForBlock(block, maxWidth);
      return width > 1;
  });

  return width;
}


}
