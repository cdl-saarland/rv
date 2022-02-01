//===- src/analysis/costModel.cpp - Vector width heuristics --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "rv/analysis/costModel.h"

#include "rv/PlatformInfo.h"
#include "rv/vectorizationInfo.h"
#include "rv/region/Region.h"
#include "rv/annotations.h"
#include "rv/config.h"

#include "llvm/IR/Instructions.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Intrinsics.h"

#include "rv/utils.h"
#include "rvConfig.h"

using namespace llvm;

#if 1
#define IF_DEBUG_CM IF_DEBUG
#else
#define IF_DEBUG_CM if (true)
#endif

namespace rv {


CostModel::CostModel(PlatformInfo & _platInfo, Config & _config)
: platInfo(_platInfo)
, config(_config)
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

bool
CostModel::IsVectorizableFunction(Function & callee) const {
// some intrinsics are trivially vectorizable
  switch (callee.getIntrinsicID()) {
    default:
      break;

    case Intrinsic::lifetime_start:
    case Intrinsic::lifetime_end:
      return true;
  }

// in case of Vector Function ABi strings in the functions attributes assume the right version will become available at widening time
  auto attribSet = callee.getAttributes().getFnAttrs();

  for (auto attrib : attribSet) {
    if (!attrib.isStringAttribute()) continue;
    StringRef attribText = attrib.getKindAsString();

    VectorMapping dummy;
    if (parseVectorMapping(callee, attribText, dummy,  false /* createMissingDecl */)) {
      return true;
    }
  }

// no ad-hoc answer available
  return false;
}


size_t
CostModel::pickWidthForInstruction(const Instruction & inst, size_t maxWidth) const {
  if (!needsReplication(inst)) return maxWidth; // remains scalar

// check call mappings, critical sections
  auto * call = dyn_cast<CallInst>(&inst);
  if (call) {
    auto * callee = call->getCalledFunction();
    if (!callee) return 1;

    // check if this is a critical section
    if (IsCriticalSection(*callee)) return maxWidth;

    // can we vectorize the callee recursively
    if (!callee->isDeclaration() && config.enableGreedyIPV) return maxWidth; // everything is possible with IPV..

    // skip trivial LLVM intrinsics
    if (IsVectorizableFunction(*callee)) return maxWidth;

    // Otw, default to the FunctionResolver API (under pessimistic assumptions)
    VectorShapeVec topArgVec;
    for (auto &Arg : call->args()) {
      (void) Arg;
      // botArgVec.push_back(VectorShape::undef()); // FIXME this causes divergence in the VA
      topArgVec.push_back(VectorShape::varying());
    }

    // find widest available implementation
    size_t sampleWidth = maxWidth;
    StringRef calleeName = callee->getName();
    for (; sampleWidth > 1; sampleWidth /= 2) {

      // if (platInfo.getMappingsForCall(matchVec, *callee, botArgVec, sampleWidth, needsPredication)) break; // FIXME deprecated
      const bool needsPredicate = false; // FIXME
      if (platInfo.getResolver(calleeName, *callee->getFunctionType(), topArgVec, sampleWidth, needsPredicate)) {
        break;
      }
    }

    IF_DEBUG_CM {
      errs() << "cm: max width for " << calleeName << " is " << sampleWidth << "\n";
    }
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
  size_t rawSize = type.getPrimitiveSizeInBits();
  if ((rawSize > 0) &&
       (type.isIntOrIntVectorTy() || type.isFPOrFPVectorTy()))
  {
    maxWidth = std::min<size_t>(maxWidth, platInfo.getMaxVectorBits() / rawSize);
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
  size_t width = std::min(maxWidth, platInfo.getMaxVectorBits());

  IF_DEBUG_CM { errs() << "cm: bounding vector width for region " << region.str() << ", initial max width " << width << "\n"; }

  region.for_blocks([&](const BasicBlock & block) {
      width = pickWidthForBlock(block, width);
      return width > 1;
  });

  return width;
}


}
