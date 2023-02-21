//===- rv/transform/remTransform.h - scalar remainder-loop generator --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#ifndef RV_TRANSFORM_REMTRANSFORM_H
#define RV_TRANSFORM_REMTRANSFORM_H

#include "llvm/Analysis/BranchProbabilityInfo.h"
#include "llvm/IR/Function.h"

#include <set>

namespace llvm {
  class LoopInfo;
  class Loop;
  class DominatorTree;
  class PostDominatorTree;
  class BranchProbabilityInfo;
}



using ValueSet = std::set<llvm::Value*>;

namespace rv {

class BranchCondition;
class ReductionAnalysis;
class VectorizationInfo;

struct PreparedLoop {
  llvm::Loop* TheLoop;
  llvm::Value * EntryAVL;
  PreparedLoop() : TheLoop(nullptr), EntryAVL(nullptr) {}
  PreparedLoop(llvm::Loop *_TheLoop, llvm::Value *_EntryAVL)
      : TheLoop(_TheLoop), EntryAVL(_EntryAVL) {}
};

class RemainderTransform {
  llvm::Function & F;
  llvm::FunctionAnalysisManager &FAM;
  llvm::DominatorTree & DT;
  llvm::PostDominatorTree & PDT;
  llvm::LoopInfo & LI;
  llvm::BranchProbabilityInfo &PBI;
  ReductionAnalysis & reda;


// RemainderTransform capability checks
  // check if remTrans currently handles the loop exit condition
  BranchCondition* analyzeExitCondition(llvm::Loop & L, int vectorWidth);

  // if this returns true RemainderTransform must not fail during the transformation and has to return a vectorizable loop
  bool canTransformLoop(llvm::Loop & L);

public:
  RemainderTransform(llvm::Function &_F, llvm::FunctionAnalysisManager & FAM, ReductionAnalysis & _reda);

  // create a vectorizable loop or return nullptr if remTrans can not currently do it
  PreparedLoop
  createVectorizableLoop(llvm::Loop & L, ValueSet & uniOverrides, bool useTailPredication, int vectorWidth, int tripAlign);

  // Check whether ::createVectorizableLoop will succeed on \p L.
  bool analyzeLoopStructure(llvm::Loop &L);
};

}

#endif // RV_TRANSFORM_REMTRANSFORM_H
