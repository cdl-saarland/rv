//===- rv/analysis/BranchEstimate.h - Approx. Uniform branch probabilities --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef RV_BRANCH_ESTIMATE_H
#define RV_BRANCH_ESTIMATE_H

#include "rv/PlatformInfo.h"
#include <llvm/ADT/SmallSet.h>

namespace llvm {
class LoopInfo;
class DominatorTree;
class BranchProbabilityInfo;
}

namespace rv {
class MaskExpander;
class VectorizationInfo;

class BranchEstimate {
  VectorizationInfo & vecInfo;
  PlatformInfo & platInfo;
  MaskExpander & maskEx;
  llvm::DominatorTree & domTree;
  llvm::LoopInfo & loopInfo;
  llvm::BranchProbabilityInfo * pbInfo;

public:
  BranchEstimate(VectorizationInfo & _vecInfo, PlatformInfo & _platInfo, MaskExpander & _maskEx, llvm::DominatorTree & _domTree, llvm::LoopInfo & _loopInfo, llvm::BranchProbabilityInfo * _pbInfo)
  :vecInfo(_vecInfo)
  ,platInfo(_platInfo)
  ,maskEx(_maskEx)
  ,domTree(_domTree)
  ,loopInfo(_loopInfo)
  ,pbInfo(_pbInfo)
  {}

  bool CheckLegality (llvm::BranchInst & branch, bool & onTrueLegal, bool & onFalseLegal);

  llvm::BasicBlock * getExitBlock(llvm::BasicBlock * entry);

  bool analyze(llvm::BranchInst & branch,
                double & trueRatio,
                double & falseRatio,
                size_t & onTrueScore,
                size_t & onFalseScore);

  void computeDispersion(std::map<llvm::BasicBlock*, double> & dispMap);

  double GetEdgeProb(llvm::BasicBlock & start,
                     llvm::BasicBlock & end);

  size_t getDomRegionScore(llvm::BasicBlock & entry);

  size_t getDomRegionValue(llvm::BasicBlock & entry,
                           llvm::BasicBlock & subBlock,
                           llvm::SmallSet<llvm::BasicBlock*, 32> & seenBlocks);

  size_t getBlockScore(llvm::BasicBlock & entry);
};

int GetNumPredecessors(llvm::BasicBlock & block);

} // namespace rv

#endif // RV_BRANCH_ESTIMATE_H
