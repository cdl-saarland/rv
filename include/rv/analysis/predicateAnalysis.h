//===- rv/analysis/predicateAnalysis.h - divergent (trans) cdep computation --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef RV_ANALYSIS_PREDICATEANALYSIS_H
#define RV_ANALYSIS_PREDICATEANALYSIS_H

#include "rv/analysis/DFG.h"
#include <llvm/IR/Dominators.h>
#include <llvm/Analysis/PostDominators.h>
#include <llvm/ADT/ArrayRef.h>

namespace rv {
  class VectorizationInfo;

  using BlockCallbackFunc = std::function<void(const llvm::BasicBlock&)>;

// This analysis determines whether a basic block predicate will be varying
// without actually emitting the predicate computation.
class PredicateAnalysis {
  VectorizationInfo & vecInfo;
  const llvm::PostDominatorTree & PDT;

public:
  PredicateAnalysis(VectorizationInfo & _vecInfo, const llvm::PostDominatorTree & _PDT)
  : vecInfo(_vecInfo)
  , PDT(_PDT)
  {}

  /// update the analysis with a newly detected divergent branch (or divergent loop where \p rootBlock is the loop header)
  /// calls @BlockCallback on every block whose predicate turns varying as a consequence.
  /// (push mode)
  void addDivergentBranch(const llvm::BasicBlock & rootBlock, llvm::ArrayRef<const llvm::BasicBlock*> & divSuccArray, BlockCallbackFunc BlockCallback);
};


} // namespace rv

#endif // RV_ANALYSIS_PREDICATEANALYSIS_H

