//===- rv/analysis/UndeadMaskAnalysis.h - at-least-one-thread-live analysis --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef RV_ANALYSIS_UNDEADMASKANALYSIS_H
#define RV_ANALYSIS_UNDEADMASKANALYSIS_H

#include <llvm/IR/Value.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Dominators.h>
#include "llvm/IR/PassManager.h"

#include <map>

namespace rv {

class VectorizationInfo;

// this analysis is run by the backend to determine whether there is at least a single active lean when a piece of code is executed.
// if (rv_any(p && q)) {
//   if (p) *uniformPtr = <uniform value>
// }
//
// without undead mask analysis:
// if (rv_any(p && q)) {
//   if (rv_any(p)) {
//     *uniformPtr = <uniform value>
//   }
// }
//
// with undead mask analysis:
// if (rv_any(p && q)) {
//   *uniformPtr = <uniform value>
// }
class UndeadMaskAnalysis {
  VectorizationInfo & vecInfo;
  const llvm::DominatorTree & domTree;

  // whether @lhs ^ @lhsNegated implies @rhs ^ @rhsNegated
  // (returns false if answer unknown)
  bool implies(const llvm::Value & lhs, bool lhsNegated, const llvm::Value & rhs, bool rhsNegated);
  std::map<const llvm::Value*, const llvm::BasicBlock*> liveDominatorMap;

public:
  UndeadMaskAnalysis(VectorizationInfo & vecInfo, llvm::FunctionAnalysisManager &FAM);
  bool isUndead(const llvm::Value & mask, const llvm::BasicBlock & where);
  void print(llvm::raw_ostream &);
};

} // namespace rv

#endif // RV_ANALYSIS_UNDEADMASKANALYSIS_H
