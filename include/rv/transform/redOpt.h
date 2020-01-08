//===- rv/transform/redOpt.h - optimize privatized reductions chains --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#ifndef RV_REDOPT_H
#define RV_REDOPT_H

#include "llvm/IR/PassManager.h"

namespace llvm {
  class DominatorTree;
  class PHINode;
}

namespace rv {

class VectorizationInfo;
class ReductionAnalysis;
class Reduction;

class ReductionOptimization {
  VectorizationInfo & vecInfo;
  ReductionAnalysis & reda;
  llvm::DominatorTree & dt;

  bool optimize(llvm::PHINode & phi, Reduction & red);
public:
  ReductionOptimization(VectorizationInfo & vecInfo, ReductionAnalysis & reda, llvm::FunctionAnalysisManager &FAM);
  bool run();
};


}

#endif // RV_REDOPT_H
