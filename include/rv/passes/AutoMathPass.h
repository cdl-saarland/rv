//===- rv/passes/AutoMathPass.h - auto-vectorize math  --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#ifndef RV_TRANSFORM_AUTOMATHPASS_H
#define RV_TRANSFORM_AUTOMATHPASS_H

#include "llvm/Pass.h"
#include "rv/legacy/passes.h"

#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SmallSet.h"

#include "llvm/Transforms/Utils/ValueMapper.h"
#include "rv/transform/remTransform.h"
#include "rv/passes/PassManagerSession.h"
#include "rv/config.h"
#include "rv/analysis/reductionAnalysis.h"
#include "llvm/ADT/StringRef.h"

#include <limits>
#include <vector>

namespace llvm {
  class Loop;
  class LoopInfo;
  class DominatorTree;
  class ScalarEvolution;
  class PostDominatorTree;
  class MemoryDependenceResults;
  class BranchProbabilityInfo;
}


namespace rv {

struct VectorMapping;
class VectorizationInfo;
class VectorizerInterface;
class PlatformInfo;

class AutoMathPass {
  PassManagerSession PMS;

  // Functions we are required to vectorized
  std::map<llvm::Function*, llvm::SmallSet<unsigned,2>> MathFuncsToWidth;

  // Callsites that have to be replaced
  std::vector<llvm::CallInst*> CallSites;

  // check whether a call to, suspect math function, F could be auto-vectorized
  // with an RV resolver.
  bool addMathFuncJob(llvm::Function *F, unsigned VecWidth);

  bool inspectCallSite(llvm::CallInst &C);

  /// generate the Vector Function ABI variant encoded in \p wfvJob.
  void vectorizeFunction(VectorizerInterface & vectorizer, VectorMapping & wfvJob);

  /// check whether F is eligible for auto-math and if so collect all function
  /// calls to vector math declarations.
  bool run(llvm::Function &);

public:
  AutoMathPass();
  bool run(llvm::Module & M);
};

class AutoMathLegacyPass : public llvm::ModulePass {
public:
  static char ID;

  AutoMathLegacyPass() : ModulePass(ID) {}

  void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;
  bool runOnModule(llvm::Module &M) override;
};

class AutoMathWrapperPass : public llvm::PassInfoMixin<AutoMathWrapperPass> {
public:
  AutoMathWrapperPass();

  static llvm::StringRef name() { return "rv::AutoMathWrapperPass"; }
  llvm::PreservedAnalyses run(llvm::Module &M,
                              llvm::ModuleAnalysisManager &MAM);
};

} // namespace rv
#endif // RV_TRANSFORM_AUTOMATHPASS_H
