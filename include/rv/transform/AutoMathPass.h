//===- rv/transform/AutoMathPass.h - auto-vectorize math  --*- C++ -*-===//
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

class AutoMathPass : public llvm::ModulePass {
  // Functions we are required to vectorized
  std::map<llvm::Function*, llvm::SmallSet<unsigned,2>> MathFuncsToWidth;

  // Callsites that have to be replaced
  std::vector<llvm::CallInst*> CallSites;

  // check whether a call to, suspect math function, F could be auto-vectorized
  // with an RV resolver.
  bool addMathFuncJob(llvm::Function *F, unsigned VecWidth);

  /// check whether F is eligible for auto-math and if so collect all function
  /// calls to vector math declarations.
  bool runOnFunction(llvm::Function & F);

  bool inspectCallSite(llvm::CallInst &C);

  /// generate the Vector Function ABI variant encoded in \p wfvJob.
  void vectorizeFunction(VectorizerInterface & vectorizer, VectorMapping & wfvJob);
public:
  static char ID;

  AutoMathPass()
  : ModulePass(ID)
  {}

  void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;
  bool runOnModule(llvm::Module & M) override;
};

struct AutoMathWrapperPass : llvm::PassInfoMixin<AutoMathWrapperPass> {
  private:
    std::shared_ptr<llvm::ModulePass> autoMath;
  public:
    AutoMathWrapperPass() : autoMath(rv::createAutoMathPass()) {};

    llvm::PreservedAnalyses run (llvm::Module &M, llvm::ModuleAnalysisManager &MAM) {
      if (autoMath->runOnModule(M))
        return llvm::PreservedAnalyses::none();
      else
        return llvm::PreservedAnalyses::all();
    }
};


} // namespace rv
#endif // RV_TRANSFORM_AUTOMATHPASS_H
