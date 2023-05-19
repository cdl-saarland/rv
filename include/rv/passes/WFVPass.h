//===- rv/transform/WFVPass.h - whole-function vectorizer pass  --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#ifndef RV_TRANSFORM_WFVPASS_H
#define RV_TRANSFORM_WFVPASS_H

#include "llvm/Pass.h"

#include "llvm/ADT/DenseSet.h"

#include "llvm/Transforms/Utils/ValueMapper.h"
#include "rv/transform/remTransform.h"
#include "rv/passes/PassManagerSession.h"
#include "rv/config.h"
#include "rv/analysis/reductionAnalysis.h"
#include "llvm/ADT/StringRef.h"
#include "rv/legacy/passes.h"

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
  class TargetLibraryInfo;
  class TargetTransformInfo;
}


namespace rv {

struct VectorMapping;
class VectorizerInterface;

class WFV {
  PassManagerSession PMS;

  bool enableDiagOutput; // WFV_DIAG

  std::vector<VectorMapping> wfvJobs;

  /// collect all stray Vector Function ABI strings in the attributes of \p F.
  void collectJobs(llvm::Function &F);

  /// generate the Vector Function ABI variant encoded in \p wfvJob.
  void vectorizeFunction(VectorizerInterface &vectorizer,
                         VectorMapping &wfvJob);

public:
  WFV();
  bool run(llvm::Module &);
};

class WFVLegacyPass : public llvm::ModulePass {
public:
  static char ID;

  WFVLegacyPass() : ModulePass(ID) {}

  void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;
  bool runOnModule(llvm::Module &M) override;
};

struct WFVWrapperPass : llvm::PassInfoMixin<WFVWrapperPass> {
public:
  WFVWrapperPass();
  llvm::PreservedAnalyses run(llvm::Module &M,
                              llvm::ModuleAnalysisManager &MAM);
};

} // namespace rv
#endif // RV_TRANSFORM_WFVPASS_H
