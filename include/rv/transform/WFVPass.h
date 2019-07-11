//===- rv/WFVPass.h - Vectorize whole functions  ----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//
//===----------------------------------------------------------------------===//

#ifndef RV_TRANSFORM_WFVPASS_H
#define RV_TRANSFORM_WFVPASS_H

#include "llvm/Pass.h"

#include "llvm/ADT/DenseSet.h"

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
  struct PostDominatorTree;
  class MemoryDependenceResults;
  class BranchProbabilityInfo;
}


namespace rv {

struct VectorMapping;
class VectorizationInfo;
class VectorizerInterface;
class PlatformInfo;

class WFVPass : public llvm::ModulePass {
  bool enableDiagOutput; // WFV_DIAG

  std::vector<VectorMapping> wfvJobs;

  /// collect all stray Vector Function ABI strings in the attributes of \p F.
  void collectJobs(llvm::Function & F);

  /// check that \p wfvJob is a sane function mapping.
  bool isSaneMapping(VectorMapping & wfvJob) const;

  /// generate the Vector Function ABI variant encoded in \p wfvJob.
  void vectorizeFunction(VectorizerInterface & vectorizer, VectorMapping & wfvJob);
public:
  static char ID;

  WFVPass()
  : ModulePass(ID)
  {}

  void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;
  bool runOnModule(llvm::Module & M) override;
};

} // namespace rv
#endif // RV_TRANSFORM_WFVPASS_H
