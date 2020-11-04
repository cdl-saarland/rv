//===- rv/transform/loopVectorizer.h - loop vectorizer pass  --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//



#include "llvm/Pass.h"

#include "llvm/Transforms/Utils/ValueMapper.h"
#include "rv/transform/remTransform.h"
#include "rv/analysis/reductionAnalysis.h"
#include "rv/analysis/loopAnnotations.h"
#include "rv/config.h"
#include "llvm/IR/PassManager.h"
#include "rv/rv.h"
#include "rv/passes.h"

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

class LoopVectorizer : public llvm::FunctionPass {
public:
  static char ID;
  LoopVectorizer()
  : llvm::FunctionPass(ID)
  , config()
  , enableDiagOutput(false)
  , introduced(false)
  , DT(nullptr)
  , PDT(nullptr)
  , LI(nullptr)
  , SE(nullptr)
  , MDR(nullptr)
  , PB(nullptr)
  , reda()
  , vectorizer()
  {}

  bool runOnFunction(llvm::Function &F) override;

  /// Register all analyses and transformation required.
  void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;

private:
  Config config;

  bool enableDiagOutput;
  bool introduced;

  llvm::Function * F;
  llvm::FunctionAnalysisManager FAM; // private pass infrastructure
  llvm::DominatorTree * DT;
  llvm::PostDominatorTree * PDT;
  llvm::LoopInfo * LI;
  llvm::ScalarEvolution * SE;
  llvm::MemoryDependenceResults * MDR;
  llvm::BranchProbabilityInfo * PB;
  std::unique_ptr<ReductionAnalysis> reda;
  std::unique_ptr<VectorizerInterface> vectorizer;

  bool canVectorizeLoop(llvm::Loop &L);

  // convert L into a vectorizable loop
  // this will create a new scalar loop that can be vectorized directly with RV
  llvm::Loop* transformToVectorizableLoop(llvm::Loop &L, int VectorWidth, int tripAlign, ValueSet & uniformOverrides);

  bool canAdjustTripCount(llvm::Loop &L, int VectorWidth, int TripCount);

  // return the trip count of L if it is constant. Otw, returns -1
  int getTripCount(llvm::Loop &L);

  // the trip count of the loop is always a multiple of this value
  // returns 1 for loop w/o known alignment
  int getTripAlignment(llvm::Loop & L);

  bool vectorizeLoop(llvm::Loop &L);
  bool vectorizeLoopOrSubLoops(llvm::Loop &L);
};

struct LoopVectorizerWrapperPass : llvm::PassInfoMixin<LoopVectorizerWrapperPass> {
  private:
    std::shared_ptr<llvm::FunctionPass> loopvec;
  public:
    LoopVectorizerWrapperPass() : loopvec(rv::createLoopVectorizerPass()) {};

    llvm::PreservedAnalyses run (llvm::Function &F, llvm::FunctionAnalysisManager &FAM) {
      if (loopvec->runOnFunction(F))
        return llvm::PreservedAnalyses::none();
      else
        return llvm::PreservedAnalyses::all();
    }
};

} // namespace rv
