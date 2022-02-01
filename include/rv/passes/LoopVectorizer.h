//===- rv/passes/loopVectorizer.h - loop vectorizer pass  --*- C++ -*-===//
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
#include "rv/transform/remTransform.h"
#include "rv/rv.h"
#include "rv/legacy/passes.h"
#include "rv/passes/PassManagerSession.h"

namespace llvm {
  class OptimizationRemarkEmitter;
  class Loop;
  class LoopInfo;
  class DominatorTree;
  class ScalarEvolution;
  class PostDominatorTree;
  class MemoryDependenceResults;
  class BranchProbabilityInfo;
  class LoopDependenceInfo;
}


namespace rv {

class LoopVectorizer {
public:
  LoopVectorizer(llvm::Function &F, llvm::TargetTransformInfo &TTI,
                 llvm::TargetLibraryInfo &TLI,
                 llvm::OptimizationRemarkEmitter &ORE);

  bool run();

private:
  Config RVConfig;
  llvm::Function &F;

  // Pass objects from PM this pass is inserted in.
  llvm::TargetTransformInfo &PassTTI;
  llvm::TargetLibraryInfo &PassTLI;
  llvm::OptimizationRemarkEmitter &PassORE;

  bool enableDiagOutput;
  bool introduced;

  // cost estimation
  struct LoopScore {
    LoopScore(bool HasSIMDAnnotation = false)
        : Score(0), HasSIMDAnnotation(HasSIMDAnnotation) {}
    unsigned Score;
    bool HasSIMDAnnotation;
  };

  struct LoopJob {
    LoopJob()
    : Header(nullptr)
    , VectorWidth(0)
    , DepDist(0)
    , TripAlign(0)
    {}

    llvm::BasicBlock *Header;
    unsigned VectorWidth;

    iter_t DepDist; // minimal dependence distance between loop iterations
    iter_t TripAlign; // multiple of loop trip count
  };

  /// \return true if legal (in that case LJ&LS get populated)
  bool scoreLoop(LoopJob& LJ, LoopScore& LS, llvm::Loop & L);

  // Step 1: Decide which loops to vectorize.
  // Step 2: Prepare all loops for vectorization.
  // Step 3: Vectorize the regions.
  bool collectLoopJobs(llvm::LoopInfo & LI);
  std::vector<LoopJob> LoopsToPrepare;
  bool prepareLoopVectorization(); // TODO mark this function as un-vectorizable (or prepare a holdout copy)

  struct LoopVectorizerJob {
    LoopJob LJ;
    ValueSet uniOverrides;
    llvm::Value *EntryAVL; // loop entry AVL (FIXME)
  };
  std::vector<LoopVectorizerJob> LoopsToVectorize;
  bool vectorizeLoopRegions();
  bool vectorizeLoop(LoopVectorizerJob& LVJob);

  PassManagerSession PMS;
  std::unique_ptr<VectorizerInterface> vectorizer;

  bool canVectorizeLoop(llvm::Loop &L);
  bool hasVectorizableLoopStructure(llvm::Loop &L, bool EmitRemarks);

  // convert L into a vectorizable loop
  // this will create a new scalar loop that can be vectorized directly with RV
  PreparedLoop transformToVectorizableLoop(llvm::Loop &L, int VectorWidth, int tripAlign, ValueSet & uniformOverrides);

  bool canAdjustTripCount(llvm::Loop &L, int VectorWidth, int TripCount);

  // return the trip count of L if it is constant. Otw, returns -1
  int getTripCount(llvm::Loop &L);

  // the trip count of the loop is always a multiple of this value
  // returns 1 for loop w/o known alignment
  int getTripAlignment(llvm::Loop & L);

  // vectorize all loops
  bool runLoopJobs();

  // Emit an optimization remark
  void remark(const llvm::StringRef OREMsg, const llvm::StringRef ORETag,
              llvm::Loop &TheLoop, llvm::Instruction *I = nullptr) const;
  void remarkMiss(const llvm::StringRef OREMsg, const llvm::StringRef ORETag,
                  llvm::Loop &TheLoop, llvm::Instruction *I = nullptr) const;
  //  void remarkAnalysis(const llvm::StringRef OREMsg, const llvm::StringRef ORETag,
  //              llvm::Loop &TheLoop) const;
};

class LoopVectorizerLegacyPass : public llvm::FunctionPass {
public:
  static char ID;
  LoopVectorizerLegacyPass() : llvm::FunctionPass(ID) {}

  bool runOnFunction(llvm::Function &F) override;

  /// Register all analyses and transformation required.
  void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;
};

struct LoopVectorizerWrapperPass
    : public llvm::PassInfoMixin<LoopVectorizerWrapperPass> {
public:
  LoopVectorizerWrapperPass(){};

  static llvm::StringRef name() { return "rv::LoopVectorizer"; }
  llvm::PreservedAnalyses run(llvm::Function &F,
                              llvm::FunctionAnalysisManager &FAM);
};

} // namespace rv
