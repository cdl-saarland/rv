//===- rv/LoopVectorizer.h - Vectorize Loops  ----------------===//
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

#include "llvm/Pass.h"

#include "llvm/Transforms/Utils/ValueMapper.h"
#include "rv/transform/remTransform.h"
#include "rv/config.h"
#include "rv/analysis/reductionAnalysis.h"

#include <limits>

namespace llvm {
  class Loop;
  class LoopInfo;
  class DominatorTree;
  class PostDominatorTree;
  class ScalarEvolution;
  struct PostDominatorTree;
  class MemoryDependenceResults;
  class BranchProbabilityInfo;
}


namespace rv {

class VectorizationInfo;
class VectorizerInterface;

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
  // return the annotated vector width
  // return -1, if unspecified
  int getVectorWidth(llvm::Loop &L);

  // minimal distance between dependent loop trips
  // returns ParallelDistance for fully parallel loops
  const int ParallelDistance = std::numeric_limits<int>::max();
  int getDependenceDistance(llvm::Loop & L);

  // the trip count of the loop is always a multiple of this value
  // returns 1 for loop w/o known alignment
  int getTripAlignment(llvm::Loop & L);

  bool vectorizeLoop(llvm::Loop &L);
  bool vectorizeLoopOrSubLoops(llvm::Loop &L);
};

} // namespace rv
