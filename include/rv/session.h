#ifndef RV_SESSION_H
#define RV_SESSION_H

#include "rv/PlatformInfo.h"
#include "rv/optConfig.h"
#include "rv/vectorizationInfo.h"

#include <llvm/Transforms/Utils/ValueMapper.h>

namespace llvm {
  class DominatorTree;
  class PostDominatorTree;
  class LoopInfo;
  class ScalarEvolution;
  class MemoryDependenceResults;

  class BranchProbabilityInfo;
}



namespace rv {


class Session {
public:
  PlatformInfo & platInfo;
  llvm::DominatorTree & domTree;
  llvm::PostDominatorTree & postDomTree;
  llvm::LoopInfo & loopInfo;
  llvm::ScalarEvolution & scev;
  llvm::MemoryDependenceResults & MDR;

private:
  llvm::BranchProbabilityInfo * pbInfo;

public:
  Session(PlatformInfo & _platInfo,
          llvm::DominatorTree & domTree,
          llvm::PostDominatorTree & postDomTree,
          llvm::LoopInfo & loopInfo,
          llvm::ScalarEvolution & scev,
          llvm::MemoryDependenceResults & _MDR,
          llvm::BranchProbabilityInfo * _pbInfo /* optional */);

  ~Session();

  // add branch probabilityes for the scalar source function (required for BOSCC)
  void addBranchProbabilities(llvm::BranchProbabilityInfo * _pbInfo) { pbInfo = _pbInfo; }
  llvm::BranchProbabilityInfo * getBranchProbabilities() const { return pbInfo; }

  // analyze, linearizer & vectorize
  void run(VectorizationInfo & vecInfo, OptConfig optConfig = OptConfig(), llvm::ValueToValueMapTy * vecInstMap = nullptr);
};

void lowerComplexMath(VectorizationInfo & vecInfo, llvm::LoopInfo & loopInfo);


}

#endif // RV_SESSION_H
