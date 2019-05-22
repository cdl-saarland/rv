#ifndef RV_BRANCH_ESTIMATE_H
#define RV_BRANCH_ESTIMATE_H

#include "rv/PlatformInfo.h"
#include <llvm/ADT/SmallSet.h>

namespace llvm {
class LoopInfo;
class DominatorTree;
class BranchProbabilityInfo;
}

namespace rv {

class VectorizationInfo;

class BranchEstimate {
  VectorizationInfo & vecInfo;
  PlatformInfo & platInfo;
  llvm::DominatorTree & domTree;
  llvm::LoopInfo & loopInfo;
  llvm::BranchProbabilityInfo * pbInfo;

public:
  BranchEstimate(VectorizationInfo & _vecInfo, PlatformInfo & _platInfo, llvm::DominatorTree & _domTree, llvm::LoopInfo & _loopInfo, llvm::BranchProbabilityInfo * _pbInfo)
  :vecInfo(_vecInfo)
  ,platInfo(_platInfo)
  ,domTree(_domTree)
  ,loopInfo(_loopInfo)
  ,pbInfo(_pbInfo)
  {}

  bool analysis(llvm::BranchInst & branch,
                double & trueRatio,
                double & falseRatio,
                size_t & onTrueScore,
                size_t & onFalseScore);

  void computeDispersion(std::map<llvm::BasicBlock*, double> & dispMap);

  double GetEdgeProb(llvm::BasicBlock & start,
                     llvm::BasicBlock & end);

  size_t getDomRegionScore(llvm::BasicBlock & entry);

  size_t getDomRegionValue(llvm::BasicBlock & entry,
                           llvm::BasicBlock & subBlock,
                           llvm::SmallSet<llvm::BasicBlock*, 32> & seenBlocks);

  size_t getBlockScore(llvm::BasicBlock & entry);
};

int GetNumPredecessors(llvm::BasicBlock & block);

} // namespace rv

#endif // RV_BRANCH_ESTIMATE_H
