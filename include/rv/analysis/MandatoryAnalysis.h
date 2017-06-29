//
// Created by tkloessner on 24.02.17.
//

#ifndef RV_MANDATORYANALYSIS_H
#define RV_MANDATORYANALYSIS_H

#include <llvm/Analysis/LoopInfo.h>
#include "../vectorizationInfo.h"
#include "DFG.h"

namespace rv {

class MandatoryAnalysis {
private:
  VectorizationInfo& mVecInfo;
  const llvm::LoopInfo& mLoopInfo;
  const llvm::CDG& mCDG;

public:
  MandatoryAnalysis(VectorizationInfo& vInfo, const llvm::LoopInfo& LoopInfo, const llvm::CDG& cdg);

  void analyze(llvm::Function& F);

private:
  void markDivergentLoopLatchesMandatory();
  void markLoopLatchesRecursively(const llvm::Loop* loop);
  void markDependentLoopExitsMandatory(const llvm::BasicBlock* BB);
};

}

#endif //RV_MANDATORYANALYSIS_H
