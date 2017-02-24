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
  const LoopInfo& mLoopInfo;
  const CDG& mCDG;

public:
  MandatoryAnalysis(VectorizationInfo& vInfo, const LoopInfo& LoopInfo, const CDG& cdg);

  void analyze(Function& F);

private:
  void markDivergentLoopLatchesMandatory();
  void markLoopLatchesRecursively(const Loop* loop);
  void markDependentLoopExitsMandatory(const BasicBlock* BB);
};

}

#endif //RV_MANDATORYANALYSIS_H
