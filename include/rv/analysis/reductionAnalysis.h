#ifndef RV_REDUCTIONANALYSIS_H
#define RV_REDUCTIONANALYSIS_H

#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/Constant.h>
#include <llvm/Transforms/Utils/ValueMapper.h>

#include "rv/vectorShape.h"

#include <map>

namespace rv {


// models a primitive value reduction of the form
// @phi [@initInputIndex, V]. [@loopInputIndex, @reductInst]
// where @reductInst has two operands: @phi and @reductInput
class Reduction {
public:
  // neutral element of this reduction operation
  llvm::Constant & neutralElem;
  // instruction feeding into the reduction phi
  llvm::Instruction & reductorInst;
  // reduction loop that contains the header phi
  llvm::Loop & redLoop;

// reduction phi
  llvm::PHINode & phi;
  int initInputIndex;
  int loopInputIndex;

  llvm::Instruction & getReductor() {
    return reductorInst;
  }

  llvm::Value & getInitValue() {
    return *phi.getIncomingValue(initInputIndex);
  }

  // operand index of @phi in @reductorInst
  int getReductorPhiIndex() {
    int phiIdx = 1;
    if (reductorInst.getOperand(0) == &phi) {
      phiIdx = 0;
    }
    assert(reductorInst.getOperand(phiIdx) == &phi && "invalid reduction");
    return phiIdx;
  }

  // value being reduced by @reductorInst into @phi
  llvm::Value & getReducibleValue() {
    int phiIdx = getReductorPhiIndex();
    return *reductorInst.getOperand(1 - phiIdx);
  }

  void dump() const;

  rv::VectorShape
  getShape(int vectorWidth);

  Reduction(llvm::Constant & _neutralElem, llvm::Instruction & _reductorInst, llvm::Loop & _reductLoop, llvm::PHINode & _phi, int _initInputIndex, int _loopInputIndex)
  : neutralElem(_neutralElem)
  , reductorInst(_reductorInst)
  , redLoop(_reductLoop)
  , phi(_phi)
  , initInputIndex(_initInputIndex)
  , loopInputIndex(_loopInputIndex)
  {}
};


class ReductionAnalysis {
  std::map<llvm::Instruction*, Reduction*> reductMap;

  const llvm::LoopInfo & loopInfo;

  void analyze(llvm::Loop & loop);
  llvm::Constant * inferNeutralElement(llvm::Instruction & reductInst);
  Reduction* tryInferReduction(llvm::PHINode & headerPhi);

public:
  ReductionAnalysis(llvm::Function & _func, const llvm::LoopInfo & _loopInfo);
  ~ReductionAnalysis();

  void analyze();

  // create reduction for the clones as well
  void updateForClones(llvm::LoopInfo & LI, llvm::ValueToValueMapTy & cloneMap);

  // look up a reduction by its constituent
  Reduction * getReductionInfo(llvm::Instruction & reductor) const;
};


} // namespace rv



#endif // RV_REDUCTIONANALYSIS_H
