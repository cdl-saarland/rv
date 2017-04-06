#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/Constant.h>

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
  llvm::Value & redInput;
  // reduction loop that contains the header phi
  llvm::Loop & redLoop;

// reduction phi
  llvm::PHINode & phi;
  int initInputIndex;
  int loopInputIndex;

  llvm::Instruction & getReductInst() {
    return llvm::cast<llvm::Instruction>(*phi.getIncomingValue(loopInputIndex));
  }

  llvm::Value & getInitValue() {
    return *phi.getIncomingValue(initInputIndex);
  }

  void dump() const;

  Reduction(llvm::Constant & _neutralElem, llvm::Value & _reductInput, llvm::Loop & _reductLoop, llvm::PHINode & _phi, int _initInputIndex, int _loopInputIndex)
  : neutralElem(_neutralElem)
  , redInput(_reductInput)
  , redLoop(_reductLoop)
  , phi(_phi)
  , initInputIndex(_initInputIndex)
  , loopInputIndex(_loopInputIndex)
  {}
};


class ReductionAnalysis {
  std::map<llvm::PHINode*, Reduction*> reductMap;

  const llvm::LoopInfo & loopInfo;

  void analyze(llvm::Loop & loop);
  llvm::Constant * inferNeutralElement(llvm::Instruction & reductInst);
  Reduction* tryInferReduction(llvm::PHINode & headerPhi);

public:
  ReductionAnalysis(llvm::Function & _func, const llvm::LoopInfo & _loopInfo);
  ~ReductionAnalysis();

  void analyze();

  Reduction * getReductionInfo(llvm::PHINode & phi) const;
};


} // namespace rv
