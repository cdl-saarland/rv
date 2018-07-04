#ifndef RV_TRANSFORM_LOWERDIVERGENTSWITCHES_H
#define RV_TRANSFORM_LOWERDIVERGENTSWITCHES_H

#include <llvm/IR/Instruction.h>

namespace llvm {
  class SwitchInst;
  class LoopInfo;
  class BasicBlock;
}

namespace rv {

class VectorizationInfo;

class LowerDivergentSwitches {
  VectorizationInfo & vecInfo;
  llvm::LoopInfo & LI;

  void lowerSwitch(llvm::SwitchInst & swInst);
  void replaceIncoming(llvm::BasicBlock & phiBlock, llvm::BasicBlock & oldIncoming, llvm::BasicBlock & newIncoming);

public:
  LowerDivergentSwitches(VectorizationInfo & _vecInfo, llvm::LoopInfo & _LI);
  bool run();
};

} // namespace rv


#endif // RV_TRANSFORM_LOWERDIVERGENTSWITCHES_H
