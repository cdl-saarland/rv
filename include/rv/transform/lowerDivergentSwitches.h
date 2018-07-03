#ifndef RV_TRANSFORM_LOWERDIVERGENTSWITCHES_H
#define RV_TRANSFORM_LOWERDIVERGENTSWITCHES_H

#include <llvm/IR/Instruction.h>

namespace llvm {
  class SwitchInst;
  class LoopInfo;
}

namespace rv {

class MaskExpander;
class VectorizationInfo;

class LowerDivergentSwitches {
  VectorizationInfo & vecInfo;
  llvm::LoopInfo & LI;
  MaskExpander & maskEx;

  void lowerSwitch(llvm::SwitchInst & swInst);

public:
  LowerDivergentSwitches(VectorizationInfo & _vecInfo, llvm::LoopInfo & _LI, MaskExpander & _maskEx);
  bool run();
};

} // namespace rv


#endif // RV_TRANSFORM_LOWERDIVERGENTSWITCHES_H
