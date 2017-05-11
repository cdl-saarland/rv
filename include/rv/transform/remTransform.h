#ifndef RV_TRANSFORM_REMTRANSFORM_H
#define RV_TRANSFORM_REMTRANSFORM_H

#include "llvm/Transforms/Utils/ValueMapper.h"
#include "llvm/IR/Function.h"

namespace llvm {
  class LoopInfo;
  class Loop;
  class DominatorTree;
}

template<class T>
inline
T&
LookUp(llvm::ValueToValueMapTy & valMap, T& key) {
  return *llvm::cast<T>(valMap[&key]);
}


namespace rv {

class ReductionAnalysis;
class VectorizationInfo;

class RemainderTransform {
  llvm::Function & F;
  llvm::LoopInfo & LI;
  ReductionAnalysis & reda;

public:
  RemainderTransform(llvm::Function &_F, llvm::LoopInfo & _LI, ReductionAnalysis & _reda)
  : F(_F)
  , LI(_LI)
  , reda(_reda)
  {}

  void embedVectorLoop(llvm::Loop & L, llvm::ValueToValueMapTy & vecValMap, VectorizationInfo & vecInfo, int vectorWidth, int tripAlign);
};

}

#endif // RV_TRANSFORM_REMTRANSFORM_H
