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
  llvm::DominatorTree & DT;
  llvm::LoopInfo & LI;
  ReductionAnalysis & reda;

  bool canTransformLoop(llvm::Loop & L);

public:
  RemainderTransform(llvm::Function &_F, llvm::DominatorTree & _DT, llvm::LoopInfo & _LI, ReductionAnalysis & _reda)
  : F(_F)
  , DT(_DT)
  , LI(_LI)
  , reda(_reda)
  {}

  // create a vectorizable loop or return nullptr if that is not possible
  llvm::Loop*
  createVectorizableLoop(llvm::Loop & L, int vectorWidth, int tripAlign);
};

}

#endif // RV_TRANSFORM_REMTRANSFORM_H
