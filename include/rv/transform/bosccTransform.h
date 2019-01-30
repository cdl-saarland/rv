#ifndef RV_TRANSFORM_BOSCCTRANSFORM_H
#define RV_TRANSFORM_BOSCCTRANSFORM_H

#include <llvm/IR/Value.h>
#include <llvm/Transforms/Utils/ValueMapper.h>

#include "rv/PlatformInfo.h"
#include "rv/vectorShape.h"

namespace llvm {
  class AllocaInst;
  class DataLayout;
  class LoopInfo;
  class DominatorTree;
  class PostDominatorTree;
  class BranchProbabilityInfo;
}

namespace rv {

class MaskExpander;
class VectorizationInfo;

class BOSCCTransform {
  VectorizationInfo & vecInfo;
  PlatformInfo & platInfo;
  MaskExpander & maskEx;
  llvm::DominatorTree & domTree;
  llvm::PostDominatorTree & postDomTree;
  llvm::LoopInfo & loopInfo;
  llvm::BranchProbabilityInfo * pbInfo;

public:
  BOSCCTransform(VectorizationInfo & _vecInfo, PlatformInfo & _platInfo, MaskExpander & _maskEx, llvm::DominatorTree & _domTree, llvm::PostDominatorTree & _postDomTree, llvm::LoopInfo & _loopInfo, llvm::BranchProbabilityInfo * _pbInfo);

  bool run();
};


} // namespace rv

#endif// RV_TRANSFORM_BOSCCTRANSFORM_H
