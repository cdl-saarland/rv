#ifndef RV_TRANSFORM_LOOPCLONER_H
#define RV_TRANSFORM_LOOPCLONER_H

#include <llvm/IR/Dominators.h>
#include <llvm/Transforms/Utils/ValueMapper.h>

#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/Dominators.h>
#include <llvm/Analysis/PostDominators.h>
#include <llvm/IR/Function.h>
#include <llvm/Analysis/BranchProbabilityInfo.h>
#include <llvm/Transforms/Utils/Cloning.h>


namespace llvm {
  class LoopInfo;
  struct PostDominatorTree;
}

namespace rv {
  struct LoopCloneInfo {
    llvm::Loop & clonedLoop;
    llvm::DomTreeNode & headerDomNode;
    llvm::DomTreeNode & exitingPostDom;
  };

  LoopCloneInfo CloneLoop(llvm::Loop & L, llvm::Function & F, llvm::DominatorTree & DT, llvm::PostDominatorTree & PDT, llvm::LoopInfo & LI, llvm::BranchProbabilityInfo * PB, llvm::ValueToValueMapTy & cloneMap);
}

#endif// RV_TRANSFORM_LOOPCLONER_H
