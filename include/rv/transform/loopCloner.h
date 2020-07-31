//===- rv/transform/loopCloner.h - loop-nest cloning --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

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
  class PostDominatorTree;
}

namespace rv {
  struct LoopCloneInfo {
    llvm::Loop & clonedLoop;
    llvm::DomTreeNode * headerDomNode;
    llvm::DomTreeNode * exitingPostDom;
  };

  LoopCloneInfo CloneLoop(llvm::Loop & L, llvm::Function & F, llvm::FunctionAnalysisManager & FAM, llvm::ValueToValueMapTy & cloneMap);
}

#endif// RV_TRANSFORM_LOOPCLONER_H
