//===- rv/transform/maskExpander.h - IR generator for edge and block predicates
//--*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#ifndef RV_TRANSFORM_MASKEXPANDER_H
#define RV_TRANSFORM_MASKEXPANDER_H

#include "rv/vectorizationInfo.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/PassManager.h"

#include <map>
#include <vector>

namespace rv {

using IndexSet = llvm::SmallVector<int, 4>;
using EdgeVec = llvm::SmallVector<llvm::Loop::Edge, 4>;

class MaskExpander {
  VectorizationInfo & vecInfo;
  llvm::FunctionAnalysisManager & FAM;
  const llvm::LoopInfo & loopInfo; // invariant

  llvm::Type *avlTy;
  llvm::Type *boolTy;
  llvm::ConstantInt *trueConst;
  llvm::ConstantInt *falseConst;

  struct EdgePred {
    llvm::Optional<Mask> edgeMask;   // global edge predicate
    llvm::Optional<Mask> branchMask; // branch condition (w/o block predicate)
    EdgePred() : edgeMask(llvm::None), branchMask(llvm::None) {}
  };

  std::map<const llvm::BasicBlock *, Mask> blockMasks;
  std::map<std::pair<const llvm::BasicBlock *, int>, EdgePred> edgeMasks;

  EdgePred *getEdgePred(const llvm::BasicBlock &srcBlock, int succIdx);
  EdgePred &requestEdgePred(const llvm::BasicBlock &srcBlock, int succIdx);

public:
  // lazy mask creation
  // request the block-local branch predicate
  Mask &requestBranchMask(llvm::Instruction &term, int succIdx,
                          llvm::IRBuilder<> &builder);

  // live mask of this block
  Mask &requestBlockMask(llvm::BasicBlock &BB);

  // live mask on this edge
  Mask &requestJoinedEdgeMask(llvm::Instruction &term, IndexSet succIdx);

  // live mask on this edge (given that the destinations live mask holds)
  Mask &requestEdgeMask(llvm::Instruction &term, int succIdx);
  Mask &requestEdgeMask(llvm::BasicBlock &source, llvm::BasicBlock &dest);

  // the successor indices of termInst that BB post-dominates
  void getPredecessorEdges(const llvm::Instruction &termInst,
                           const llvm::BasicBlock &BB,
                           IndexSet &oPredIndices) const;

  // direct mask manipulation
  Mask &setBlockMask(llvm::BasicBlock &BB, Mask mask) {
    blockMasks[&BB] = mask;
    return blockMasks[&BB];
  }
  Mask &setEdgeMask(llvm::BasicBlock &BB, int succIdx, Mask mask);
  Mask &setBranchMask(llvm::BasicBlock &BB, int succIdx, Mask mask);

  Mask *getEdgeMask(const llvm::BasicBlock &begin, const llvm::BasicBlock &end);

  Mask *getBlockMask(const llvm::BasicBlock &BB) {
    auto It = blockMasks.find(&BB);
    if (It != blockMasks.end()) {
      return &It->second;
    } else {
      return nullptr;
    }
  }

  Mask *getEdgeMask(const llvm::Instruction &branch, int succIdx) {
    auto *edgePred = getEdgePred(*branch.getParent(), succIdx);
    if (!edgePred || !edgePred->edgeMask.hasValue())
      return nullptr;
    return &edgePred->edgeMask.getValue();
  }

  Mask *getBranchMask(const llvm::Instruction &branch, int succIdx) {
    auto *edgePred = getEdgePred(*branch.getParent(), succIdx);
    if (!edgePred || !edgePred->branchMask.hasValue())
      return nullptr;
    return &edgePred->branchMask.getValue();
  }

  // expand all masks in the region
  void expandRegionMasks();

  MaskExpander(VectorizationInfo & _vecInfo,
               llvm::FunctionAnalysisManager &FAM);
  ~MaskExpander();
};

} // namespace rv

#endif // RV_TRANSFORM_MASKEXPANDER_H
