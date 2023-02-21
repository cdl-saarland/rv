//===- rv/transform/maskExpander.h - IR generator for edge and block predicates  --*- C++ -*-===//
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
#include "llvm/Analysis/PostDominators.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/CFG.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/IR/PassManager.h"

#include <vector>
#include <map>



namespace rv {

using IndexSet = llvm::SmallVector<int, 4>;
using EdgeVec = llvm::SmallVector<llvm::Loop::Edge, 4>;


class MaskExpander {
  VectorizationInfo & vecInfo;
  llvm::FunctionAnalysisManager & FAM;
  const llvm::LoopInfo & loopInfo; // invariant

  llvm::Type * boolTy;
  llvm::ConstantInt * trueConst;
  llvm::ConstantInt * falseConst;

  struct EdgePred {
    llvm::Value * edgeMask; // complete edge predicate
    llvm::Value * branchMask; // branch condition (w/o block predicate)
    EdgePred() : edgeMask(nullptr), branchMask(nullptr) {}
  };

  std::map<const llvm::BasicBlock*, llvm::Value*> blockMasks;
  std::map<std::pair<const llvm::BasicBlock*, int>, EdgePred> edgeMasks;

  const EdgePred* getEdgePred(const llvm::BasicBlock & srcBlock, int succIdx) const;
  EdgePred & requestEdgePred(const llvm::BasicBlock & srcBlock, int succIdx);
public:
// lazy mask creation
  // request the block-local branch predicate
  llvm::Value & requestBranchMask(llvm::Instruction & term,
                                  int succIdx,
                                  llvm::IRBuilder<> & builder);

  // live mask of this block
  llvm::Value & requestBlockMask(llvm::BasicBlock & BB);

  // live mask on this edge
  llvm::Value & requestJoinedEdgeMask(llvm::Instruction & term, IndexSet succIdx);

  // live mask on this edge (given that the destinations live mask holds)
  llvm::Value & requestEdgeMask(llvm::Instruction & term, int succIdx);
  llvm::Value & requestEdgeMask(llvm::BasicBlock & source, llvm::BasicBlock & dest);

  // the successor indices of termInst that BB post-dominates
  void getPredecessorEdges(const llvm::Instruction & termInst,
                           const llvm::BasicBlock & BB,
                           IndexSet & oPredIndices) const;

// direct mask manipulation
  void setBlockMask(llvm::BasicBlock & BB, llvm::Value & mask) { blockMasks[&BB] = &mask; }
  void setEdgeMask(llvm::BasicBlock & BB, int succIdx, llvm::Value & mask); //  { edgeMasks[&BB][succIdx].edgeMask = &mask; }
  void setBranchMask(llvm::BasicBlock & BB, int succIdx, llvm::Value & mask); // { edgeMasks[&BB][succIdx].branchMask = &mask; }

  llvm::Value* getEdgeMask(const llvm::BasicBlock & begin, const llvm::BasicBlock & end) const;

  llvm::Value* getBlockMask(const llvm::BasicBlock & BB) const {
    auto it = blockMasks.find(&BB);
    if (it != blockMasks.end()) {
      return it->second;
    } else {
      return nullptr;
    }
  }

  llvm::Value* getEdgeMask(const llvm::Instruction & branch, int succIdx) const {
    auto * edgePred = getEdgePred(*branch.getParent(), succIdx);
    if (!edgePred) return nullptr;
    return edgePred->edgeMask;
  }

  llvm::Value* getBranchMask(const llvm::Instruction & branch, int succIdx) const {
    auto * edgePred = getEdgePred(*branch.getParent(), succIdx);
    if (!edgePred) return nullptr;
    return edgePred->branchMask;
  }

  // expand all masks in the region
  void expandRegionMasks();

  MaskExpander(VectorizationInfo & _vecInfo,
               llvm::FunctionAnalysisManager &FAM);
  ~MaskExpander();
};



} // namespace rv

#endif // RV_TRANSFORM_MASKEXPANDER_H
