#ifndef RV_TRANSFORM_MASKEXPANDER_H
#define RV_TRANSFORM_MASKEXPANDER_H




#include "rv/vectorizationInfo.h"
#include "llvm/Analysis/PostDominators.h"
#include "rv/analysis/DFG.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/CFG.h"
#include "llvm/Analysis/LoopInfo.h"

#include <vector>
#include <map>



namespace rv {

using llvm::BasicBlock;
using llvm::Value;
using llvm::TerminatorInst;

using IndexSet = llvm::SmallVector<int, 4>;
using EdgeVec = SmallVector<Loop::Edge, 4>;


class MaskExpander {
  VectorizationInfo & vecInfo;
  const llvm::DominatorTree & domTree;
  const llvm::PostDominatorTree & postDomTree;
  const llvm::LoopInfo & loopInfo;

  llvm::Type * boolTy;
  llvm::ConstantInt * trueConst;
  llvm::ConstantInt * falseConst;

  struct EdgePred {
    Value * edgeMask; // complete edge predicate
    Value * branchMask; // branch condition (w/o block predicate)
    EdgePred() : edgeMask(nullptr), branchMask(nullptr) {}
  };

  std::map<const BasicBlock*, Value*> blockMasks;
  std::map<const BasicBlock*, std::vector<EdgePred>> edgeMasks;

  // loopPhis that need patching after all acyclic masks have been generated
  std::map<const Loop*, PHINode*> loopPhis;
  void setLoopLiveMask(const Loop& loop, PHINode& phi) {
    loopPhis[&loop] = &phi;
  }

  // request a mask for this loop and its child loops
  // returns the outer most loop this loop exits to
  llvm::Loop* requestLoopMasks(llvm::Loop & loop);


  // attach entry masks to loops (post step)
  void patchLoopMasks();

  void gatherLoopDivergenceInfo(Loop & loop, VectorShape & oLoopDivergenceShape, EdgeVec & oDivergentExits, EdgeVec & oKillExits);
public:
// lazy mask creation
  // request the block-local branch predicate
  Value & requestBranchMask(TerminatorInst & term, int succIdx, IRBuilder<> & builder);

  // live mask of this block
  llvm::Value & requestBlockMask(llvm::BasicBlock & BB);

  // live mask on this edge
  llvm::Value & requestJoinedEdgeMask(llvm::TerminatorInst & term, IndexSet succIdx);

  // live mask on this edge (given that the destinations live mask holds)
  llvm::Value & requestEdgeMask(llvm::TerminatorInst & term, int succIdx);
  llvm::Value & requestEdgeMask(llvm::BasicBlock & source, BasicBlock & dest);

  // the successor indices of termInst that BB post-dominates
  void getPredecessorEdges(const llvm::TerminatorInst & termInst, const BasicBlock & BB, IndexSet & oPredIndices) const;

// direct mask manipulation
  void setBlockMask(BasicBlock & BB, Value & mask) { blockMasks[&BB] = &mask; }
  void setEdgeMask(BasicBlock & BB, int succIdx, Value & mask); //  { edgeMasks[&BB][succIdx].edgeMask = &mask; }
  void setBranchMask(BasicBlock & BB, int succIdx, Value & mask); // { edgeMasks[&BB][succIdx].branchMask = &mask; }

  Value* getEdgeMask(const BasicBlock & begin, const BasicBlock & end) const;

  Value* getBlockMask(const BasicBlock & BB) const {
    auto it = blockMasks.find(&BB);
    if (it != blockMasks.end()) {
      return it->second;
    } else {
      return nullptr;
    }
  }

  Value* getEdgeMask(const TerminatorInst & branch, int succIdx) const {
    auto * branchBlock = branch.getParent();
    auto it = edgeMasks.find(branchBlock);
    if (it == edgeMasks.end()) return nullptr;
    auto & edgeVec = it->second;
    return edgeVec[succIdx].edgeMask;
  }

  Value* getBranchMask(const TerminatorInst & branch, int succIdx) const {
    auto * branchBlock = branch.getParent();
    auto it = edgeMasks.find(branchBlock);
    if (it == edgeMasks.end()) return nullptr;
    auto & edgeVec = it->second;
    return edgeVec[succIdx].branchMask;
  }

  // expand all masks in the region
  void expandRegionMasks();

  MaskExpander(VectorizationInfo & _vecInfo, const DominatorTree & _domTree, const llvm::PostDominatorTree & _postDomTree, const llvm::LoopInfo & _loopInfo);
  ~MaskExpander();
};



} // namespace rv

#endif // RV_TRANSFORM_MASKEXPANDER_H
