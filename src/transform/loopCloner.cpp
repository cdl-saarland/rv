#include "rv/transform/loopCloner.h"
#include "rv/utils.h"

#include <llvm/IR/Constants.h>

using namespace llvm;
namespace rv {

struct LoopCloner {
  Function & F;
  DominatorTree & DT;
  PostDominatorTree & PDT;
  LoopInfo & LI;
  BranchProbabilityInfo * PB;

  LoopCloner(Function & _F, DominatorTree & _DT, PostDominatorTree & _PDT, LoopInfo & _LI, BranchProbabilityInfo * _PB)
  : F(_F)
  , DT(_DT)
  , PDT(_PDT)
  , LI(_LI)
  , PB(_PB)
  {}

  // TODO pretend that the new loop is inserted
  // LoopInfo and DomTree will be updated as-if the preheader branches to both the original and the sclar loop
  // Note that this will not repair the analysis structures beyond the exits edges

  LoopCloneInfo
  CloneLoop(Loop & L, ValueToValueMapTy & valueMap) {
    auto * loopPreHead = L.getLoopPreheader();
    auto * preTerm = loopPreHead->getTerminator();
    auto & loopHead = *L.getHeader();
    auto * loopExiting = L.getExitingBlock();
    assert(loopPreHead && loopExiting && " can only clone single exit loops");

    auto * splitBranch = BranchInst::Create(&loopHead, &loopHead, ConstantInt::getTrue(loopHead.getContext()), loopPreHead);

    // clone all basic blocks
    CloneLoopBlocks(L, valueMap);

    // on false branch to the copy
    splitBranch->setSuccessor(1, &LookUp(valueMap, loopHead));

    // the same in both worlds (needed by idom repair)
    valueMap[loopPreHead] = loopPreHead;

    auto & clonedHead = LookUp(valueMap, loopHead);
    auto & clonedExiting = LookUp(valueMap, *loopExiting);

    // repair LoopInfo & DomTree
    CloneLoopAnalyses(L.getParentLoop(), L, valueMap);
    auto * clonedLoop = LI.getLoopFor(&clonedHead);
    assert(clonedLoop);

    // repair the dom tree
    CloneDomTree(*loopPreHead, L, loopHead, valueMap);
    auto * clonedDomNode = DT.getNode(&clonedHead);
    assert(clonedDomNode);

    auto * loopPostDom = PDT.getNode(loopExiting)->getIDom()->getBlock();
    ClonePostDomTree(*loopPostDom, L, *loopExiting, valueMap);
    PDT.recalculate(F);
    auto * clonedExitingPostDom = PDT.getNode(&clonedExiting);

    // transfer branch probabilities, if any
    if (PB) {
      CloneBranchProbabilities(L, valueMap);
    }

    // drop the fake branch again (we created it to fake a sound CFG during analyses repair)
    splitBranch->eraseFromParent();
    assert(loopPreHead->getTerminator() == preTerm);

    return LoopCloneInfo{*clonedLoop, *clonedDomNode, *clonedExitingPostDom};
  }

  // transfer all branch probabilities from the src loop to the dest loop
  void
  CloneBranchProbabilities(Loop & srcLoop, ValueToValueMapTy & valueMap) {
    for (auto * block : srcLoop.blocks()) {
      const auto & term = *block->getTerminator();
      for (size_t i = 0; i < term.getNumSuccessors(); ++i) {
        auto p = PB->getEdgeProbability(block, i);
        auto * cloned = &LookUp(valueMap, *block);
        PB->setEdgeProbability(cloned, i, p);
      }
    }
  }

  // clone and remap all loop blocks internally
  void
  CloneLoopBlocks(Loop& L, ValueToValueMapTy & valueMap) {
    auto * loopHead = L.getHeader();
    // clone loop blocks
    SmallVector<BasicBlock*, 16> clonedBlockVec;
    for (auto * BB : L.blocks()) {
      auto * clonedBlock = CloneBasicBlock(BB, valueMap, "C");
      valueMap[BB] = clonedBlock;
      clonedBlockVec.push_back(clonedBlock);

      // add to block list
      F.getBasicBlockList().insert(loopHead->getIterator(), clonedBlock);
    }

    remapInstructionsInBlocks(clonedBlockVec, valueMap);
  }

  // register with the dom tree
  void
  CloneDomTree(BasicBlock & clonedIDom, Loop & L, BasicBlock & currentBlock, ValueToValueMapTy & valueMap) {
    if (!L.contains(&currentBlock)) return;

    auto & currentClone = LookUp(valueMap, currentBlock);
    DT.addNewBlock(&currentClone, &clonedIDom);

    auto * domNode = DT.getNode(&currentBlock);
    for (auto * childDom : *domNode) {
      CloneDomTree(currentClone, L, *childDom->getBlock(), valueMap);
    }
  }

  // register with the post dom tree
  void
  ClonePostDomTree(BasicBlock & clonedIDom, Loop & L, BasicBlock & currentBlock, ValueToValueMapTy & valueMap) {
    if (!L.contains(&currentBlock)) return;

    auto & currentClone = LookUp(valueMap, currentBlock);
    PDT.addNewBlock(&currentClone, &clonedIDom);

    auto * pDomNode = DT.getNode(&currentBlock);
    for (auto * childPostDom : *pDomNode) {
      ClonePostDomTree(currentClone, L, *childPostDom->getBlock(), valueMap);
    }
  }

  // returns a dom tree node and a loop representing the cloned loop
  // L is the original loop
  void
  CloneLoopAnalyses(Loop * clonedParentLoop, Loop & L, ValueToValueMapTy & valueMap) {
    // create a loop object
    auto * clonedHead = &LookUp(valueMap, *L.getHeader());
    auto * clonedLoop = LI.AllocateLoop(); // clonedHead

    // embed the loop object in the loop tree
    if (!clonedParentLoop) {
      LI.addTopLevelLoop(clonedLoop);
    } else {
      clonedParentLoop->addChildLoop(clonedLoop);
    }

    // add the header first
    clonedLoop->addBasicBlockToLoop(clonedHead, LI); //->addBlockEntry(clonedHead);

    // add blocks to the loop (that really sit on this level)
    for (auto * BB : L.blocks()) {
      if (BB == L.getHeader()) continue;
      if (LI.getLoopFor(BB) != &L) continue;

      auto * clonedBlock = &LookUp(valueMap, *BB);
      clonedLoop->addBasicBlockToLoop(clonedBlock, LI);
      // LI.changeLoopFor(clonedBlock, clonedLoop);
      // if (BB == clonedHead) continue;
      // clonedLoop->addBlockEntry(clonedBlock);
    }

    // recursively build child loops
    for (auto * childLoop : L) {
      CloneLoopAnalyses(clonedLoop, *childLoop, valueMap);
    }
  }
};


LoopCloneInfo
CloneLoop(llvm::Loop & L, llvm::Function & F, llvm::DominatorTree & DT, llvm::PostDominatorTree & PDT, llvm::LoopInfo & LI, llvm::BranchProbabilityInfo * PB, ValueToValueMapTy & cloneMap) {
  LoopCloner loopCloner(F, DT, PDT, LI, PB);
  return loopCloner.CloneLoop(L, cloneMap);
}

} // namespace rv
