//===- Linearizer.cpp ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @authors simon
//

#include "rv/transform/Linearizer.h"

#include "rv/region/Region.h"
#include "rv/vectorizationInfo.h"
#include "rv/transform/maskExpander.h"

#include <llvm/IR/Instructions.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/ADT/PostOrderIterator.h>

#include "llvm/Transforms/Utils/SSAUpdater.h"
#include <cassert>
#include <climits>
#include <set>

#include "report.h"

#include "rvConfig.h"

#if 1
#define IF_DEBUG_LIN IF_DEBUG
#else
#define IF_DEBUG_LIN if (false)
#endif

#if 0
#define IF_DEBUG_DTFIX IF_DEBUG
#else
#define IF_DEBUG_DTFIX if (false)
#endif

using namespace llvm;

#if 0
#define IF_DEBUG_INDEX IF_DEBUG
#else
#define IF_DEBUG_INDEX if (false)
#endif

namespace rv {

void
Linearizer::addToBlockIndex(BasicBlock & block) {
  assert(relays.size() < INT_MAX);
  int id = relays.size();
  assert(!blockIndex.count(&block));
  blockIndex[&block] = id;
  relays.push_back(RelayNode(block, id));
}

void
Linearizer::scheduleDomRegion(BasicBlock * domEntry, Loop * loop, std::string padStr, RPOT::rpo_iterator itStart, RPOT::rpo_iterator itEnd) {
  IF_DEBUG_INDEX errs() << padStr << "Sched DomRegion for " << domEntry->getName() << " loop " << (loop ? loop->getHeader()->getName() : "") << "\n";

  // schedule the dom region entry
  // if (!loop || loop->contains(domEntry)) {
    // do not add blocks to the index that are masked out
    addToBlockIndex(*domEntry);
  // }

  auto * domRegionNode = dt.getNode(domEntry);

  // schedule all nested dom regions in rpo
  for (auto it = itStart; it != itEnd; ++it) {
    auto * BB = *it;
    if (!inRegion(*BB)) continue;
    if (loop && !loop->contains(BB)) continue;

  // only directly schedule idom children
    auto * bbNode = dt.getNode(BB);
    auto * bbParentDom = bbNode->getIDom();
    if (bbParentDom != domRegionNode) {
      continue;
    }


  // schedule the entire loop as an IDom
    auto * bbLoop = li.getLoopFor(BB);
    if (bbLoop != loop) {
      // nested loop header -> schedule that loop entirely before continuing
      if (bbLoop && (bbLoop->getParentLoop() == loop && bbLoop->getHeader() == BB)) {
        scheduleLoop(bbLoop, padStr +  "  ", it, itEnd);
      }

    // otw, simply schedule this idom block
    } else {
      scheduleDomRegion(BB, loop, padStr + "  ", it, itEnd);
    }
  }
}

// schedule all idoms of the loop header
// first all idoms wihin the loop, then all idoms after the loop (all in RPOT order)
void
Linearizer::scheduleLoop(Loop * loop, std::string padStr, RPOT::rpo_iterator itStart, RPOT::rpo_iterator itEnd) {
  auto * loopHeader = loop->getHeader();
  // auto * headerDom = dt.getNode(loopHeader);

  IF_DEBUG_INDEX errs() << padStr << "Sched Loop at " << loopHeader->getName() << "\n";

  // schedule all dominatesd block within the loop
  scheduleDomRegion(loopHeader, loop, padStr+ "  ", itStart, itEnd);


  auto * parentLoop = loop->getParentLoop();

  // schedule all idoms that are not within this loop
  for (auto it = itStart; it != itEnd; ++it) {
    auto * BB = *it;

    if (!inRegion(*BB)) continue;
    if (loop->contains(BB)) continue;

    // TODO what about idoms on different parent loop levels (masked out if loop is set in rec call)
    if (loop->contains(dt.getNode(BB)->getIDom()->getBlock()) &&
        (!parentLoop || parentLoop->contains(BB))) {
     // dt.dominates(loopHeader, BB)) { //dt.getNode(BB)->getIDom() == headerDom) {
      scheduleDomRegion(BB, parentLoop, padStr + "  ", it, itEnd);;
    }
  }
#if 0

  // schedule all dominated parts in the parent loop that reach the outside loop
  scheduleDomRegion(loopHeader, loop->getParentLoop(), loop, itStart, itEnd);
#endif

#if 0
  SmallVector<BasicBlock*, 4> exitingBlocks;
  loop->getExitingBlocks(exitingBlocks);

// schedule all (immediately) dominated loop exits
  // only consider loop exits that reside in the immediate parent loop of this loop
  std::set<BasicBlock*> pendingExits;

  for (auto * exitingBlock : exitingBlocks) {
    auto * exitingDom = dt.getNode(exitingBlock);

    auto itSucc = succ_begin(exitingBlock);
    auto itEnd = succ_end(exitingBlock);

    for (;itSucc != itEnd; ++itSucc) {
      auto * exitBlock = *itSucc;

      // only consider exits to the immediate parent loop
      if (loop->getParentLoop() != li.getLoopFor(exitBlock)) continue;

      auto * exitNode = dt.getNode(exitBlock);

      // exiting block is idom of exit block
      if (exitNode->getIDom() == exitingDom) {
        pendingExits.insert(exitBlock);
      }
    }
  }

  // visit exiting blocks in RPOT order
  for (auto it = itStart; it != itEnd; ++it) {
    BasicBlock * exitBlock = *it;
    // this exit as an idom of a loop exit
    if (pendingExits.count(exitBlock)) {
      errs() << "\t eligible exit " << exitBlock->getName() << "\n";
      scheduleDomRegion(exitBlock, loop->getParentLoop(), nullptr, it, itEnd);
    // idom
    } else if(dt.getNode(dt).getIDom() == headerNode) {
    }
  }
#endif
}

void
Linearizer::buildBlockIndex() {
  relays.reserve(func.getBasicBlockList().size());

  RPOT rpot(&func);

  auto & entryBlock = vecInfo.getEntry();

  Loop * topLoop = li.getLoopFor(&entryBlock);

  scheduleDomRegion(&entryBlock, topLoop, "", rpot.begin(), rpot.end());
  return;
#if 0

  // FIXME this will diverge for non-canonical (LoopInfo) loops
  std::vector<BasicBlock*> stack;
  std::set<Loop*> pushedLoops;

  for (auto & block : func) {
    // seek unprocessed blocks
    if (!inRegion(block)) continue; // FIXME we need a Region::blocks-in-the-region iterator
    if (blockIndex.count(&block)) continue; // already indexed this block
    stack.push_back(&block);

    // process blocks
    while (!stack.empty()) {
      BasicBlock * block = stack.back();
      if (blockIndex.count(block)) {
        stack.pop_back();
        continue; // already indexed this block
      }

      auto * loop = li.getLoopFor(block);

      // we are seeing this loop for the first time
      // drop this block
      // push first the latch and than all predecessors of the header on top
      if (loop && pushedLoops.insert(loop).second) {
        stack.pop_back(); // forget how we entered this loop

        auto & latch = *loop->getLoopLatch();
        stack.push_back(&latch);

        // push all header predecessors on top of the latch
        for (auto * pred : predecessors(loop->getHeader())) {
          if (!inRegion(*pred)) continue;

          // do not descend into the latch
          if (loop->contains(pred)) continue;

          // Otw, check if dependencies are satifised
          if (!blockIndex.count(pred)) {
            stack.push_back(pred);
          }
        }

        // start processing the loop
        continue;
      }

      // filter out all dependences to loop-carried blocks if we are looking at the loop header
      Loop * filterLoop = nullptr;
      if (loop && loop->getHeader() == block) {
        filterLoop = loop;
      }

      bool allDone = true;

      for (auto * pred : predecessors(block)) {
        if (!inRegion(*pred)) continue;

        // do not descend into the latch
        if (filterLoop && filterLoop->contains(pred)) continue;

        // Otw, check if dependencies are satifised
        if (!blockIndex.count(pred)) {
          stack.push_back(pred);
          allDone = false;
        }
      }

      // all dependences satisfied -> assign topo index
      if (allDone) {
        // assign an id
        stack.pop_back();
        assert(!blockIndex.count(block));
        addToBlockIndex(*block);

        // if we are re-vising the loop header all dependences outside of the loop have been scheduled
        // now its time to schedule the remainder of the loop before any other outside block
        if (filterLoop) {
          auto * loopLatch = filterLoop->getLoopLatch();
          assert(loopLatch && "loop does not have a latch");
          if (!blockIndex.count(loopLatch)) {
            stack.push_back(loopLatch);
          }
        }
      }
    }
  }
#endif
}

Value&
Linearizer::promoteDefToBlock(BasicBlock & block, SmallVector<Value*, 16> & defs, Value & defaultDef, int defBlockId, int blockId, VectorShape instShape) {
  Value * localDef = nullptr;
  PHINode * localPhi = nullptr;

  auto * type = defaultDef.getType();

  auto itBegin = pred_begin(&block), itEnd = pred_end(&block);
  for (auto it = itBegin; it != itEnd; ++it) {
    auto * predBlock = *it;

    Value * inVal = nullptr;

    if (!hasIndex(*predBlock)) {
      // TODO ad hoc stepping through blendBlocks
      inVal = &promoteDefToBlock(*predBlock, defs, defaultDef, defBlockId, blockId, instShape);

    } else {
      int predIndex = getIndex(*predBlock);

      // turn incoming value into an explicit value (nullptr -> Undef)
      if (predIndex < defBlockId) {
        // predecessor not in span -> undef
        inVal = &defaultDef;

      } else if (predIndex >= blockId) {
        continue; // reaching backedge -> ignore

      } else {
        // predecessor in span with def
        int reachingDefId = predIndex - defBlockId;
        auto * reachingDef = defs[reachingDefId];
        if (!reachingDef) {
          // reaching undef within block range
          inVal = &defaultDef;
        } else {
          inVal = reachingDef;
        }
      }
    }

    // first reaching def OR reaching def is the same
    if (!localDef || localDef == inVal) {
      localDef = inVal;
      continue;
    }

    // Otw, we need a phi node
    if (!localPhi) {
      localPhi = PHINode::Create(type, 0, "", &*block.getFirstInsertionPt());
      vecInfo.setVectorShape(*localPhi, instShape);
      for (auto itPassedPred = itBegin; itPassedPred != it; ++itPassedPred) {
        localPhi->addIncoming(localDef, *itPassedPred);
      }
      IF_DEBUG_LIN { errs() << "\t | partial def PHI @ " << blockId << ", " << block.getName() << " : " << *localPhi << "\n"; }
      localDef = localPhi;
    }

    // attach the incoming value
    localPhi->addIncoming(inVal, predBlock);
  }

  // register as final definition at this point
  IF_DEBUG_LIN { errs() << "\t- localDef @ " << (blockId) << " " << *localDef << "\n"; }

  return *localDef;
}

Value &
Linearizer::promoteDefinitionExt(SmallVector<Value*, 16> & defs, Value & inst, Value & defaultDef, int defBlockId, int destBlockId) {
  assert(defBlockId <= destBlockId);
  if (defBlockId == destBlockId) return inst;

  IF_DEBUG_LIN { errs() << "\t* promoting value " << inst << " from def block " << defBlockId << " to " << destBlockId << "\n"; }

  const size_t span = destBlockId - defBlockId;
  assert(defs.size() > span);

  auto instShape = vecInfo.getVectorShape(inst);

  defs[0] = &inst;
  for (size_t i = 1; i < span + 1; ++i) {
    int blockId = defBlockId + i;
    auto & block = getBlock(blockId);

    defs[i] = &promoteDefToBlock(block, defs, defaultDef, defBlockId, blockId, instShape);
  }

  IF_DEBUG_LIN { errs() << "\tdefs[" << span << "] " << *defs[span] << "\n"; }
  return *defs[span];
}

Value &
Linearizer::promoteDefinition(Value & inst, Value & defaultDef, int defBlockId, int destBlockId) {
  IF_DEBUG_LIN { errs() << "\t* promoting value " << inst << " from def block " << defBlockId << " to " << destBlockId << "\n"; }

  assert(defBlockId <= destBlockId);

  if (defBlockId == destBlockId) return inst;

  const int span = destBlockId - defBlockId;
  SmallVector<Value*, 16> defs(span + 1, nullptr);
  return promoteDefinitionExt(defs, inst, defaultDef, defBlockId, destBlockId);
}

Value &
Linearizer::promoteDefinition(Value & inst, Value & defaultDef, int defBlockId, BasicBlock & userBlock) {
  if (!isa<Instruction>(inst)) return inst;

  if (hasIndex(userBlock)) {
    return promoteDefinition(inst, defaultDef, defBlockId, getIndex(userBlock));

  } else {
    int maxPredId = defBlockId;
    for (auto * predBlock : predecessors(&userBlock)) {
      assert(hasIndex(*predBlock) && "predecessor of blendBlock must be indexed block");
      maxPredId = std::max<>(maxPredId, getIndex(*predBlock));
    }

    const int span = maxPredId - defBlockId;
    SmallVector<Value*, 16> defs(span + 1, nullptr);

    // short cut: predecessor is def block
    if (span == 0) {
      return inst;
    }

    IF_DEBUG_LIN { errs() << "\t* promoting value " << inst << " from def block " << defBlockId << " to block " << userBlock.getName() << ", range " << span << " max pred " << maxPredId << "\n"; }

  // promote the definition to all predecessors of @userBlock
    promoteDefinitionExt(defs, inst, defaultDef, defBlockId, maxPredId);

  // materialize a definition in @userBlock
    return promoteDefToBlock(userBlock, defs, defaultDef, defBlockId, maxPredId + 1, vecInfo.getVectorShape(inst));
  }
}

void
Linearizer::verifyLoopIndex(Loop & loop) {
  for (auto * childLoop : loop) {
    verifyLoopIndex(*childLoop);
  }

  // not part of region -> skip this loop
  if (!inRegion(*loop.getHeader())) return;

  int startId = getNumBlocks(), endId = 0;

  for (auto * block : loop.blocks()) {
    startId = std::min<>(getIndex(*block), startId);
    endId = std::max<>(getIndex(*block), endId);
  }

  IF_DEBUG_LIN {
    errs() << "Loop index range of " << loop.getHeader()->getName() << " from "  << startId << " to " << endId << "\n";
  }

  // there are no blocks in the range that are not part of the loop
  for (int i = startId; i <= endId; ++i) {
    assert(loop.contains(&getBlock(i)) && "non-loop block in topo range of loop");
  }

  // the header has @startId, the latch as @endId
  assert(startId == getIndex(*loop.getHeader()));
  assert(endId == getIndex(*loop.getLoopLatch()));
}

void
Linearizer::verifyCompactDominance(BasicBlock & head) {
  int minIndex = 10000;
  int maxIndex = 0;
  std::set<int> domSet;

  IF_DEBUG_INDEX errs() << "verifyCompactDom " << head.getName() << "\n";
  auto * loop = li.getLoopFor(&head);

  for (auto & BB : vecInfo.getScalarFunction()) {
    if (!vecInfo.inRegion(BB)) continue;

    if (loop && !loop->contains(&BB)) continue;

    if (dt.dominates(&head, &BB)) {
      int id = getIndex(BB);
      minIndex = std::min<int>(minIndex, id);
      maxIndex = std::max<int>(maxIndex, id);
      domSet.insert(id);
    }
  }

  IF_DEBUG_INDEX errs() << "   [" << minIndex << ", " << maxIndex << "]\n";

  for (int i = minIndex; i <= maxIndex; ++i) {
    assert(domSet.count(i));
  }
}

void
Linearizer::verifyBlockIndex() {
  for (auto & block : func) {
    assert(!inRegion(block) || hasIndex(block));
    verifyCompactDominance(block);
  }

  for (auto * loop : li) {
    verifyLoopIndex(*loop);
  }
}

bool
Linearizer::needsFolding(TerminatorInst & termInst) {
  if (isa<ReturnInst>(termInst) || isa<UnreachableInst>(termInst)) return false;

  // fold all non-uniform branches
  return !vecInfo.getVectorShape(termInst).isUniform();
}

static void
InsertAtFront(BasicBlock & block, Instruction & inst) {
  block.getInstList().insert(block.begin(), &inst);
}


bool
Linearizer::needsFolding(PHINode & phi) {
  // this implementation exploits the fact that edges only disappear completely by relaying
  // e.g. if a edge persists we may assume that it always implies the old predicate

  auto & block = *phi.getParent();

  // this is the case if there are predecessors that are unknown to the PHI
  SmallPtrSet<BasicBlock*, 4> predSet;

  for (auto * inBlock : predecessors(&block)) {
    auto blockId = phi.getBasicBlockIndex(inBlock);
    if (blockId < 0) { return true; }
    predSet.insert(inBlock);
  }

  // or incoming blocks in the PHI node are no longer predecessors
  for (uint i = 0; i < phi.getNumIncomingValues(); ++i) {
    if (!predSet.count(phi.getIncomingBlock(i))) { return true; }
  }

  // Phi should still work
  return false;
}


template<class T>
DomTreeNodeBase<BasicBlock> *
FindIDom(const T & inBlocks, DominatorTree & dt) {
  BasicBlock * commonDomBlock = nullptr;
  for (auto * predBlock : inBlocks) {
    if (!commonDomBlock) { commonDomBlock = predBlock; }
    else { commonDomBlock = dt.findNearestCommonDominator(commonDomBlock, predBlock); }

    IF_DEBUG_DTFIX { errs() << "\t\t\t: dom with " << predBlock->getName() << " is " << commonDomBlock->getName() << "\n"; }

    assert(commonDomBlock && "domtree repair: did not reach a common dom node!");
  }

// domtree update: least common dominator of all incoming branches
  return dt.getNode(commonDomBlock);
}

typedef SmallVector<BasicBlock*, 4> SuperBlockVec;
class SuperInput {
public:
  SuperBlockVec inBlocks; // original predecessors that reach this remaining predecessor
  BasicBlock * predBlock; // remaining predecessor in linear CFG
  BasicBlock * blendBlock; // block used for select materialization

  SuperInput(SuperBlockVec && _inBlocks, BasicBlock & _predBlock)
  : inBlocks(_inBlocks)
  , predBlock(&_predBlock)
  , blendBlock(nullptr)
  {
    assert(!inBlocks.empty());
  }

  SuperInput()
  : inBlocks()
  , predBlock(nullptr)
  , blendBlock(nullptr)
  {}

  DomTreeNodeBase<BasicBlock>*
  materializeControl(BasicBlock & phiBlock, DominatorTree & dt, LoopInfo & loopInfo, Region * region) {
    if (!blendBlock) return dt.getNode(inBlocks[0]);

  // re-wire old predecessor to blend block
    predBlock->getTerminator()->replaceUsesOfWith(&phiBlock, blendBlock);

  // branch to phi Block
    BranchInst::Create(&phiBlock, blendBlock);

  // add block to region
    if (region) region->add(*blendBlock);

  // update domtree
    DomTreeNodeBase<BasicBlock>* idom = FindIDom<>(predecessors(blendBlock), dt);
    return dt.addNewBlock(blendBlock, idom->getBlock());

  // TODO update loop info
  }
};

static
Loop*
GetCommonLoop(LoopInfo & li, SuperBlockVec & blocks) {
  Loop * loop = nullptr;
  bool firstRound = true;
  for (auto * block : blocks) {
    if (firstRound) {
      loop = li.getLoopFor(block);
      firstRound = false;
    } else {
      auto * candLoop = li.getLoopFor(block);
      // wind up the loop tree until a common ancestro of @loop and @candLoop is found
      while (candLoop && candLoop != loop && !candLoop->contains(loop)) {
          candLoop = candLoop->getParentLoop();
      }
      loop = candLoop;
    }
  }

  return loop;
}

/// \brief create a super input value for this phi node
Value *
Linearizer::createSuperInput(PHINode & phi, SuperInput & superInput) {
  Constant * falseMask = ConstantInt::getFalse(phi.getContext());

  auto & blocks = superInput.inBlocks;

// make sure we have a dominating definition of the first incoming value available
  auto * defaultValue = phi.getIncomingValueForBlock(blocks[0]);

  // early exit: there is only one predecessor: no phis, no blend blocks -> return that value right away
  if (blocks.size() <= 1) return defaultValue; // FIXME we still need a dominating definition

// we will need blending: create a block for that to take place
  if (!superInput.blendBlock) {
    auto & joinBlock = *phi.getParent();
    auto superBlockName = joinBlock.getName() + ".s";
    superInput.blendBlock = BasicBlock::Create(phi.getContext(), superBlockName, phi.getParent()->getParent(), phi.getParent());
    auto * blockLoop = GetCommonLoop(li, blocks); // FIXME this does not apply to loop header inputs..
    if (blockLoop) {
      blockLoop->addBasicBlockToLoop(superInput.blendBlock, li);
    }
  }

  // make sure the default definition is dominating
  // FIXME also do this for the single predecessor case if inVal does not dominate it
  Value * blendedVal = defaultValue;
  if (isa<Instruction>(defaultValue)) {
    auto & defFuture = createRepairPhi(*defaultValue, *superInput.blendBlock);
    defFuture.addIncoming(defaultValue, blocks[0]);
    defFuture.addIncoming(defaultValue->getType() == falseMask->getType() ? falseMask : UndefValue::get(defaultValue->getType()), superInput.blendBlock);
    blendedVal = &defFuture;
  }

// Start buildling cascasding selects for all remaining incoming values
  IRBuilder<> builder(superInput.blendBlock);

  auto & phiBlock = *phi.getParent();

  numFoldedAssignments += blocks.size() - 1;

  auto phiShape = vecInfo.getVectorShape(phi);
  for (size_t i = 1; i < blocks.size(); ++i) {
    auto * inBlock = blocks[i];
    auto * inVal = phi.getIncomingValueForBlock(inBlock);

    auto * edgeMask = getEdgeMask(*inBlock, phiBlock);
    assert(edgeMask && "edgeMask not available!");

  // make sure the mask predicate is available at this point
  // TODO use caching
    if (isa<Instruction>(edgeMask)) {
      auto & maskFuture = createRepairPhi(*edgeMask, *superInput.blendBlock);
      maskFuture.addIncoming(edgeMask, inBlock);
      maskFuture.addIncoming(falseMask, superInput.blendBlock);
      edgeMask = &maskFuture;
    }

  // promote the incoming value
    if (isa<Instruction>(inVal)) {
      auto & inValFuture = createRepairPhi(*inVal, *superInput.blendBlock);
      inValFuture.addIncoming(inVal, inBlock);
      inValFuture.addIncoming(inVal->getType() == falseMask->getType() ? falseMask : UndefValue::get(inVal->getType()), superInput.blendBlock);
      inVal = &inValFuture;
    }

  // don't blend undefs
    if (isa<UndefValue>(inVal)) continue; // no need to blend in undef
    if (isa<UndefValue>(blendedVal)) { blendedVal = inVal; continue; }

    ++numBlends; // statistics

    std::string name = inVal->getName().str() + ".b";
    blendedVal = builder.CreateSelect(edgeMask, inVal, blendedVal, name);
    vecInfo.setVectorShape(*blendedVal, phiShape);
  }

  return blendedVal;
}

void
Linearizer::foldPhis(BasicBlock & block) {
// find first non repair PHI
  BasicBlock::iterator itPhi = block.begin();
  for (
      BasicBlock::iterator it = itPhi;
      it != block.end() && isa<PHINode>(*it) && isRepairPhi(cast<PHINode>(*it));
      ++it)
  {
    itPhi = it;
  }

  // no non-repair PHI found
  if (itPhi == block.end() || !isa<PHINode>(*itPhi)) {
    return;
  }

// no PHis, no folding
  auto & phi = cast<PHINode>(*itPhi);

  // only phi found is a repair phi
  if (isRepairPhi(phi)) return;

// check if PHIs need to be folded at all
  if (!needsFolding(phi)) {
    numUniformAssignments += phi.getNumIncomingValues();
    return;
  }

  // TODO fast path for num preds == 1

  IF_DEBUG_LIN { errs() << "\t- folding PHIs in " << block.getName() << "\n"; }

// identify all incoming values that stay immediate predecessors of this block
  // after folding multiple immediate predecessors may carry phi inputs in superposition (the former select blocks all re-route through the remaining predecessors)

  // merges new predecessors to all incoming definitions that are in super position
  std::map<BasicBlock*, SuperInput> selectBlockMap;

  // blocks in this set do not need a blend block
  SmallPtrSet<BasicBlock*, 4> seenPreds;
  SmallPtrSet<BasicBlock*, 4> seenInputs;

  for (auto * predBlock : predecessors(&block)) {
    if (!seenPreds.insert(predBlock).second) continue;

    // assert(preservedInputBlocks.count(predBlock) && "assuming that new preds are a subset of old preds");

    IF_DEBUG_LIN { errs() << "\t   inspecting pred " << predBlock->getName() << "\n"; }

    assert(hasIndex(*predBlock));
    auto & predReachingBlocks = getReachingBlocks(getIndex(*predBlock));

    // all inputs that are incoming on this edge after folding
    SuperBlockVec superposedInBlocks;

    for (size_t i = 0; i < phi.getNumIncomingValues(); ++i) {
      auto * inBlock = phi.getIncomingBlock(i);

      // otw, this value needs blending on any dominated input
      if (predBlock == inBlock || predReachingBlocks.count(inBlock)) {
        IF_DEBUG_LIN { errs() <<  "\t      - reaching in block " << inBlock->getName() << "\n";  }
        superposedInBlocks.push_back(inBlock);
        seenInputs.insert(inBlock);
      }
    }

    assert(superposedInBlocks.size() >= 1 && "no dominating def available on this select block?!");
    IF_DEBUG_LIN errs() << "phi: superposed incoming value for new inbound block " << predBlock->getName() << " : " << superposedInBlocks.size() << "\n";
    selectBlockMap[predBlock] = SuperInput(std::move(superposedInBlocks), *predBlock);
  }

  assert((phi.getNumIncomingValues() == seenInputs.size()) && "block reachability not promoted down to phi");
  // TODO can abort here if selectBlockMap.empty()
  assert(!selectBlockMap.empty());

// phi -> select based on getEdgeMask(start, dest)
  auto itStart = block.begin(), itEnd = block.end();
  for (auto it = itStart; it != itEnd; ) {
    auto * phi = dyn_cast<PHINode>(&*it++);
    if (!phi) break;
    if (phi->getNumIncomingValues() == 1) continue; // LCSSA
    if (isRepairPhi(*phi)) continue; // only a placeholder for defered SSA repair

    IRBuilder<> builder(&block, block.getFirstInsertionPt());

    numPreservedAssignments += selectBlockMap.size() - 1;

  // materialize blended inputs
    auto phiShape = vecInfo.getVectorShape(*phi);
    auto & flatPhi = *PHINode::Create(phi->getType(), 6, phi->getName(), phi);
    SmallPtrSet<const BasicBlock*, 4>  seenPreds;
    for (auto * predBlock : predecessors(&block)) {
      if (!seenPreds.insert(predBlock).second) continue;

      auto itSuperInput = selectBlockMap.find(predBlock);

      assert(itSuperInput != selectBlockMap.end());

      // folded iput
      auto * superInVal = createSuperInput(*phi, itSuperInput->second);
      auto & superInput = itSuperInput->second;

      auto * selectBlock = superInput.blendBlock ? superInput.blendBlock : predBlock;
      flatPhi.addIncoming(superInVal, selectBlock);
    }

  // simplify this input
    Value * replacement = nullptr;
    if (flatPhi.getNumIncomingValues() == 1) {
      replacement = flatPhi.getIncomingValue(0);
      flatPhi.eraseFromParent();
    } else {
      vecInfo.setVectorShape(flatPhi, phiShape); // TODO infer from operands
      replacement = &flatPhi;
    }

  // remove the old phi node
    phi->replaceAllUsesWith(replacement);
    phi->eraseFromParent();
  }

// embed future blend blocks into control
  for (auto it : selectBlockMap) {
    it.second.materializeControl(block, dt, li, region);
    if (it.second.blendBlock) {
      vecInfo.setVectorShape(*it.second.blendBlock->getTerminator(), VectorShape::uni());
    }
  }

  // update idom
  dt.getNode(&block)->setIDom(FindIDom<>(predecessors(&block), dt));
}

int
Linearizer::processLoop(int headId, Loop * loop) {
  auto & loopHead = getBlock(headId);
  assert(loop && "not actually part of a loop");
  assert(loop->getHeader() == &loopHead && "not actually the header of the loop");

  IF_DEBUG_LIN {
    errs() << "processLoop : header " << loopHead.getName() << " ";
    dumpRelayChain(getIndex(loopHead));
    errs() << "\n";
  }

  auto & latch = *loop->getLoopLatch();
  int latchIndex = getIndex(latch);
  int loopHeadIndex = getIndex(loopHead);

  // inherited relays from the pre-header edge: all targets except loop header
  RelayNode * headRelay = getRelay(headId);

  assert(!vecInfo.isDivergentLoop(loop) && "divLoopTrans should have normalized this loop by now");

  {
    ++numUniformLoops;
    if (headRelay) {
      // forward header reaching blocks to loop exits
      SuperBlockVec exitBlocks;
      loop->getExitBlocks(exitBlocks);
      for (auto * exitBlock : exitBlocks) {
        IF_DEBUG_LIN { errs() << "- merging head reaching&chain into exit " << exitBlock->getName();  dumpRelayChain(headRelay->id); errs() << "\n"; }
        int exitId = getIndex(*exitBlock);
        RelayNode * exitRelay = getRelay(exitId);
        if (!exitRelay) {
          exitRelay = &createRelay(exitId, headRelay->next);
        } else {
          if (headRelay->next) {
            int nextId = headRelay->next->id;
            addTargetToRelay(exitRelay, nextId);
          }
        }

        mergeInReaching(*exitRelay, *headRelay);
        // if (headRelay->next) addTargetToRelay(&exitRelay, headRelay->next->id); // FIXME
        IF_DEBUG_LIN { errs() << "\tafter merge: " << exitBlock->getName();  dumpRelayChain(getIndex(*exitBlock)); errs() << "\n"; }
      }
    }
  }

  // emit all blocks within the loop (except the latch)
  int latchNodeId = processRange(loopHeadIndex, latchIndex, loop);

  // now emit the latch (without descending into its successors)
  emitBlock(latchIndex);
  foldPhis(latch);

  // emit loop header again to re-wire the latch to the header
  emitBlock(loopHeadIndex);

  // attach undef inputs for all preheader edges to @loopHead
  addUndefInputs(loopHead);
  IF_DEBUG_LIN { errs() << "-- processLoop finished --\n"; }

  return latchNodeId + 1; // continue after the latch
}

void
Linearizer::addUndefInputs(llvm::BasicBlock & block) {
  auto itBegin = block.begin(), itEnd = block.end();
  for (auto it = itBegin; isa<PHINode>(*it) && it != itEnd; ++it) {
    auto & phi = cast<PHINode>(*it);
    for (auto * predBlock : predecessors(&block)) {
      auto blockId = phi.getBasicBlockIndex(predBlock);
      if (blockId >= 0) continue;

      phi.addIncoming(UndefValue::get(phi.getType()), predBlock);
    }
  }
}


// forwards branches to the relay target of @targetId to the actual @targetId block
// any scheduleHeads pointing to @target will be advanced to the next block on their itinerary
// @return the relay node representing all blocks that have to be executed after this one, if any
Linearizer::RelayNode *
Linearizer::emitBlock(int targetId) {
  auto & target = getBlock(targetId);
  IF_DEBUG_LIN {
    errs() << "\temit : " << target.getName() << "\n";
  }

// advance all relays for @target
  BasicBlock * relayBlock;
  auto * advancedRelay = advanceScheduleHead(targetId, relayBlock);

// if there is no relay for this head we are done
  if (!relayBlock) {
    return nullptr;
  }

// make all predecessors of @relayBlock branch to @target instead
  auto itStart = relayBlock->use_begin(), itEnd = relayBlock->use_end();

  // dom node of emitted target block
  auto * targetDom = dt.getNode(&target);
  assert(targetDom);

  IF_DEBUG_DTFIX errs() << "\t\tDTFIX: searching idom for " << target.getName() << "\n";

  for (auto itUse = itStart; itUse != itEnd; ) {
    Use & use = *(itUse++);

    int i = use.getOperandNo();
    auto & term = *cast<TerminatorInst>(use.getUser());
    IF_DEBUG_LIN { errs() << "\t\tlinking " << term << " opIdx " << i << "\n"; }

    // forward branches from relay to target
    term.setOperand(i, &target);
    IF_DEBUG_LIN { errs() << "\t\t-> linked " << term << " opIdx " << i << "\n"; }
  }

// search for a new idom
  auto * nextCommonDom = FindIDom<>(predecessors(&target), dt);

// domtree update: least common dominator of all incoming branches
  IF_DEBUG_DTFIX { errs() << "DT before dom change:";dt.print(errs()); }
  assert(nextCommonDom);
  targetDom->setIDom(nextCommonDom);
  IF_DEBUG_DTFIX { errs() << "DT after dom change:";dt.print(errs()); }

// if there are any instructions stuck in @relayBlock move them to target now
  // repair LCSSA incoming blocks along the way
  for (auto it = relayBlock->begin(); it != relayBlock->end() && !isa<TerminatorInst>(*it); it = relayBlock->begin()) {
    auto * phi = dyn_cast<PHINode>(it);
    if (phi && phi->getNumIncomingValues() == 1) {
      auto itPred = pred_begin(relayBlock);
      auto predEnd = pred_end(relayBlock);
      assert(itPred != predEnd);
      auto * singlePred = *itPred;

      IF_DEBUG_LIN {
        itPred++;
        assert(itPred == predEnd);
      }
      phi->setIncomingBlock(0, singlePred);
    }

    it->removeFromParent();
    InsertAtFront(target, *it);
  }

// dump remaining uses for debugging purposes
  IF_DEBUG_LIN {
    for (auto & use : relayBlock->uses()) {
      auto * userInst = dyn_cast<Instruction>(use.getUser());
      if (userInst) {
        errs() << "UserInst : " << *use.getUser() << " in block " << *userInst->getParent() << "\n";
        assert(!userInst);
      } else {
        errs() << "USe : " << *use.getUser() << "\n";
      }
    }
  }

  // free up the relayBlock
  relayBlock->eraseFromParent();

  // remaining exits after this block
  return advancedRelay;
}


// process the branch our loop at this block and return the next blockId
int
Linearizer::processBlock(int headId, Loop * parentLoop) {
  // pending blocks at this point
  auto & head = getBlock(headId);

  IF_DEBUG_LIN { errs() << "processBlock "; dumpRelayChain(headId); errs() << "\n"; }

// descend into loop, if any
  auto * loop = li.getLoopFor(&head);
  if (loop != parentLoop) {
    return processLoop(headId, loop);
  }

  // all dependencies satisfied -> emit this block
  auto * advancedExitRelay = emitBlock(headId);

  // convert phis to selectsw
  foldPhis(head);

  // materialize all relays
  processBranch(head, advancedExitRelay, parentLoop);

  return headId + 1;
}

int
Linearizer::processRange(int startId, int endId, Loop * parentLoop) {
  for (auto i = startId; i < endId;) {
    assert(!parentLoop || parentLoop->contains(&getBlock(i)));
    i = processBlock(i, parentLoop);
  }

  return endId;
}

bool
Linearizer::containsOriginalPhis(BasicBlock & block) {
  for (auto & inst : block) {
    auto * phi = dyn_cast<PHINode>(&inst);
    if (!phi) return false;
    if (!isRepairPhi(*phi)) return true;
  }
  return false;
}

void
Linearizer::mergeInReaching(RelayNode & dest, RelayNode & source) {
  if (&dest == &source) return;
  for (auto * bb: source.reachingBlocks) {
    dest.addReachingBlock(*bb);
  }
}


void
Linearizer::processBranch(BasicBlock & head, RelayNode * exitRelay, Loop * parentLoop) {
  IF_DEBUG_LIN {
    errs() << "  processBranch : " << *head.getTerminator() << " of block " << head.getName() << "\n";
  }

  auto & term = *head.getTerminator();
  auto & headRelay = getRelayUnchecked(getIndex(head));

  if (term.getNumSuccessors() == 0) {
    auto * retInst = dyn_cast<ReturnInst>(&term);
    auto * unreachInst = dyn_cast<UnreachableInst>(&term);

    if (!exitRelay) {
       IF_DEBUG_LIN { errs() << "\t control sink.\n"; }
       return;

    // lazily fold control sinks
    } else if (
        unreachInst ||
        (retInst && retInst->getNumOperands() == 0)
    ) {
      IF_DEBUG_LIN { errs() << "\t replacing control sink with branch because of pending relays.\n"; }


      // replace the control sink with a branch to the exitRelay->block
      auto * lateBranch = BranchInst::Create(exitRelay->block, &term);
      vecInfo.setVectorShape(*lateBranch, vecInfo.getVectorShape(term));
      vecInfo.dropVectorShape(term);
      term.eraseFromParent();

      // make sure all reaching prefixes are forwarded to reach exitRelay as well
      mergeInReaching(*exitRelay, headRelay);

      // this is a delayed return (since other prefixes still have unserved relays)
      ++numDelayedReturns;

      return;
    }

    errs() << "RV: error: Could not fold unconditional terminator!\n";
    errs() << term << "\n";
    abort();
    return;
  }

// generic handler for unifor terminators without side-effects
  bool mustFoldBranch = needsFolding(term);
  if (!mustFoldBranch) {
    IF_DEBUG_LIN { errs() << "\t uniform terminator."; }
    for (size_t i = 0; i < term.getNumSuccessors(); ++i) {
      auto & destBlock = *term.getSuccessor(i);
      int targetId = getIndex(destBlock);

      // add must-have targets
      auto & relay = addTargetToRelay(exitRelay, targetId);

      // statistics (cant branch to the block we wanted to go)
      if (relay.id < targetId && dt.dominates(&head, &destBlock)) {
        ++numDivertedHeads;
      }

      // promote reachability down to successors
      mergeInReaching(relay, headRelay);

      // if the branch target feeds a phi and the edge is relayed -> track reachability
      if (containsOriginalPhis(destBlock)) {
         relay.addReachingBlock(head);
      }

      // redirect branch to relay
      term.setSuccessor(i, relay.block);
    }

    // only track preserved conditional branches
    if (term.getNumSuccessors() > 1) {
      ++numPreservedBranches;
    }
    return;
  }


// this branch needs to be folded
  assert(isa<BranchInst>(term) && "folding only implemented for branches!");
  auto * branch = dyn_cast<BranchInst>(&term);
  assert(branch && "can only fold conditional BranchInsts (for now)");
  assert(branch->isConditional());

// statistics: this branch will be folded
  ++numFoldedBranches;

// order successors by global topologic order
  uint firstSuccIdx = 0;
  uint secondSuccIdx = 1;

  if (getIndex(*branch->getSuccessor(firstSuccIdx)) > getIndex(*branch->getSuccessor(secondSuccIdx))) {
    std::swap<>(firstSuccIdx, secondSuccIdx);
  }
  BasicBlock * firstBlock = branch->getSuccessor(firstSuccIdx);
  int firstId = getIndex(*firstBlock);
  BasicBlock * secondBlock = branch->getSuccessor(secondSuccIdx);
  int secondId = getIndex(*secondBlock);
  assert(firstId > 0 && secondId > 0 && "branch leaves the region!");

  IF_DEBUG_LIN {
    if (mustFoldBranch) {  errs() << "\tneeds folding. first is " << firstBlock->getName() << " at " << firstId << " , second is " << secondBlock->getName() << " at " << secondId << "\n"; }
  }

// process the first successor
// if this branch is folded then @secondBlock is a must-have after @firstBlock
  RelayNode * firstRelay = &addTargetToRelay(exitRelay, firstId);

  // the branch to secondBlock is relayed -> remember we came from head
  if (containsOriginalPhis(*secondBlock)) {
    firstRelay->addReachingBlock(head);
  }

  firstRelay = &addTargetToRelay(firstRelay, secondId);
  branch->setSuccessor(secondSuccIdx, firstRelay->block);


// relay the first branch to its relay block
  branch->setSuccessor(firstSuccIdx, firstRelay->block);

// whatever reaches head reaches the first successor
  mergeInReaching(*firstRelay, headRelay);

// domtree repair
  // if there is no relay node for B then A will dominate B after the transformation
  // this is because in that case all paths do B have to go through A first
  if (dt.dominates(&head, secondBlock) && !getRelay(secondId)) {
    auto * secondDom = dt.getNode(secondBlock);
    auto * firstDom = dt.getNode(firstBlock);
    assert(firstDom);

    IF_DEBUG_DTFIX { errs() << "DT before dom change:"; dt.print(errs()); }
    IF_DEBUG_DTFIX { errs() << "DTFIX: " << secondBlock->getName() << " idom is " << firstBlock->getName() << " by dominance\n"; }
    secondDom->setIDom(firstDom);
    IF_DEBUG_DTFIX { errs() << "DT after dom change:";dt.print(errs()); }
  }

// process the second successor
  // this makes sure all paths from the first successor will eventuall reach the second successor (post dom constraint)
  auto & secondRelay = addTargetToRelay(exitRelay, secondId);

  mergeInReaching(secondRelay, headRelay);

  // auto & secondRelay = requestRelay(secondMustHaves);
  if (containsOriginalPhis(*secondBlock)) {
    secondRelay.addReachingBlock(head);
  }
  secondRelay.addReachingBlock(*firstBlock);

// mark branch as non-divergent
  vecInfo.setVectorShape(*branch, VectorShape::uni());
}

void
Linearizer::run() {
  IF_DEBUG_LIN {
    errs() << "-- LoopInfo --\n";
    li.print(errs());
    errs() << "-- domTree --\n";
    dt.print(errs());
  }

// initialize with a global topologic enumeration
  buildBlockIndex();

// early exit on trivial cases
  if (getNumBlocks() <= 1) return;

// FIXME currently maskAnslysis is invalidated as a result of linearization.
  // We cache the latch masks locally before touching the function as we need those to make divergent loops uniform
  cacheMasks();

// dump divergent branches / loops
  IF_DEBUG_LIN {
    errs() << "-- LIN: divergent loops/brances in the region --";
    for (int i = 0; i < getNumBlocks(); ++i) {
      auto & block = getBlock(i);
      errs() << "\n" << i << " : " << block.getName() << " , ";
      if (needsFolding(*block.getTerminator())) {
         errs() << "Fold : " << *block.getTerminator();
      }
    }
  }

// verify the integrity of the block index
  verifyBlockIndex();

// fold divergent branches and convert divergent loops to fix point iteration form
  linearizeControl();

// simplify branches
  cleanup();

  dt.recalculate(func);

// repair SSA form on the linearized CFG
  resolveRepairPhis();

// repair SSA (def/use chains that were broken by chain merging)
  fixSSA();

// simplify trivial blends
  numSimplifiedBlends += simplifyBlends();

// verify control integrity
  IF_DEBUG_LIN verify();

// report statistics
  if (numFoldedBranches > 0 || numDivertedHeads > 0) {
    Report() << "lin:\n";
  }
  if (numFoldedBranches > 0) {
    Report() << "\t"
             << numFoldedBranches << " folded branches,\n\t"
             << numPreservedBranches << " preserved branches,\n\t"
             << numBlends << " folded phis,\n\t"
             << numSimplifiedBlends << " blends simplified.\n";
  }
  if (numDivertedHeads > 0) {
    Report() << "\t" << numDivertedHeads << " diverted relays.\n";
  }
  if (numUniformLoops > 0) {
    Report() << "\t" << numUniformLoops << " uniform loops.\n";
  }
  if (numFoldedAssignments > 0) {
    Report() << "phi stats:\n"
      << "\t" << numUniformAssignments << " c-uniform incoming values\n"
      << "\t" << numFoldedAssignments << " folded incoming values\n"
      << "\t" << numPreservedAssignments << " preserved incoming values.\n";
  }
}

void
Linearizer::linearizeControl() {
  IF_DEBUG_LIN {  errs() << "\n-- LIN: linearization log --\n"; }

  int lastId = processRange(0, getNumBlocks(), nullptr);
  (void) lastId;

  assert(lastId  == getNumBlocks());

  IF_DEBUG_LIN {  errs() << "\n-- LIN: linearization finished --\n"; }
}

PHINode &
Linearizer::createRepairPhi(Value & val, IRBuilder<> & builder) {
  PHINode * repairPhi = builder.CreatePHI(val.getType(), 2, val.getName() + ".R");

  VectorShape resShape = VectorShape::uni();
  if (vecInfo.hasKnownShape(val)) {
    resShape = vecInfo.getVectorShape(val);
  }

  vecInfo.setVectorShape(*repairPhi, resShape);
  repairPhis.insert(repairPhi);
  return *repairPhi;
}

PHINode &
Linearizer::createRepairPhi(Value & val, BasicBlock & destBlock) {
  if (destBlock.empty()) {
    IRBuilder<> builder(&destBlock);
    return createRepairPhi(val, builder);
  } else {
    IRBuilder<> builder(&destBlock, destBlock.getFirstInsertionPt());
    return createRepairPhi(val, builder);
  }
}

void
Linearizer::resolveRepairPhis() {
  IF_DEBUG_LIN { errs() << "-- resolving repair PHIs --\n"; }

  for (auto * repairPHI : repairPhis) {
    assert(repairPHI->getNumIncomingValues() == 2);
    auto * innerBlock = repairPHI->getIncomingBlock(0);
    auto * innerVal = repairPHI->getIncomingValue(0);
    auto * outerVal = repairPHI->getIncomingValue(1);

    int startIndex = getIndex(*innerBlock);

    auto & userBlock = *repairPHI->getParent();

    IF_DEBUG_LIN { errs() << " repair " << *repairPHI << " on range " << startIndex << " to " << userBlock.getName() << "\n"; }
    auto & promotedDef = promoteDefinition(*innerVal, *outerVal, startIndex, userBlock);
    repairPHI->replaceAllUsesWith(&promotedDef);
    vecInfo.dropVectorShape(*repairPHI);
    repairPHI->eraseFromParent();
  }

  repairPhis.clear();
}

void
Linearizer::verify() {
  IF_DEBUG_LIN { errs() << "\n-- LIN: verify linearization --\n"; func.dump(); }

  for (int i = 0; i < getNumBlocks(); ++i) {
    auto * block = &getBlock(i);
    auto * loop = li.getLoopFor(block);

    if (!loop) {
      assert(!needsFolding(*block->getTerminator()));

    } else if (loop && loop->getHeader() == block) {
      assert(!vecInfo.isDivergentLoop(loop));
    }
  }

  // check whether the on-the-fly domTree repair worked
  dt.verifyDomTree();

  // generic verification passes
  llvm::verifyFunction(func, &errs());
}

void
Linearizer::cacheMasks(){
  for (int i = 0; i < getNumBlocks(); ++i) {
    auto & block = getBlock(i);

// cache branch masks
   auto & term = *block.getTerminator();
   for (size_t i = 0; i < term.getNumSuccessors(); ++i) {
     auto * succBlock = term.getSuccessor(i);
     auto * edgeMask = maskEx.getEdgeMask(term, i); // .getExitMask(block, *succBlock);
     if (edgeMask) setEdgeMask(block, *succBlock, edgeMask);
   }
  }
}

void
Linearizer::cleanup() {
// simplify terminators
  // linearization can lead to terminators of the form "br i1 cond %blockA %blockA"
  for (auto & block : func) {
    auto * term = block.getTerminator();
    if (!term || term->getNumSuccessors() <= 1) continue; // already as simple as it gets

    bool allSame = true;
    BasicBlock * singleSucc = nullptr;
    for (uint i = 0; i < term->getNumSuccessors(); ++i) {
      if (!singleSucc) {
        singleSucc = term->getSuccessor(i);
      } else if (singleSucc != term->getSuccessor(i)) {
        allSame = false;
        break;
      }
    }

    if (allSame) {
      auto * simpleBranch = BranchInst::Create(singleSucc, term);
      vecInfo.setVectorShape(*simpleBranch, VectorShape::uni());
      vecInfo.dropVectorShape(*term);
      term->eraseFromParent();
    }
  }
}

int
Linearizer::getLeastIndex(const BasicBlock & block) const {
  if (hasIndex(block)) return getIndex(block);
  int leastIndex = -1;

  for (auto * predBlock : predecessors(&block)) {
    if (hasIndex(*predBlock)) {
      leastIndex = std::max<>(leastIndex, getIndex(*predBlock));
    }
  }

  assert (leastIndex > -1 && "has no indexed predecessors!");

  return leastIndex;
}

void
Linearizer::fixSSA() {
  for (auto & block : func) {
    if (!inRegion(block)) continue;

    DenseMap<Instruction*, Value*> promotionCache;

    for (auto & inst : block) {

      auto * phi = dyn_cast<PHINode>(&inst);

      // phi def/use repair
      if (phi) {
        for (size_t inIdx = 0; inIdx < phi->getNumIncomingValues(); ++inIdx) {
          auto * inBlock = phi->getIncomingBlock(inIdx);
          auto * inVal = phi->getIncomingValue(inIdx);

          auto * inInst = dyn_cast<Instruction>(inVal);
          if (!inInst) continue;

          auto inShape = vecInfo.getVectorShape(*inInst);

          auto & defBlock = *inInst->getParent();
          if (dt.dominates(&defBlock, inBlock)) continue;

          // ssa repair
          SmallVector<PHINode*, 8> phiVec;
          SSAUpdater ssaUpdater(&phiVec);
          ssaUpdater.Initialize(inInst->getType(), "_prom");
          ssaUpdater.AddAvailableValue(&defBlock, inInst);

          // kill recurring definitions in the loop header by forcing Undef
          auto * defLoop = li.getLoopFor(&defBlock);
          if (defLoop && defLoop->getHeader() != &defBlock) {
            ssaUpdater.AddAvailableValue(defLoop->getHeader(), UndefValue::get(inInst->getType()));
          }
          auto & fixedDef = *ssaUpdater.GetValueAtEndOfBlock(inBlock);
          for (auto * phi : phiVec) vecInfo.setVectorShape(*phi, inShape);

          phi->setIncomingValue(inIdx, &fixedDef);

        }
        continue;
      }

      // non-phi def/use repair
      for (size_t opIdx = 0; opIdx < inst.getNumOperands(); ++opIdx) {
        auto * opInst = dyn_cast<Instruction>(inst.getOperand(opIdx));
        if (!opInst) continue;

        auto & defParent = *opInst->getParent();

      // check if this chain was broken
        if (dt.dominates(&defParent, &block)) continue;

      // do we have a cached definition available?
        Value * fixedDef = nullptr;
        auto itCachedDef = promotionCache.find(opInst);
        if (itCachedDef != promotionCache.end()) {
          fixedDef = itCachedDef->second;
        } else {
          auto opShape = vecInfo.getVectorShape(*opInst);

          // ssa repair
          SmallVector<PHINode*, 8> phiVec;
          SSAUpdater ssaUpdater(&phiVec);
          ssaUpdater.Initialize(opInst->getType(), "_prom");
          ssaUpdater.AddAvailableValue(opInst->getParent(), opInst);

          // kill recurring definitions in the loop header by forcing Undef
          auto * defLoop = li.getLoopFor(opInst->getParent());
          if (defLoop && defLoop->getHeader() != opInst->getParent()) {
            ssaUpdater.AddAvailableValue(defLoop->getHeader(), UndefValue::get(opInst->getType()));
          }
          auto & promotedDef = *ssaUpdater.GetValueAtEndOfBlock(&block);
          for (auto * phi : phiVec) vecInfo.setVectorShape(*phi, opShape);

          // if not, promote the definition down to this use
          promotionCache[opInst] = &promotedDef;
          fixedDef = &promotedDef;
        }

        inst.setOperand(opIdx, fixedDef);
      }
    }

    // promote predicates
    auto * blockPred = vecInfo.getPredicate(block);
    if (!blockPred) continue;
    auto * predInst = dyn_cast<Instruction>(blockPred);
    if (!predInst) continue;

    auto & predDefBlock = *predInst->getParent();
    if (dt.dominates(&predDefBlock, &block)) continue;

    int defBlockIdx = getLeastIndex(predDefBlock);

    auto * boolTy = Type::getInt1Ty(predInst->getContext());
    auto & promotedDef = promoteDefinition(*predInst, *Constant::getNullValue(boolTy), defBlockIdx, block);

    vecInfo.setPredicate(block, promotedDef);
  }
}

// select simplifcation logic
using ValVec = SmallVector<Value*, 4>;

static bool
IsConstBool(Value & V, bool isTrue) {
  if (!isa<ConstantInt>(V)) return false;
  auto & constInt = cast<ConstantInt>(V);
  return isTrue ? constInt.getSExtValue() != 0 : constInt.getSExtValue() == 0;
}

static Value*
SimplifyBoolSelect(IRBuilder<> & builder, Value & cond, Value & trueVal, Value & falseVal, StringRef selName, ValVec & replacements) {
  if (&cond == &trueVal || IsConstBool(trueVal, true)) {
    // "C ? C : B"  or  "C ? True : B"  ->  C || B
    return builder.CreateOr(&cond, &falseVal, selName);
  }

  if (IsConstBool(falseVal, true)) {
    // "C ? A : True" ->  !C || A
    auto * notCond = builder.CreateNot(&cond, cond.getName().str() + ".not");
    replacements.push_back(notCond);
    return builder.CreateOr(notCond, &trueVal, selName);
  }

  if (&cond == &falseVal || IsConstBool(falseVal, false)) {
    //  "C ? A : C"  or  "C ? A : false" --> C && A
    return builder.CreateAnd(&cond, &trueVal, selName);
  }

  if (IsConstBool(trueVal, false)) {
    // "C ? false : B" --> !C && B
    auto * notCond = builder.CreateNot(&cond, cond.getName().str() + ".not");
    replacements.push_back(notCond);
    return builder.CreateAnd(notCond, &falseVal, selName);
  }

  // keep
  return nullptr;
}

static Value*
SimplifySelect(SelectInst & select, ValVec & replacements) {
  auto & cond = *select.getCondition();
  auto & trueVal = *select.getTrueValue();
  auto & falseVal = *select.getFalseValue();

  // generic rules
  if (&trueVal == &falseVal) {
    return &trueVal;
  }

  // boolean simplification rules
  if (cond.getType() == trueVal.getType()) {
    IRBuilder<> builder(select.getParent(), select.getIterator());
    return SimplifyBoolSelect(builder, cond, trueVal, falseVal, select.getName(), replacements);
  }

  // keep
  return nullptr;
}

size_t
Linearizer::simplifyBlends() {
  size_t numSimplified = 0;
  for (auto & block : func) {
    if (!inRegion(block)) continue;
    for (auto it = block.begin(); it != block.end();) {
      ValVec replacements;
      auto * select = dyn_cast<SelectInst>(it++);
      if (!select) continue;

      auto * simplified = SimplifySelect(*select, replacements);

      if (simplified) {
        IF_DEBUG_LIN { errs() << "Replacing " << *select << " with " << *simplified << "\n"; }
        numSimplified++;

        auto selectShape = vecInfo.getVectorShape(*select);
        replacements.push_back(simplified);
        for (auto * val : replacements) {
          if (isa<Instruction>(val)) vecInfo.setVectorShape(*val, selectShape);
        }

        select->replaceAllUsesWith(simplified);
        select->eraseFromParent();
        continue;
      }
    }
  }

  return numSimplified;
}

} // namespace rv
