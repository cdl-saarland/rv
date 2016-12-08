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

#include "rv/Region/Region.h"
#include "rv/vectorizationInfo.h"

#include <llvm/IR/Instructions.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>

#include <cassert>

#if 0
#define IF_DEBUG_DTFIX IF_DEBUG
#else
#define IF_DEBUG_DTFIX if (false)
#endif

namespace rv {


void
Linearizer::addToBlockIndex(BasicBlock & block) {
  blockIndex[&block] = blocks.size();
  blocks.push_back(&block);
}

using namespace rv;
using namespace llvm;

void
Linearizer::buildBlockIndex() {
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
}

Value &
Linearizer::promoteDefinition(Value & inst, uint defBlockId, uint destBlockId) {
  IF_DEBUG_LIN { errs() << "\t\tpromoting value " << inst << " from def block " << defBlockId << " to " << defBlockId << "\n"; }

  assert(defBlockId <= destBlockId);

  if (defBlockId == destBlockId) return inst;

  const int span = destBlockId - defBlockId + 1;

  auto * type = inst.getType();

  std::vector<Value*> defs(span, nullptr);
  defs[0] = &inst;

  for (int i = 1; i < span + 1; ++i) {
    int blockId = defBlockId + i;

    auto & block = getBlock(blockId);

    Value * localDef = nullptr;
    PHINode * localPhi = nullptr;

    auto itBegin = pred_begin(&block), itEnd = pred_end(&block);
    for (auto it = itBegin; it != itEnd; ++it) {
      auto * predBlock = *it;
      int predIndex = getIndex(*predBlock);

      // turn incoming value into an explicit value (nullptr -> Undef)
      Value * inVal = nullptr;
      if (predIndex < defBlockId) {
        // predecessor not in span -> undef
        inVal = UndefValue::get(type);

      } else if (predIndex >= blockId) {
        continue; // reaching backedge -> ignore

      } else {
        // predecessor in span with def
        int reachingDefId = predIndex - defBlockId;
        auto * reachingDef = defs[reachingDefId];
        if (!reachingDef) {
          // reaching undef within block range
          inVal = UndefValue::get(type);
        } else {
          inVal = reachingDef;
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
        for (auto itPassedPred = itBegin; itPassedPred != it; ++itPassedPred) {
          localPhi->addIncoming(localDef, *itPassedPred);
        }
        localDef = localPhi;
      }

      // attach the incoming value
      localPhi->addIncoming(inVal, predBlock);
    }

    // register as final definition at this point
    defs[i] = localDef;
  }

  return *defs[span];
}

void
Linearizer::verifyLoopIndex(Loop & loop) {
  for (auto * childLoop : loop) {
    verifyLoopIndex(*childLoop);
  }

  auto & latch = *loop.getLoopLatch();
  uint startId = blocks.size();
  uint endId = 0;

  for (auto * block : loop.blocks()) {
    startId = std::min<>(getIndex(*block), startId);
    endId = std::max<>(getIndex(*block), endId);
  }

  IF_DEBUG_LIN {
    errs() << "Loop index range of " << loop.getHeader()->getName() << " from "  << startId << " to " << endId << "\n";
  }

  // there are no blocks in the range that are not part of the loop
  for (uint i = startId; i <= endId; ++i) {
    assert(loop.contains(blocks[i]) && "non-loop block in topo range of loop");
  }

  // the header has @startId, the latch as @endId
  assert(startId == getIndex(*loop.getHeader()));
  assert(endId == getIndex(latch));
}

void
Linearizer::verifyBlockIndex() {
  for (auto * loop : li) {
    verifyLoopIndex(*loop);
  }
}

bool
Linearizer::needsFolding(TerminatorInst & termInst) {
  assert(!isa<SwitchInst>(termInst) && "switches unsupported at the moment");

  if (isa<ReturnInst>(termInst) || isa<UnreachableInst>(termInst)) return false;

// Only conditional branches are subject to divergence
  auto & branch = cast<BranchInst>(termInst);
  if (!branch.isConditional()) return false;

// the branch condition is immediately divergent
  // if (!vecInfo.getVectorShape(*branch.getCondition()).isUniform()) return true;
  if (!vecInfo.getVectorShape(branch).isUniform()) return true;

#if 0
  // logical fallacy we keep suspending this decision until all reaching terminators have been processed
  // eventuall we know if this branch needs folding or not
// the successors of this block are in the same @destBlock set of a relay node
  // this implies that
  // FIXME data structure..
  for (auto it : scheduleHeads) {
    bool allIncluded = true;
    for (uint i = 0; i < branch.getNumSucessors(); ++i) {
      allIncluded &= it->second->destBlocks.count(i);
    }

    if (allIncluded) return true;
  }
#endif

  return false;
}

Function *
Linearizer::requestReductionFunc(llvm::Module & mod, const std::string & name) {
  auto * redFunc = mod.getFunction(name);
  if (redFunc) return redFunc;
  auto & context = mod.getContext();
  auto * boolTy = Type::getInt1Ty(context);
  auto * funcTy = FunctionType::get(boolTy, boolTy, false);
  redFunc = Function::Create(funcTy, GlobalValue::ExternalLinkage, name, &mod);
  redFunc->setDoesNotAccessMemory();
  redFunc->setDoesNotThrow();
  redFunc->setConvergent();
  redFunc->setDoesNotRecurse();
  return redFunc; // TODO add SIMD mapping
}

Instruction &
Linearizer::createReduction(Value & pred, const std::string & name, BasicBlock & atEnd) {
  auto * redFunc = requestReductionFunc(*atEnd.getParent()->getParent(), name);
  auto * call = CallInst::Create(redFunc, &pred, "reduce", &atEnd);
  vecInfo.setVectorShape(*call, VectorShape::uni());
  return *call;
}

void
Linearizer::dropLoopExit(BasicBlock & block, Loop & loop) {
  auto & term = *block.getTerminator();
  assert(loop.contains(&block) && "can not drop loop exit edge from block that is not in loop");
  assert(term.getNumSuccessors() > 1 && "these must be an edge to drop here");

// find a successor within this loop
  BasicBlock * uniqueLoopSucc = nullptr;
  for (uint i = 0; i < term.getNumSuccessors(); ++i) {
    auto * succ = term.getSuccessor(i);
    if (!uniqueLoopSucc && loop.contains(succ)) {
      uniqueLoopSucc = succ;
      break;
    }
  }
  assert(uniqueLoopSucc && "could not find successor within loop");

// send all loop exiting edges to that successor inside the loop
  // replace this node with a single successor node
  auto * loopBranch = BranchInst::Create(uniqueLoopSucc, &term);
  term.eraseFromParent();
  vecInfo.dropVectorShape(term);
  vecInfo.setVectorShape(*loopBranch, VectorShape::uni());
}

static void
InsertAtFront(BasicBlock & block, Instruction & inst) {
  block.getInstList().insert(block.begin(), &inst);
}

Linearizer::RelayNode &
Linearizer::convertToSingleExitLoop(Loop & loop, ConstBlockSet mustHaves) {

// query the live mask on the latch
  auto & latch = *loop.getLoopLatch();
  auto latchIndex = getIndex(latch);
  assert(latchIndex >= 0);
  auto & header = *loop.getHeader();
  auto headerIndex = getIndex(header);
  assert(headerIndex >= 0);
  Value* liveCond = maskAnalysis.getExitMask(latch, header);

// collect all loop exits
  // on the way check if we need to repair any LCSSA phi nodes
  // we will create a single exit at the latch and might need to re-route live-out values through it
  SmallVector<BasicBlock*, 3> loopExitBlocks;
  loop.getExitBlocks(loopExitBlocks);

  ConstBlockSet loopExits(mustHaves);
  for (auto * block : loopExitBlocks) {
    loopExits.insert(block);
  }

// create a common relay for all loopExits
  auto & loopExitRelay = requestRelay(loopExits);

// move LCSSA nodes to relay
  for (auto * block : loopExitBlocks) {

    if (block == loopExitRelay.block) {
      continue; // already migrated LCSSA phi to loop exit relay
    }

    // check if we need to repair any LCSSA phi nodes
    for (auto it = block->begin(); isa<PHINode>(it) && it != block->end(); it = block->begin()) {
      auto * lcPhi = &cast<PHINode>(*it);
      if (!lcPhi) break;

      // for all exiting edges
      for (uint i = 0; i < lcPhi->getNumIncomingValues(); ++i) {
        auto * exitingBlock = lcPhi->getIncomingBlock(i);
        assert (loop.contains(exitingBlock) && "not an LCSSA Phi node");

        auto * inst = dyn_cast<Instruction>(lcPhi->getIncomingValue(i));
        if (!inst) {
          // no repair necessary as the incoming value is globally available in the function
          continue;
        }

        BasicBlock * defBlock = inst->getParent();

        // branch will start from the latch
        lcPhi->setIncomingBlock(i, &latch);

        // def dominates exit block and will continue to do so after loop transform
        if (dt.dominates(defBlock, block)) {
          continue;
        }

        // def does not dominate latch
        // create a dominating def by inserting PHI nodes with incoming undefs
        int defIndex = getIndex(*defBlock);
        assert(headerIndex <= defIndex && defIndex <= latchIndex && "non-dominating def not in loop");

        auto & dominatingDef = promoteDefinition(*inst, defIndex, latchIndex);

        // replace incoming value with new dominating def
        lcPhi->setIncomingValue(i, &dominatingDef);
      }

      // migrate this PHI node to the loopExitRelay
      IF_DEBUG_LIN { errs() << "\t\tMigrating " << lcPhi->getName() << " from " << lcPhi->getParent()->getName() << " to " << loopExitRelay.block->getName() << "\n"; }

      // FIXME this will generate redundant PHI nodes if the same instruction is liveout on multiple exits
      lcPhi->removeFromParent();
      InsertAtFront(*loopExitRelay.block, *lcPhi);
    }
  }

// drop all loop exiting blocks
  SmallVector<BasicBlock*, 3> loopExitingBlocks;
  loop.getExitingBlocks(loopExitingBlocks);

  for (auto * exitingBlock : loopExitingBlocks) {
    dropLoopExit(*exitingBlock, loop);
  }

// drop old latch
  auto * latchTerm = latch.getTerminator();
  assert(latchTerm);
  assert(latchTerm->getNumSuccessors() == 1);
  vecInfo.dropVectorShape(*latchTerm);
  latchTerm->eraseFromParent();

// create a new if-all-threads-have-left exit branch
  // cond == rv_any(<loop live mas>) // as long as any thread is still executing
  auto * anyThreadLiveCond = &createReduction(*liveCond, "rv_any", latch);
  BranchInst* branch = BranchInst::Create(&header, loopExitRelay.block, anyThreadLiveCond, &latch);

// mark loop and its latch exit as non-divergent
  vecInfo.setVectorShape(*branch, VectorShape::uni());
  vecInfo.setLoopDivergence(loop, false);

// Update mask analysis information.
  Value* loopExitCond = maskAnalysis.getCombinedLoopExitMask(loop);
  maskAnalysis.updateExitMasks(latch,
                                 anyThreadLiveCond,
                                 loopExitCond,
                                 &*(latch.getFirstInsertionPt()));

  return loopExitRelay;
}

uint
Linearizer::processLoop(BasicBlock & loopHead, ConstBlockSet mustHaves) {
  IF_DEBUG_LIN {
    errs() << "processLoop : header " << loopHead.getName() << " {";
    for (const auto * mustHaveExit : mustHaves) {
      errs() << mustHaveExit->getName() << ", ";
    }
    errs() << "}\n";
  }

  assert(inRegion(loopHead));
  auto * loop = li.getLoopFor(&loopHead);
  assert(loop && "not actually part of a loop");
  assert(loop->getHeader() == &loopHead && "not actually the header of the loop");

  auto & latch = *loop->getLoopLatch();

  if (vecInfo.isDivergentLoop(loop)) {
    // convert loop into a non-divergent form
    convertToSingleExitLoop(*loop, mustHaves);
  }

  // emit all blocks within the loop (except the latch)
  uint latchNodeId = processRange(getIndex(loopHead), getIndex(latch), mustHaves, loop);

  // now emit the latch (without descending into its successors)
  emitBlock(latch);

  // emit loop header again to re-wire the latch to the header
  emitBlock(loopHead);

  return latchNodeId + 1; // continue after the latch
}

// makes relayBlocks for @target branch to @target
// any scheduleHeads pointing to @target will be advanced to the next block on their itinerary
// @return the relay node representing all blocks that have to be executed after this one, if any
Linearizer::RelayNode *
Linearizer::emitBlock(BasicBlock & target) {
  IF_DEBUG_LIN {
    errs() << "\temit : " << target.getName() << "\n";
  }

  assert(!relayBlocks.count(&target) && "can not finalize relays");

// mark this block as finished
  finishedBlocks.insert(&target);

// advance all relays for @target
  BasicBlock * relayBlock;
  auto * advancedRelay = advanceScheduleHead(target, relayBlock);

  // if there is no relay for this head we are done
  if (!relayBlock) {
    // IF_DEBUG_LIN { errs() << "\tno relayBlock!\n"; }
    return nullptr;
  }

// make all predecessors of @relayBlock branch to @target instead
#if 0
  // for now: branch to you destination
  IRBuilder<> builder(relayBlock);
  builder.CreateBr(&target);

#else
  auto itStart = relayBlock->use_begin();
  auto itEnd = relayBlock->use_end();

  // dom node of emitted target block
  auto * targetDom = dt.getNode(&target);
  assert(targetDom);

  // while at it search for a new dominator
  BasicBlock * commonDomBlock = nullptr;

  for (auto itUse = itStart; itUse != itEnd; ) {
    Use & use = *(itUse++);

    int i = use.getOperandNo();
    auto & branch = *cast<BranchInst>(use.getUser());
    IF_DEBUG_LIN { errs() << "\t\tlinking " << branch << " opIdx " << i << "\n"; }

    // forward branches from relay to target
    branch.setOperand(i, &target);

    // advance to the nearest common dominator of all branches so far
    auto * branchBlock = branch.getParent();
    if (!commonDomBlock) { commonDomBlock = branchBlock; }
    else { commonDomBlock = dt.findNearestCommonDominator(commonDomBlock, branchBlock); }
    assert(commonDomBlock && "domtree repair: did not reach a common dom node!");
  }

// domtree update: least common dominator of all incoming branches
  // assert(commonDomBlock != &target && "did not find a valid idom of target"); // FIXME what about loop headers?
  auto * nextCommonDom = dt.getNode(commonDomBlock);
  assert(nextCommonDom);
  IF_DEBUG_DTFIX{ errs() << "DT before dom change:";dt.print(errs()); }
  targetDom->setIDom(nextCommonDom);
  IF_DEBUG_DTFIX { errs() << "DT after dom change:";dt.print(errs()); }

// if there are any instructions stuck in @relayBlock move them to target now
  for (auto it = relayBlock->begin(); it != relayBlock->end() && !isa<TerminatorInst>(*it); it = relayBlock->begin()) {
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
  relayBlocks.erase(relayBlock);
#endif

  return advancedRelay;
}


// linearize the region consisting of all blocks that @domNode dominates and that are contained within @parentLoop
// create a relay block (@relayBlock) for every destination outside of the region (@destBlock)
// all branches to @nextBlock can stay because it will be scheduled next
uint
Linearizer::processBlock(BasicBlock & head, ConstBlockSet mustHaves, Loop * parentLoop) {
  // pending blocks at this point
  auto * headRelay = getRelay(head);
  // assert(headRelay && "dominance based propagation disengaged for now!");

  // we need to promote them to the branch targets
  mustHaves.clear(); // FIXME this should all be kept in the relayNode for now
  if (headRelay) {
    for (auto * block : headRelay->destBlocks) {
      if (block == &head) continue; // jsut emitting this block now
      mustHaves.insert(block);
    }
  }

  IF_DEBUG_LIN {
    errs() << "processBlock (" << getIndex(head) << ") " << head.getName() << " {";
    for (const auto * mustHaveExit : mustHaves) {
      errs() << mustHaveExit->getName() << ", ";
    }
    errs() << "}\n";
  }

#if 0
  // FIXME this is a hack to skip over the exits of normalized divergent loops
  if (parentLoop && (&head == parentLoop->getLoopLatch())) {
    IF_DEBUG_LIN { errs() << "\tconverted loop latch (skipping)\n";  }

    // emit the latch and be done with this loop
    emitBlock(head);

    return getIndex(head) + 1;
  }
#endif

  // assert(!finishedBlocks.count(&head));

// descend into loop, if any
  auto * loop = li.getLoopFor(&head);

  if (loop != parentLoop) {
    return processLoop(head, mustHaves);

  } else {
    // all dependencies satisfied -> emit this block
    emitBlock(head);

    // materialize all relays
    processBranch(head, mustHaves, parentLoop);

    return getIndex(head) + 1;
  }

#if 0
// process remaining blocks in dom region
  for (uint i = getIndex(head); i < blocks.size(); ++i) { // FIXME make this more efficient
    auto * block = blocks[i];
    if (finishedBlocks.count(block)) continue;

    // stay within the region even if we dominate loop exits
    if (parentLoop && !parentLoop->contains(block)) continue;

    if (dt.dominates(&head, block)) {
       processDomRegion(*block, mustHaves, parentLoop);
    }
  }
#endif
}

uint
Linearizer::processRange(uint startId, uint endId, ConstBlockSet mustHaves, Loop * parentLoop) {
  for (uint i = startId; i < endId;) { // FIXME make this more efficient
    auto * block = blocks[i];

    // assert(!finishedBlocks.count(block));
    assert(!parentLoop || parentLoop->contains(block));

    i = processBlock(*block, mustHaves, parentLoop);
  }

  return endId;
}

void
Linearizer::processBranch(BasicBlock & head, ConstBlockSet mustHaves, Loop * parentLoop) {
  IF_DEBUG_LIN {
    errs() << "  processBranch : " << *head.getTerminator() << " of block " << head.getName() << "\n";
  }

  auto & term = *head.getTerminator();

  if (term.getNumSuccessors() == 0) {
    IF_DEBUG_LIN {
      errs() << "\t control sink.\n";
    }
    return;
  }

  auto * branch = dyn_cast<BranchInst>(&term);

// Unconditional branch case
  if (!branch->isConditional()) {
#if 0
    if (false /* dt.dominates(&head, branch->getSuccessor(0))*/ ) {
      processDomRegion(*branch->getSuccessor(0), mustHaves, parentLoop);
    } else
#endif
    {
      ConstBlockSet pendingBlocks(mustHaves); // {B} + mustHaves
      pendingBlocks.insert(branch->getSuccessor(0));
      auto & relay = requestRelay(pendingBlocks);
      branch->setSuccessor(0,  relay.block);
    }

    return;
  }

// whether this branch must be eliminated from the CFG
  assert(branch && "can only fold conditional BranchInsts (for now)");
  bool mustFoldBranch = needsFolding(*branch);

// order successors by global topologic order
  unsigned firstIdx = 0;
  unsigned secondIdx = 1;

  if (getIndex(*branch->getSuccessor(firstIdx)) > getIndex(*branch->getSuccessor(secondIdx))) {
    std::swap<>(firstIdx, secondIdx);
  }
  BasicBlock * firstBlock = branch->getSuccessor(firstIdx);
  BasicBlock * secondBlock = branch->getSuccessor(secondIdx);

// process the first successor
// if this branch is folded then @secondBlock is a must-have after @firstBlock
  ConstBlockSet firstMustHaves(mustHaves); // {B} + mustHaves

  if (mustFoldBranch) {
    firstMustHaves.insert(secondBlock); // @secondBlock only becomes a must have of @firstBlock if this branch is folded
    branch->setSuccessor(secondIdx, firstBlock); // put @secondBlock after @firstBlock's dominance region in the linearized schedule
  }


// domtree repair
  // if there is no relay node for B then A will dominate B after the transformation
  // this is because in that case all paths do B have to go through A first
  if (dt.dominates(&head, secondBlock) && !getRelay(*secondBlock)) {
    auto * secondDom = dt.getNode(secondBlock);
    auto * firstDom = dt.getNode(firstBlock);
    assert(firstDom);

    IF_DEBUG_DTFIX { errs() << "DT before dom change:"; dt.print(errs()); }
    secondDom->setIDom(firstDom);
    IF_DEBUG_DTFIX { errs() << "DT after dom change:";dt.print(errs()); }
  }

#if 0
  bool descendIntoFirst = false;
    // if the edge is within the loop and does not lead back to the header
    (!parentLoop || (parentLoop->contains(firstBlock) && firstBlock != parentLoop->getHeader())) &&
     // and we dominate the branch target
     dt.dominates(&head, firstBlock);

  if (descendIntoFirst) { // TODO OR all blocks with a lower topo index have been finished
    // if we dominate the successor we can descend into it right away
    // we know that @secondBlock will come after the domRegion of @firstBlock, a fact we could tell ::processDomRegion about to avoid superfluous relays
    processDomRegion(*firstBlock, firstMustHaves, parentLoop);

  } else
#endif

  {
    // preserve any pre-existing must haves
    if (relayBlocks.count(firstBlock)) {
      // TODO should probably use this to disable dominance criterion
      getDestBlocks(*firstBlock, firstMustHaves);
    }

    // Otw, we need to process any other predecessors of @secondBlock first
    // we stall the processing of branches to @firstBlock by redirecting those branches to a @relayBlock first
    firstMustHaves.insert(firstBlock); // as @firstBlock is deferred now as well, we need to add it to the blocks @relayBlock is deferring
    auto & firstRelay = requestRelay(firstMustHaves);
    branch->setSuccessor(firstIdx, firstRelay.block);
  }

// process the second successor
#if 0
  bool descendIntoSecond = false;
    // if the edge is within the loop and does not lead back to the header
    (!parentLoop || (parentLoop->contains(secondBlock) && secondBlock != parentLoop->getHeader())) &&
     // and we dominate the branch target
     dt.dominates(&head, secondBlock);

  if (descendIntoSecond) { // TODO OR all blocks with a lower topo index have been finished
   processDomRegion(*secondBlock, mustHaves, parentLoop);
  } else
#endif

  {
    ConstBlockSet secondMustHaves(mustHaves); // inherited must haves
    secondMustHaves.insert(secondBlock);

    // preserve any pre-existing must haves
    if (relayBlocks.count(secondBlock)) {
      // TODO should probably use this to disable dominance criterion
      getDestBlocks(*secondBlock, secondMustHaves);
    }

    auto & secondRelay = requestRelay(secondMustHaves);
    if (!mustFoldBranch) {
      branch->setSuccessor(secondIdx, secondRelay.block);
    }
  }

// mark branch as non-divergent
  vecInfo.setVectorShape(*branch, VectorShape::uni());
}

void
Linearizer::run() {

  IF_DEBUG_LIN {
    errs() << "-- LoopInfo --\n";
    li.print(errs());
  }

// initialize with a global topologic enumeration
  buildBlockIndex();

// verify the integrity of the block index
  verifyBlockIndex();

// early exit on trivial cases
  if (blocks.size() <= 1) return;

// dump divergent branches / loops
  IF_DEBUG_LIN {
    dt.print(errs());

    errs() << "-- LIN: divergent loops/brances in the region --";
    for (uint i = 0; i < blocks.size(); ++i) {
      auto * block = blocks[i];
      auto * loop = li.getLoopFor(block);

      errs() << "\n" << i << " : " << block->getName() << " , ";

      if (!loop) {
        if (needsFolding(*block->getTerminator())) {
           errs() << "Fold : " << *block->getTerminator() << " of block " << block->getName();
        }

      } else if (loop && loop->getHeader() == block) {
        if (vecInfo.isDivergentLoop(loop)) {
           errs() << "Fold loop with header: " << block->getName();
        }
      }
    }
  }

// fold divergent branches and convert divergent loops to fix point iteration form
  linearizeControl();

  cleanup();

// verify control integrity
  verify();
}

void
Linearizer::linearizeControl() {
  IF_DEBUG_LIN {  errs() << "\n-- LIN: linearization log --\n"; }

  uint lastId = processRange(0, blocks.size(), ConstBlockSet(), nullptr);

  assert(lastId  == blocks.size());
  assert(scheduleHeads.empty() && "did not finalize all schedule heads!");

  IF_DEBUG_LIN {  errs() << "\n-- LIN: linearization finished --\n"; }
}



void
Linearizer::verify() {
  // TODO move this in its own verification module

  IF_DEBUG_LIN { errs() << "\n-- LIN: verify linearization --\n"; }

  for (uint i = 0; i < blocks.size(); ++i) {
    auto * block = blocks[i];
    auto * loop = li.getLoopFor(block);

    if (!loop) {
      assert(!needsFolding(*block->getTerminator()));

    } else if (loop && loop->getHeader() == block) {
      assert(!vecInfo.isDivergentLoop(loop));
    }
  }

  // check whether the on-the-fly domTree repair worked
  dt.verifyDomTree();

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


} // namespace rv
