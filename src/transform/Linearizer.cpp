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
#include <climits>
#include <set>

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

namespace rv {

void
Linearizer::addToBlockIndex(BasicBlock & block) {
  assert(relays.size() < INT_MAX);
  int id = relays.size();
  blockIndex[&block] = id;
  relays.push_back(RelayNode(block, id));
}

using namespace rv;
using namespace llvm;

void
Linearizer::buildBlockIndex() {
  relays.reserve(func.getBasicBlockList().size());

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
Linearizer::promoteDefinition(Value & inst, int defBlockId, int destBlockId) {
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
  if (!vecInfo.getVectorShape(branch).isUniform()) return true;

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
Linearizer::convertToSingleExitLoop(Loop & loop, RelayNode * exitRelay) {

// query the live mask on the latch
  auto & latch = *loop.getLoopLatch();
  auto latchIndex = getIndex(latch);
  assert(latchIndex >= 0);
  auto & header = *loop.getHeader();
  assert(getIndex(header) >= 0);
  Value* liveCond = maskAnalysis.getExitMask(latch, header);

// create a relay for the single exit block that this loop will have after the conversion
  // merge all existing exit blocks into the relay chain inherited from the header
  SmallVector<BasicBlock*, 3> loopExitBlocks;
  loop.getExitBlocks(loopExitBlocks);

  auto * loopExitRelay = exitRelay;
  for (auto * block : loopExitBlocks) {
    auto exitId = getIndex(*block);
    loopExitRelay = &addTargetToRelay(loopExitRelay, exitId);
  }

// move LCSSA nodes to exitBlockRelay
  for (auto * block : loopExitBlocks) {

    // skip over the exit we are keeping
    if (block == loopExitRelay->block) {
      continue; // already migrated LCSSA phi to loop exit relay
    }

    // check if we need to repair any LCSSA phi nodes
    // FIXME we should really do this on the final dom tree AFTER the loop body was normalized
    for (auto it = block->begin(); isa<PHINode>(it) && it != block->end(); it = block->begin()) {
      auto * lcPhi = &cast<PHINode>(*it);
      if (!lcPhi) break;

      // for all exiting edges
      for (uint i = 0; i < lcPhi->getNumIncomingValues(); ++i) {
        assert (loop.contains(lcPhi->getIncomingBlock(i)) && "not an LCSSA Phi node");

        auto * inst = dyn_cast<Instruction>(lcPhi->getIncomingValue(i));
        if (!inst) {
          continue; // no repair necessary as the incoming value is globally available in the function
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
        assert(getIndex(header) <= defIndex && defIndex <= latchIndex && "non-dominating def not in loop");

        auto & dominatingDef = promoteDefinition(*inst, defIndex, latchIndex);

        // replace incoming value with new dominating def
        lcPhi->setIncomingValue(i, &dominatingDef);
      }

      // migrate this PHI node to the loopExitRelay
      IF_DEBUG_LIN { errs() << "\t\tMigrating " << lcPhi->getName() << " from " << lcPhi->getParent()->getName() << " to " << loopExitRelay->block->getName() << "\n"; }

    // we eliminate LCSSA Phis instead of fixing their predecessor blocks
      lcPhi->replaceAllUsesWith(lcPhi->getIncomingValue(0));
      lcPhi->eraseFromParent();
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

// create a new if-all-threads-have-left exit branch cond == rv_any(<loop live mask>)
  auto * anyThreadLiveCond = &createReduction(*liveCond, "rv_any", latch);
  BranchInst* branch = BranchInst::Create(&header, loopExitRelay->block, anyThreadLiveCond, &latch);

// mark loop and its latch exit as non-divergent
  vecInfo.setVectorShape(*branch, VectorShape::uni());
  vecInfo.setLoopDivergence(loop, false);

// Update mask analysis information.
  Value* loopExitCond = maskAnalysis.getCombinedLoopExitMask(loop);
  maskAnalysis.updateExitMasks(latch,
                                 anyThreadLiveCond,
                                 loopExitCond,
                                 &*(latch.getFirstInsertionPt()));

  return *loopExitRelay;
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

  if (vecInfo.isDivergentLoop(loop)) {
    // inherited relays from the pre-header edge: all targets except loop header
    RelayNode * exitRelay = getRelay(headId);
    if (exitRelay) {
      exitRelay = exitRelay -> next;
    }

    // convert loop into a non-divergent form
    convertToSingleExitLoop(*loop, exitRelay);
  }

  // emit all blocks within the loop (except the latch)
  int latchNodeId = processRange(loopHeadIndex, latchIndex, loop);

  // FIXME repair SSA in the loop here, AFTER loop conversion
  // repairLiveOutSSA({(val, defStart)}, destId)

  // now emit the latch (without descending into its successors)
  emitBlock(latchIndex);

  // emit loop header again to re-wire the latch to the header
  emitBlock(loopHeadIndex);

  return latchNodeId + 1; // continue after the latch
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
    auto & branch = *cast<BranchInst>(use.getUser());
    IF_DEBUG_LIN { errs() << "\t\tlinking " << branch << " opIdx " << i << "\n"; }

    // forward branches from relay to target
    branch.setOperand(i, &target);
    IF_DEBUG_LIN { errs() << "\t\t-> linked " << branch << " opIdx " << i << "\n"; }
  }

// search for a new idom
  // FIXME we can do this in lockstep with the branch fixing above for release builds
  BasicBlock * commonDomBlock = nullptr;
  for (auto itPred = pred_begin(&target); itPred != pred_end(&target); ++itPred) {
    auto * predBlock = *itPred;
    if (!commonDomBlock) { commonDomBlock = predBlock; }
    else { commonDomBlock = dt.findNearestCommonDominator(commonDomBlock, predBlock); }

    IF_DEBUG_DTFIX { errs() << "\t\t\t: dom with " << predBlock->getName() << " is " << commonDomBlock->getName() << "\n"; }

    assert(commonDomBlock && "domtree repair: did not reach a common dom node!");
  }

// domtree update: least common dominator of all incoming branches
  auto * nextCommonDom = dt.getNode(commonDomBlock);
  assert(nextCommonDom);
  IF_DEBUG_DTFIX { errs() << "DT before dom change:";dt.print(errs()); }
  IF_DEBUG_DTFIX{ errs() << "DTFIX: " << target.getName() << " idom is " << commonDomBlock->getName() << " by common pred dom\n"; }
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

void
Linearizer::processBranch(BasicBlock & head, RelayNode * exitRelay, Loop * parentLoop) {
  IF_DEBUG_LIN {
    errs() << "  processBranch : " << *head.getTerminator() << " of block " << head.getName() << "\n";
  }

  auto & term = *head.getTerminator();

  if (term.getNumSuccessors() == 0) {
    IF_DEBUG_LIN { errs() << "\t control sink.\n"; }
    return;
  }

  auto * branch = dyn_cast<BranchInst>(&term);

// Unconditional branch case
  if (!branch->isConditional()) {
    auto & nextBlock = *branch->getSuccessor(0);
    auto & relay = addTargetToRelay(exitRelay, getIndex(nextBlock));
    IF_DEBUG_LIN {
      errs() << "\tunconditional. merged with " << nextBlock.getName() << " "; dumpRelayChain(relay.id); errs() << "\n";
    }

    branch->setSuccessor(0,  relay.block);
    return;
  }

// whether this branch must be eliminated from the CFG
  assert(branch && "can only fold conditional BranchInsts (for now)");
  bool mustFoldBranch = needsFolding(*branch);

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

  if (mustFoldBranch) {
    firstRelay = &addTargetToRelay(firstRelay, secondId);
    branch->setSuccessor(secondSuccIdx, firstRelay->block);
  }

// relay the first branch to its relay block
  branch->setSuccessor(firstSuccIdx, firstRelay->block);

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
  auto & secondRelay = addTargetToRelay(exitRelay, secondId);

  // auto & secondRelay = requestRelay(secondMustHaves);
  if (!mustFoldBranch) {
    branch->setSuccessor(secondSuccIdx, secondRelay.block);
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
  if (getNumBlocks() <= 1) return;

// dump divergent branches / loops
  IF_DEBUG_LIN {
    dt.print(errs());

    errs() << "-- LIN: divergent loops/brances in the region --";
    for (int i = 0; i < getNumBlocks(); ++i) {
      auto & block = getBlock(i);
      auto * loop = li.getLoopFor(&block);

      errs() << "\n" << i << " : " << block.getName() << " , ";

      if (loop && loop->getHeader() == &block) {
        if (vecInfo.isDivergentLoop(loop)) {
           errs() << "div-loop header: " << block.getName();
        }
      }
      if (needsFolding(*block.getTerminator())) {
         errs() << "Fold : " << *block.getTerminator();
      }
    }
  }

// fold divergent branches and convert divergent loops to fix point iteration form
  linearizeControl();

// simplify branches
  cleanup();

// verify control integrity
  verify();
}

void
Linearizer::linearizeControl() {
  IF_DEBUG_LIN {  errs() << "\n-- LIN: linearization log --\n"; }

  int lastId = processRange(0, getNumBlocks(), nullptr);
  (void) lastId;

  assert(lastId  == getNumBlocks());

  IF_DEBUG_LIN {  errs() << "\n-- LIN: linearization finished --\n"; }
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
