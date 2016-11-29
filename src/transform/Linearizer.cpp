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

  for (auto & block : func) {
    // seek unprocessed blocks
    if (!inRegion(block)) continue; // FIXME we need a Region::blocks-in-the-region iterator
    if (blockIndex.count(&block)) continue;
    stack.push_back(&block);

    // process blocks
    while (!stack.empty()) {
      BasicBlock * block = stack.back(); // HINT @block overwritten deliberatly

      // check whether we are looking at the header of a loop
      Loop * filterLoop = nullptr;
      auto * loop = li.getLoopFor(block);
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
  auto & header = *loop.getHeader();
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

        // TODO repair
        assert(dt.dominates(defBlock, &latch) && "LCSSA data flow repair not yet implemented");

        // branch will start from the latch
        lcPhi->setIncomingBlock(i, &latch);
      }

      // migrate this PHI node to the loopExitRelay
      IF_DEBUG_LIN { errs() << "\t\tMigrating " << lcPhi->getName() << " from " << lcPhi->getParent()->getName() << " to " << loopExitRelay.block->getName() << "\n"; }

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

void
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

  if (!vecInfo.isDivergentLoop(loop)) {
    processDomRegion(loopHead, mustHaves, loop);

    // emit loop header again to re-wire to blocks inside the loop
    emitBlock(loopHead);

  } else {
    // convert loop into a non-divergent form
    convertToSingleExitLoop(*loop, mustHaves);

    // process nested CFG
    processDomRegion(loopHead, mustHaves, loop);

    // re-link header
    emitBlock(loopHead);

    // FIXME we have link the exitBlocks just as processBranch(,,) does
    // we don't do it and so test_030_loopmx02 fails because the second loop exit ist never branched to from the first
    // assert(loopExitRelay.empty() && "FIXME!!!");
  }
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

  for (auto itUse = itStart; itUse != itEnd; ) {
    Use & use = *(itUse++);

    int i = use.getOperandNo();
    auto & branch = *cast<BranchInst>(use.getUser());
    IF_DEBUG_LIN { errs() << "\t\tlinking " << branch << " opIdx " << i << "\n"; }

    branch.setOperand(i, &target);
  }

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
void
Linearizer::processDomRegion(BasicBlock & head, ConstBlockSet mustHaves, Loop * parentLoop) {
  IF_DEBUG_LIN {
    errs() << "processDomRegion : " << head.getName() << " {";
    for (const auto * mustHaveExit : mustHaves) {
      errs() << mustHaveExit->getName() << ", ";
    }
    errs() << "}\n";
  }

  // FIXME this is a hack to skip over the exits of normalized divergent loops
  if (parentLoop && (&head == parentLoop->getLoopLatch())) {
    IF_DEBUG_LIN { errs() << "\tconverted loop latch (skipping)\n";  }

    // emit the latch and be done with this loop
    emitBlock(head);
    return;
  }

  assert(!finishedBlocks.count(&head));

// descend into loop, if any
  auto * loop = li.getLoopFor(&head);

  if (loop != parentLoop) {
    processLoop(head, mustHaves);

  } else {
  // schedule basic block @head
    // link relay nodes to @head
    auto * headRelay = emitBlock(head);

    // fetch pending blocks beyond this block
    // we need to promote them to the branch targets
    if (headRelay) {
      for (auto * block : headRelay->destBlocks) {
        mustHaves.insert(block);
      }
    }

  // descend into all nodes that @head dominates and fold this branch, if necessary
    processBranch(head, mustHaves, parentLoop);
  }

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
    if (dt.dominates(&head, branch->getSuccessor(0))) {
      processDomRegion(*branch->getSuccessor(0), mustHaves, parentLoop);
    } else {
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

  // keep track of successor changes at this @branch
  bool branchModified = false;

  bool descendIntoFirst =
    // if the edge is within the loop and does not lead back to the header
    (!parentLoop || (parentLoop->contains(firstBlock) && firstBlock != parentLoop->getHeader())) &&
     // and we dominate the branch target
     dt.dominates(&head, firstBlock);

  if (descendIntoFirst) { // TODO OR all blocks with a lower topo index have been finished
    // if we dominate the successor we can descend into it right away
    // we know that @secondBlock will come after the domRegion of @firstBlock, a fact we could tell ::processDomRegion about to avoid superfluous relays
    processDomRegion(*firstBlock, firstMustHaves, parentLoop);

  } else {
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
    branchModified = true;
  }

// process the second successor
  bool descendIntoSecond =
    // if the edge is within the loop and does not lead back to the header
    (!parentLoop || (parentLoop->contains(secondBlock) && secondBlock != parentLoop->getHeader())) &&
     // and we dominate the branch target
     dt.dominates(&head, secondBlock);

  if (descendIntoSecond) { // TODO OR all blocks with a lower topo index have been finished
   processDomRegion(*secondBlock, mustHaves, parentLoop);

  } else {
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
      branchModified = true;
    }
  }

  if (branchModified) {
    // if this branch was modified try to simplify during cleanup() after linearization
  }

// mark branch as non-divergent
  vecInfo.setVectorShape(*branch, VectorShape::uni());
}

void
Linearizer::run() {
// initialize with a global topologic enumeration
  buildBlockIndex();

// early exit on trivial cases
  if (blocks.size() <= 1) return;

// dump divergent branches / loops
  IF_DEBUG_LIN {
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

  for (uint i = 0; i < blocks.size(); ++i) {
    auto * block = blocks[i];
    if (finishedBlocks.count(block)) continue;

    processDomRegion(*block, ConstBlockSet(), nullptr);
  }

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
