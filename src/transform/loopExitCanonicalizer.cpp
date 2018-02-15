//===- loopExitCanonicalizer.cpp ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @authors karrenberg
//



#include "rv/transform/loopExitCanonicalizer.h"

#include <stdexcept>

#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/Instructions.h>

#include "utils/rvTools.h"
#include "rvConfig.h"

using namespace llvm;


char LoopExitCanonicalizerWrapper::ID = 0;
// NOTE: The order of initialized dependencies is important
//       to prevent 'Unable to schedule' errors!
INITIALIZE_PASS_BEGIN(LoopExitCanonicalizerWrapper, "loop-exit-canonicalizer", "LoopExitCanonicalizer", false, false)
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass)
INITIALIZE_PASS_END(LoopExitCanonicalizerWrapper, "loop-exit-canonicalizer", "LoopExitCanonicalizer", false, false)

// Public interface to the LoopExitCanonicalizer pass
FunctionPass*
llvm::createLoopExitCanonicalizerPass()
{
	return new LoopExitCanonicalizerWrapper();
}



LoopExitCanonicalizer::LoopExitCanonicalizer(LoopInfo& loopInfo)
        : mLoopInfo(loopInfo)
{
}

LoopExitCanonicalizerWrapper::LoopExitCanonicalizerWrapper()
    : FunctionPass(ID)
{
    initializeLoopExitCanonicalizerWrapperPass(*PassRegistry::getPassRegistry());
}

LoopExitCanonicalizer::~LoopExitCanonicalizer()
{
}

void
LoopExitCanonicalizerWrapper::releaseMemory()
{
}

void
LoopExitCanonicalizerWrapper::getAnalysisUsage(AnalysisUsage &AU) const
{
    AU.addRequired<LoopInfoWrapperPass>();
    AU.addPreserved<LoopInfoWrapperPass>();
}

bool
LoopExitCanonicalizerWrapper::doInitialization(Module& M)
{
    // The return value presumably signals whether the module was changed or not.
    // There is no documentation on this in LLVM.
    return false;
}

bool
LoopExitCanonicalizerWrapper::doFinalization(Module& M)
{
    // The return value presumably signals whether the module was changed or not.
    // There is no documentation on this in LLVM.
    return false;
}

bool
LoopExitCanonicalizerWrapper::runOnFunction(Function& F)
{
    LoopInfo& loopInfo      = getAnalysis<LoopInfoWrapperPass>().getLoopInfo();

    LoopExitCanonicalizer canonicalizer(loopInfo);
    return canonicalizer.canonicalize(F);
}

void
LoopExitCanonicalizerWrapper::print(raw_ostream& O, const Module* M) const
{
}

bool
LoopExitCanonicalizer::canonicalize(Function& F)
{

    for (auto &L : mLoopInfo) {
        canonicalizeLoop(L);
    }

    return false;
}

void
LoopExitCanonicalizer::canonicalizeLoop(Loop* loop) const
{
    assert (loop);

    for (auto &SL : *loop) {
        canonicalizeLoop(SL);
    }

    SmallVector<BasicBlock*, 2> exitBlocks;
    loop->getExitBlocks(exitBlocks);

    for (const auto &exitBB : exitBlocks)
    {
        Loop* outerLoop = mLoopInfo.getLoopFor(exitBB);
        assert (outerLoop != loop);

        if (exitBB->getUniquePredecessor())
        {
            assert (loop->isLoopExiting(exitBB->getUniquePredecessor()));
            continue;
        }

        IF_DEBUG {
          outs() << "loop exit has more than one incoming edge: '";
          outs() << exitBB->getName() << "' - canonicalizing...\n";
        }

        SmallVector<BasicBlock*, 2> exitingBlocks;
        rv::getExitingBlocks(exitBB, mLoopInfo, exitingBlocks);

        for (auto &exitingBB : exitingBlocks)
        {
            assert (mLoopInfo.getLoopFor(exitingBB) == loop &&
                    "exiting blocks from different loop go to same exit block - not in LCSSA?");

            BasicBlock* newExitBB = createIntermediateBlock(exitingBB, exitBB);
            if (outerLoop)
            {
                outerLoop->addBasicBlockToLoop(newExitBB, mLoopInfo);
            }
        }
    }

    IF_DEBUG {
        exitBlocks.clear();
        loop->getExitBlocks(exitBlocks);
        for (const auto &exitBB : exitBlocks)
        {
            assert (exitBB->getUniquePredecessor());
        }
    }
}

BasicBlock*
LoopExitCanonicalizer::createIntermediateBlock(BasicBlock* source,
                                               BasicBlock* target) const
{
    assert (source && target);
    assert (!target->getUniquePredecessor());

    BasicBlock* newTarget = BasicBlock::Create(target->getContext(),
                                               "loop.exit.dedicated",
                                               target->getParent(),
                                               target);

    // Adjust edge.
    replaceTarget(source, target, newTarget);

    // Create edge to from new to old target.
    BranchInst::Create(target, newTarget);

    // Adjust phis.
    adjustPhis(source, target, newTarget);

    return newTarget;
}

void
LoopExitCanonicalizer::adjustPhis(BasicBlock* source,
                                  BasicBlock* target,
                                  BasicBlock* newTarget) const
{
    assert (source && target && newTarget);

    for (auto &I : *target)
    {
        if (!isa<PHINode>(I)) break;
        PHINode* phi = cast<PHINode>(&I);

        PHINode* newPhi = PHINode::Create(phi->getType(),
                                          2,
                                          "lcssa.phi",
                                          newTarget->getFirstNonPHI());

        newPhi->addIncoming(phi->getIncomingValueForBlock(source), source);

        phi->removeIncomingValue(source, false /* DeletePhiIfEmpty */);
        phi->addIncoming(newPhi, newTarget);
    }
}

void
LoopExitCanonicalizer::replaceTarget(BasicBlock* source,
                                     BasicBlock* target,
                                     BasicBlock* newTarget) const
{
    assert (source && target && newTarget);

    TerminatorInst* terminator = source->getTerminator();

    bool replaced = false; RV_UNUSED(replaced);
    for (unsigned i=0, e=terminator->getNumSuccessors(); i<e; ++i)
    {
        BasicBlock* succBB = terminator->getSuccessor(i);
        if (succBB != target) continue;

        assert (!replaced && "block must not have multiple edges going to the same target!");
        terminator->setSuccessor(i, newTarget);
        replaced = true;
    }

    assert (replaced);
}
