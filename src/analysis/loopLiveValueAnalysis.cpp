//===- loopLiveValueAnalysis.cpp ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @author karrenberg
//


#include "rv/analysis/loopLiveValueAnalysis.h"

#include <llvm/IR/Instructions.h>
#include <stdexcept>

#include "rv/rvInfo.h"
#include <rv/rvInfoProxyPass.h>

#include "utils/metadata.h"
#include "rvConfig.h"

using namespace rv;
using namespace llvm;

char LoopLiveValueAnalysisWrapper::ID = 0;
// NOTE: The order of initialized dependencies is important
//       to prevent 'Unable to schedule' errors!
INITIALIZE_PASS_BEGIN(LoopLiveValueAnalysisWrapper, "loop live value analysis", "loop live value analysis", false, false)
INITIALIZE_PASS_DEPENDENCY(RVInfoProxyPass)
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass)
INITIALIZE_PASS_END(LoopLiveValueAnalysisWrapper, "loop live value analysis", "loop live value analysis", false, false)

// Public interface to the LoopLiveValueAnalysis pass
FunctionPass*
llvm::createLoopLiveValueAnalysisPass()
{
    return new LoopLiveValueAnalysisWrapper();
}

LoopLiveValueAnalysisWrapper::LoopLiveValueAnalysisWrapper()
        : FunctionPass(ID), mAnalysis(nullptr)
{
    initializeLoopLiveValueAnalysisWrapperPass(*PassRegistry::getPassRegistry());
}

LoopLiveValueAnalysis::LoopLiveValueAnalysis(const RVInfo& rvInfo, const LoopInfo& loopInfo)
    : mInfo(rvInfo), mLoopInfo(loopInfo)
{
    for (auto &it : mLiveValueMaps)
    {
        delete it.second;
    }
}

LoopLiveValueAnalysis::~LoopLiveValueAnalysis() {
  for (auto it : mLiveValueMaps) {
    delete it.second;
  }
  for (auto it : mLoopResultMap) {
    delete it.second;
  }
}

void
LoopLiveValueAnalysisWrapper::releaseMemory()
{
}

void
LoopLiveValueAnalysisWrapper::getAnalysisUsage(AnalysisUsage &AU) const
{
    AU.addRequired<RVInfoProxyPass>();
    AU.addRequired<LoopInfoWrapperPass>();

    AU.setPreservesAll();
}

bool
LoopLiveValueAnalysisWrapper::doInitialization(Module& M)
{
    // The return value presumably signals whether the module was changed or not.
    // There is no documentation on this in LLVM.
    return false;
}

bool
LoopLiveValueAnalysisWrapper::doFinalization(Module& M)
{
    // The return value presumably signals whether the module was changed or not.
    // There is no documentation on this in LLVM.
    return false;
}

bool
LoopLiveValueAnalysisWrapper::runOnFunction(Function& F)
{
    const RVInfo& rvInfo = getAnalysis<RVInfoProxyPass>().getInfo();
    const LoopInfo& loopInfo = getAnalysis<LoopInfoWrapperPass>().getLoopInfo();

    mAnalysis = new LoopLiveValueAnalysis(rvInfo, loopInfo);

    return mAnalysis->run(F);
}

bool
LoopLiveValueAnalysis::run(Function& F)
{
    try {
        // Do stuff.
        IF_DEBUG { errs() << "analyzing function for values live across loop boundaries\n"; }
        for (auto &L : mLoopInfo)
        {
            findAllLoopLiveValues(L);
        }
        DEBUG_RV( outs() << "loop live value analysis finished.\n"; );
    }
    catch (std::logic_error& error)
    {
        errs() << "\nException occurred during LoopLiveValueAnalysis: "
        << error.what() << "\n";
        return true;
    }
    catch (...)
    {
        errs() << "\nINTERNAL ERROR: Unexpected exception occurred during "
        << "LoopLiveValueAnalysis!\n";
        return true;
    }

    return false;
}

void
LoopLiveValueAnalysisWrapper::print(raw_ostream& O, const Module*) const
{
    mAnalysis->dump(O);
}

void LoopLiveValueAnalysis::dump(raw_ostream& O)
{
    if (mLiveValueMaps.empty())
    {
        O << "\nLoop live value set is empty!\n\n";
        return;
    }

    O << "\nLoop live value sets:\n";
    for (auto &it : mLiveValueMaps)
    {
        O << "\nLoop: " << it.first->getHeader()->getName() << "\n";
        it.second->printValues();
    }
    O << "\n\n";
}

void
LoopLiveValueAnalysis::printLoopLiveValues(const Loop* loop) const
{
    assert (loop);
    assert (mLiveValueMaps.count(loop));
    mLiveValueMaps.find(loop)->second->printValues();
}

LoopLiveValueAnalysis::LoopLiveValueInfo*
LoopLiveValueAnalysis::getLiveValueInfo(const Loop* loop) const
{
    assert (loop);
    assert (mLiveValueMaps.count(loop) && "live value set for loop not found!");

    return mLiveValueMaps.find(loop)->second;
}

// Remove value from ALL loops.
void
LoopLiveValueAnalysis::removeLiveValue(Instruction* value)
{
    assert (value);

    for (auto &it : mLiveValueMaps)
    {
        if (!it.second->hasValue(value)) continue;
        it.second->remove(value);
    }
}

void
LoopLiveValueAnalysis::updateLiveValue(Instruction* value,
                                       Instruction* newValue)
{
    assert (value && newValue);

    for (auto &it : mLiveValueMaps)
    {
        if (!it.second->hasValue(value)) continue;
        it.second->update(value, newValue);
    }
}

void
LoopLiveValueAnalysis::setLoopResults(const Loop* loop, LoopResults* results)
{
    assert (loop && results);
    assert (!hasLoopResults(loop));
    mLoopResultMap[loop] = results;
}

bool
LoopLiveValueAnalysis::hasLoopResults(const Loop* loop) const
{
    assert (loop);
    return mLoopResultMap.count(loop);
}

LoopResults*
LoopLiveValueAnalysis::getLoopResults(const Loop* loop) const
{
    assert (loop);
    assert (mLoopResultMap.count(loop));
    return mLoopResultMap.find(loop)->second;
}


void
LoopLiveValueAnalysis::findAllLoopLiveValues(Loop* loop)
{
    assert (loop);

    LoopLiveValueInfo* liveValueMap = new LoopLiveValueInfo();

    findLoopLiveValues(loop, *liveValueMap);

    mLiveValueMaps[loop] = liveValueMap;

    for (auto &SL : *loop)
    {
        findAllLoopLiveValues(SL);
    }
}

void
LoopLiveValueAnalysis::findLoopLiveValues(Loop*              loop,
                                          LoopLiveValueInfo& liveValueMap)
{
    assert (loop);
    DEBUG_RV( outs() << "\ncollecting all values that are live across loop boundaries...\n"; );

    for (Loop::block_iterator BB=loop->block_begin(); BB!=loop->block_end(); ++BB)
    {
        BasicBlock* curBB = *BB;

        for (auto &I : *curBB)
        {
            // Masks are generated correctly, so we don't consider them as live values.
            if (rv::hasMetadata(&I, rv::RV_METADATA_MASK)) continue;

            Instruction* useI = findUseOutsideLoop(&I, loop);
            if (!useI) continue;

            DEBUG_RV( outs() << "  found live value: " << I << "\n"; );
            DEBUG_RV( outs() << "    with use outside loop-boundary: " << *useI << "\n"; );

            liveValueMap.insert(&I);
        }
    }

    DEBUG_RV( liveValueMap.printValues(); );
}

// Returns instruction that uses 'inst' outside the loop.
Instruction*
LoopLiveValueAnalysis::findUseOutsideLoop(Instruction* inst,
                                          const Loop*  loop) const
{
    DEBUG_RV(
        for (Instruction::use_iterator U=inst->use_begin(); U!=inst->use_end(); ++U)
        {
            Instruction* useI = cast<Instruction>(U->getUser());
            if (loop->contains(useI->getParent())) continue;
            assert (isa<PHINode>(useI) && "not in LCSSA?!");
        }
    );
    for (Instruction::use_iterator U=inst->use_begin(); U!=inst->use_end(); ++U)
    {
        Instruction* useI = cast<Instruction>(U->getUser());

        // Return the use if it is outside of the loop.
        if (!loop->contains(useI->getParent())) return useI;
    }

    return nullptr;
}


typedef LoopLiveValueAnalysis::LoopLiveValueInfo LLVI;

LLVI::LoopLiveValueInfo()
{
}

LLVI::~LoopLiveValueInfo()
{
}

void
LLVI::insert(Instruction* value)
{
    assert (value);
    mLiveValues.insert(value);
}

void
LLVI::remove(Instruction* value)
{
    assert (value);
    mLiveValues.remove(value);
}

void
LLVI::update(Instruction* value, Instruction* newValue)
{
    assert (value && newValue);
    remove(value);
    insert(newValue);
}

void
LLVI::printValues() const
{
    outs() << "\nValues live across loop boundaries:\n";
    for (const auto &it : mLiveValues)
    {
        outs() << "  * " << *it << "\n";
    }
    outs() << "\n";
}
