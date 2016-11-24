//===- cfgLinearizer.h ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @authors karrenberg, simon
//
//
#include "utils/metadata.h"
#include "utils/rvTools.h"

#include "rv/transforms/cfgLinearizer.h"
#include "rv/rvInfo.h"
#include "rv/analysis/maskAnalysis.h"
#include "rv/analysis/loopLiveValueAnalysis.h"
#include "rv/VectorizationInfoProxyPass.h"
#include "rv/vectorizationInfo.h"
#include <rv/rvInfoProxyPass.h>

#include <llvm/IR/Instructions.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/Transforms/Scalar.h> // SROA
#include <llvm/Transforms/Utils/SSAUpdater.h>
#include <llvm/Transforms/Utils/PromoteMemToReg.h>
#include <llvm/Transforms/Utils/Local.h> // DemoteRegToStack()

#include <stdexcept>
#include <analysis/SketchGraph.h>
#include <analysis/PathFinder.h>

// Copied from DemoteRegToStack.cpp
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Analysis/CFG.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Type.h"
#include "llvm/Transforms/Utils/Local.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/IR/Dominators.h"

#include "rvConfig.h"

using namespace llvm;

namespace rv {

// These are modified version of llvm::DemoteRegToStack and llvm::DemotePHIToStack from llvm/lib/Transform/Utils/DemoteRegToStack.cpp
// Different to the original versions, a store slot is created even for Instructions/PHI nodes without llvm::Users
// This is necessary, as these values may be referenced by users outside of LLVM (in particular RV's MaskGraph)


/// DemoteRegToStack - This function takes a virtual register computed by an
/// Instruction and replaces it with a slot in the stack frame, allocated via
/// alloca.  This allows the CFG to be changed around without fear of
/// invalidating the SSA information for the value.  It returns the pointer to
/// the alloca inserted to create a stack slot for I.
static
AllocaInst *
SafelyDemoteRegToStack(Instruction &I, bool VolatileLoads,
                                   Instruction *AllocaPoint) {
  // Create a stack slot to hold the value.
  AllocaInst *Slot;
  if (AllocaPoint) {
    Slot = new AllocaInst(I.getType(), nullptr,
                          I.getName()+".reg2mem", AllocaPoint);
  } else {
    Function *F = I.getParent()->getParent();
    Slot = new AllocaInst(I.getType(), nullptr, I.getName()+".reg2mem",
                          &*F->getEntryBlock().begin());
  }

  // Change all of the users of the instruction to read from the stack slot.
  while (!I.use_empty()) {
    Instruction *U = cast<Instruction>(I.user_back());
    if (PHINode *PN = dyn_cast<PHINode>(U)) {
      // If this is a PHI node, we can't insert a load of the value before the
      // use.  Instead insert the load in the predecessor block corresponding
      // to the incoming value.
      //
      // Note that if there are multiple edges from a basic block to this PHI
      // node that we cannot have multiple loads. The problem is that the
      // resulting PHI node will have multiple values (from each load) coming in
      // from the same block, which is illegal SSA form. For this reason, we
      // keep track of and reuse loads we insert.
      DenseMap<BasicBlock*, Value*> Loads;
      for (unsigned i = 0, e = PN->getNumIncomingValues(); i != e; ++i)
        if (PN->getIncomingValue(i) == &I) {
          Value *&V = Loads[PN->getIncomingBlock(i)];
          if (!V) {
            // Insert the load into the predecessor block
            V = new LoadInst(Slot, I.getName()+".reload", VolatileLoads,
                             PN->getIncomingBlock(i)->getTerminator());
          }
          PN->setIncomingValue(i, V);
        }

    } else {
      // If this is a normal instruction, just insert a load.
      Value *V = new LoadInst(Slot, I.getName()+".reload", VolatileLoads, U);
      U->replaceUsesOfWith(&I, V);
    }
  }


  // Insert stores of the computed value into the stack slot. We have to be
  // careful if I is an invoke instruction, because we can't insert the store
  // AFTER the terminator instruction.
  assert(!isa<InvokeInst>(I));
#if 0
  Instruction * InsertPt;
  if (!isa<TerminatorInst>(I)) {
    InsertPt = &I;
    ++InsertPt;
  } else {
    InvokeInst &II = cast<InvokeInst>(I);
    if (II.getNormalDest()->getSinglePredecessor())
      InsertPt = &*(II.getNormalDest()->getFirstInsertionPt());
    else {
      // We cannot demote invoke instructions to the stack if their normal edge
      // is critical.  Therefore, split the critical edge and insert the store
      // in the newly created basic block.
      unsigned SuccNum = GetSuccessorNumber(I.getParent(), II.getNormalDest());
      TerminatorInst *TI = &cast<TerminatorInst>(I);
      assert (isCriticalEdge(TI, SuccNum) &&
              "Expected a critical edge!");
      BasicBlock *BB = SplitCriticalEdge(TI, SuccNum);
      assert (BB && "Unable to split critical edge.");
      InsertPt = &*(BB->getFirstInsertionPt());
    }
  }

#if 1
  InsertPt = &*(InsertPt->getParent()->getFirstNonPHIOrDbgOrLifetime());
#else
  for (; isa<PHINode>(InsertPt) || isa<LandingPadInst>(InsertPt); ++InsertPt)
    /* empty */;   // Don't insert before PHI nodes or landingpad instrs.
#endif
#endif

  new StoreInst(&I, Slot, I.getNextNode());
  return Slot;
}

AllocaInst *
CFGLinearizer::SafelyDemotePHIToStack(PHINode *P, Instruction *AllocaPoint) {
  // Create a stack slot to hold the value.
  AllocaInst *Slot;
  if (AllocaPoint) {
    Slot = new AllocaInst(P->getType(), nullptr,
                          P->getName()+".reg2mem", AllocaPoint);
  } else {
    Function *F = P->getParent()->getParent();
    Slot = new AllocaInst(P->getType(), nullptr, P->getName()+".reg2mem",
                          &*F->getEntryBlock().begin());
  }

  // Iterate over each operand inserting a store in each predecessor.
  for (unsigned i = 0, e = P->getNumIncomingValues(); i < e; ++i) {
    if (InvokeInst *II = dyn_cast<InvokeInst>(P->getIncomingValue(i))) {
      assert(II->getParent() != P->getIncomingBlock(i) &&
             "Invoke edge not supported yet"); (void)II;
    }
    new StoreInst(P->getIncomingValue(i), Slot,
                  P->getIncomingBlock(i)->getTerminator());
  }

  // Insert a load in place of the PHI and replace all uses.
  Instruction * InsertPt = P;
#if 1
  InsertPt = &*(InsertPt->getParent()->getFirstNonPHIOrDbgOrLifetime());
#else
  for (; isa<PHINode>(InsertPt) || isa<LandingPadInst>(InsertPt); ++InsertPt)
    /* empty */;   // Don't insert before PHI nodes or landingpad instrs.
#endif

  Value *V = new LoadInst(Slot, P->getName()+".reload", InsertPt);

  // if the PHI node might be a mask (i1 type) add a fake use
  if (P->getType()->isIntegerTy(1)) {
    auto * fakeVal = createFakeUse(*V, InsertPt); // generate a fake use for this instruction so we can get a hold of it once we run mem2reg to get it back
    mvecInfo.setVectorShape(*V, mvecInfo.getVectorShape(*P));
    mvecInfo.remapPredicate(*fakeVal, *P); // remap to the fake use for now
  }

  P->replaceAllUsesWith(V);

  // Delete PHI.
  mvecInfo.dropVectorShape(*P);
  P->eraseFromParent();
  return Slot;
}

void
CFGLinearizer::remapFakeUses(Function & f) {
  std::vector<CallInst*> killList;
  for (auto & block : f) {
    for (auto & inst : block) {
      auto * call = dyn_cast<CallInst>(&inst);
      if (!call) continue;
      if (call->getCalledFunction()->getName().str() != "rv_fake") continue;
      auto & rebuildVal = *call->getOperand(0);
      mvecInfo.remapPredicate(rebuildVal, *call);
      call->replaceAllUsesWith(&rebuildVal); // just to be sure..
      killList.push_back(call);
    }
  }

  for (auto * call : killList) call->eraseFromParent();
}

Value*
CFGLinearizer::createFakeUse(Value & val, Instruction * InsertPt) {
  auto * mod = mInfo.mModule;
  auto * func = requestReductionFunc(*mod, "rv_fake");
  return CallInst::Create(func, &val, "fake", InsertPt);
}






CFGLinearizer::CFGLinearizer(const RVInfo& rvInfo,
                             LoopInfo& loopInfo,
                             MaskAnalysis& maskAnalysis,
                             const LoopLiveValueAnalysis& loopLiveValueAnalysis,
                             VectorizationInfo& vecInfo,
                             const PostDominatorTree& postDomTree,
                             const DominatorTree& domTree)
        : mInfo(rvInfo),
          mLoopInfo(loopInfo),
          mMaskAnalysis(maskAnalysis),
          mLoopLiveValueAnalysis(loopLiveValueAnalysis),
          mvecInfo(vecInfo),
          mPostDomTree(postDomTree),
          mDomTree(domTree)
{
}

CFGLinearizer::~CFGLinearizer()
{
}

Function *
CFGLinearizer::requestReductionFunc(llvm::Module & mod, const std::string & name) {
  auto * func = mod.getFunction(name);
  if (func) return func;
  auto & context = mod.getContext();
  auto * boolTy = Type::getInt1Ty(context);
  auto * funcTy = FunctionType::get(boolTy, boolTy, false);
  auto * redFunc = Function::Create(funcTy, GlobalValue::ExternalLinkage, name, &mod);
  redFunc->setDoesNotAccessMemory();
  redFunc->setDoesNotThrow();
  redFunc->setConvergent();
  redFunc->setDoesNotRecurse();
  return redFunc; // TODO add SIMD mapping
}

Instruction *
CFGLinearizer::createReduction(Value & pred, const std::string & name, BasicBlock & atEnd) {
  auto * func = requestReductionFunc(*atEnd.getParent()->getParent(), name);
  auto * call = CallInst::Create(func, &pred, "reduce", &atEnd);
  mvecInfo.setVectorShape(*call, VectorShape::uni());
  return call;
}

bool
CFGLinearizer::linearize(Function& F)
{
   auto * mod = F.getParent();

    DEBUG_RV(
            outs() << "\n#########################################################\n";
            outs() << "## CFG LINEARIZATION\n";
            outs() << "#########################################################\n";
    );

    // If an error occurred in one of the previous phases, abort.
    try {
        // Collect information about loop exits etc. that we need after linearization.
        collectLoopExitInfo(&F);
        linearize(&F);
    }
    catch (std::logic_error& error)
    {
        errs() << "\nException occurred during CFGLinearizer: "
        << error.what() << "\n";
        return true;
    }
    catch (...)
    {
        errs() << "\nINTERNAL ERROR: Unexpected exception occurred during "
        << "CFGLinearizer!\n";
        return true;
    }

#if 1
  // cleanup
    for (auto it : mLinearizeInfoMap) {
      delete it.second;
    }
    mLinearizeInfoMap.clear();

    std::set<Cluster*> freed;
    for (auto it : mClusterMap) {
      if (freed.insert(it.second).second) {
        delete it.second;
      }
    }
    mClusterMap.clear();

   for (auto it : mLoopExitInfoMap) {
     auto * lei = it.second;
     for (auto itBlock : *lei) {
       delete itBlock.second;
     }
     delete it.second;
   }
   mLoopExitInfoMap.clear();
#endif

    IF_DEBUG {
      errs() << "function after linearization:\n";
      F.print(errs());
    }

    return verify(F);
}

namespace {

bool
verifyIncomingEdges(const Function& f)
{
    for (auto &BB : f)
    {
        for (auto &I : BB)
        {
            if (!isa<PHINode>(I)) break;
            const PHINode* phi = cast<PHINode>(&I);
            if (phi->getNumIncomingValues() != rv::getNumIncomingEdges(BB))
            {
                errs() << "INTERNAL ERROR: ";
                errs() << "phi has different number of incoming edges than parent block ('";
                errs() << BB.getName() << "'): " << *phi << "\n";
                return false;
            }
            for (const_pred_iterator P=pred_begin(&BB), PE=pred_end(&BB); P!=PE; ++P)
            {
                if (phi->getBasicBlockIndex(*P) == -1)
                {
                    errs() << "INTERNAL ERROR: ";
                    errs() << "block '" << (*P)->getName() << "' has incoming edge ";
                    errs() << "that does not exist in phi: " << *phi << "\n";
                    return false;
                }
            }
            for (unsigned i=0, e=phi->getNumIncomingValues(); i<e; ++i)
            {
                BasicBlock* incBB = phi->getIncomingBlock(i);
                bool found = false;
                for (const_pred_iterator P=pred_begin(&BB), PE=pred_end(&BB); P!=PE; ++P)
                {
                    if (incBB != *P) continue;
                    found = true;
                    break;
                }
                if (!found)
                {
                    errs() << "INTERNAL ERROR: ";
                    errs() << "incoming block of phi ('" << incBB->getName() << "') ";
                    errs() << "is no incoming edge of parent block ('" << BB.getName();
                    errs() << "') of phi: " << *phi << "\n";
                    return false;
                }
            }
        }
    }

    return true;
}

// TODO: Move to utils.
bool
isNestedLoop(const Loop* innerLoop, const Loop* outerLoop)
{
    assert (innerLoop && outerLoop);
    if (innerLoop == outerLoop) return false;
    const Loop* curLoop = innerLoop;
    while (curLoop)
    {
        if (curLoop == outerLoop) return true;
        curLoop = curLoop->getParentLoop();
    }
    return false;
}


} // unnamed namespace

bool
CFGLinearizer::verify(const Function& f) const
{
    bool verified = true;

    // Test if there are any blocks without predecessor (except for entry block).
    for (auto &BB : f)
    {
        if (&BB == &f.getEntryBlock()) continue;

        if (pred_begin(&BB) == pred_end(&BB))
        {
            errs() << "INTERNAL ERROR: Block '" << BB.getName()
                << "' has no predecessors after CFG linearization!\n";
            verified = false;
        }
    }

    // Make sure all incoming edges to a block match incoming blocks of phis.
    verified &= verifyIncomingEdges(f);

    return verified;
}



void
CFGLinearizer::linearize(Function* f)
{
    assert (f);

    getDivergenceCausingBlocks(*f);
    getRewireTargets(*f);

    determineClusters(f);
    determineRewireOrders();
    determineNewEdges(f);

    MemInfoMapType   memInfos;
    MaskValueMapType maskValueMap;
    MaskValueMapType maskPhiValueMap;
    MaskValueMapType allocaValueMap;
    MaskBlockMapType maskBlockMap;
    StoreSetType     undefStoreSet;
    reg2mem(f, memInfos, maskValueMap, maskPhiValueMap, allocaValueMap, maskBlockMap, undefStoreSet);

    //DEBUG_RV( outs() << *f; );
    //DEBUG_RV( f->viewCFGOnly(); );

    DEBUG_RV(
        for (auto &MI : memInfos)
        {
            if (!MI->mTargetIsPHI) continue;
            assert (MI->mReloads->size() == 1);
            BasicBlock* parentBB = MI->mReloads->front()->getParent();

            // assert (rv::hasMetadata(parentBB, rv::RV_METADATA_OPTIONAL) == !mvecInfo.isMandatory(parentBB));

            if (!mvecInfo.isMandatory(parentBB)) continue;

            const StoreVecType& stores = *MI->mStores;
            OverwriteMapType overwriteMap;
            findOverwritingStores(stores, undefStoreSet, overwriteMap);
            if (!overwriteMap.empty())
            {
                errs() << "WARNING: Stores overwrite each other before linearization!\n";
                errs() << "         (This is most likely because of a critical edge.)\n";
                for (const auto &pair : overwriteMap)
                {
                    const StoreInst* store = pair.first;
                    outs() << "\nstore overwrites others ('" << store->getParent()->getName();
                    outs() << "'): " << *store << "\n";
                    for (const auto &ovwStore : *pair.second)
                    {
                        outs() << " * ('" << ovwStore->getParent()->getName();
                        outs() << "'): " << *ovwStore << "\n";
                    }
                }
            }
            assert (overwriteMap.empty() &&
                    "must not have overlapping paths before linearization!");
        }
    );

    // For temporary sanity check below.
    typedef SmallPtrSet<BasicBlock*, 2> TMPTYPE1;
    typedef DenseMap<BasicBlock*, TMPTYPE1*> TMPTYPE2;

    DEBUG_RV_VISIBLE(
        TMPTYPE2 tmpRetainedEdges;
        for (auto &BB : *f)
        {
            if (mLoopInfo.isLoopHeader(&BB))
            {
                tmpRetainedEdges[&BB] = new TMPTYPE1();
                for (pred_iterator P=pred_begin(&BB); P!=pred_end(&BB); ++P)
                {
                    tmpRetainedEdges[&BB]->insert(*P);
                }
                assert (tmpRetainedEdges[&BB]->size() == 2);
                continue;
            }
            // ... store more edges that should not change.
        }
    );

    // Linearize function, thereby rewiring edges that target MANDATORY blocks.
    linearize(f, memInfos, maskValueMap, maskPhiValueMap);

    // Temporary sanity check: We never change predecessors of loop headers.
    DEBUG_RV(
        for (auto &pair : tmpRetainedEdges)
        {
            BasicBlock* block = pair.first;
            for (auto BB : *pair.second)
            {
                bool found = false;
                for (pred_iterator P=pred_begin(block); P!=pred_end(block); ++P)
                {
                    if (*P != BB) continue;
                    found = true;
                    break;
                }
                assert (found);
            }
        }
    );
    DEBUG_RV_VISIBLE(
        for (auto it : tmpRetainedEdges) {
            delete it.second;
        }
    );

    //DEBUG_RV( outs() << *f; );
    //DEBUG_RV( f->viewCFGOnly(); );

    // Repair any dominance relations that were destroyed.
    DEBUG_RV( outs() << "\nRepairing SSA form after CFG linearization...\n"; );

    repairOverlappingPaths(memInfos, maskValueMap, maskPhiValueMap, allocaValueMap, maskBlockMap, undefStoreSet);
    mem2reg(f, memInfos, maskValueMap, maskPhiValueMap, allocaValueMap, maskBlockMap);

    // Update mask analysis.
    mMaskAnalysis.mapMaskValues(maskValueMap);
    mMaskAnalysis.mapMaskValues(maskPhiValueMap);

    DEBUG_RV( outs() << "\nLinearization of function finished!\n"; );

    // We invalidate insert points of masks since they may have been removed.
    mMaskAnalysis.invalidateInsertPoints();
}

// Collect information about loop exits etc. that we need after linearization.
void
CFGLinearizer::collectLoopExitInfo(Function* f)
{
    assert (f);

    SmallVector<Loop*, 2> loopStack;
    for (LoopInfo::iterator L=mLoopInfo.begin(), LE=mLoopInfo.end(); L!=LE; ++L)
    {
        loopStack.push_back(*L);
    }

    while (!loopStack.empty())
    {
        Loop* loop = loopStack.pop_back_val();

        for (auto &SL : *loop)
        {
            loopStack.push_back(SL);
        }

        // assert (!rv::hasMetadata(loop, rv::RV_METADATA_LOOP_DIVERGENT_TRUE) == !mvecInfo.isDivergentLoop(loop));

        if (!mvecInfo.isDivergentLoop(loop)) continue;

        LoopExitMapType* loopExitMap = mLoopExitInfoMap[loop];
        if (!loopExitMap)
        {
            loopExitMap = new LoopExitMapType();
            mLoopExitInfoMap[loop] = loopExitMap;
        }

        SmallVector<BasicBlock*, 2> exitBlocks;
        loop->getExitBlocks(exitBlocks);
        for (auto &exitBB : exitBlocks)
        {
            BasicBlock* exitingBB = exitBB->getUniquePredecessor();
            assert (exitingBB && "loop not canonicalized?!");

            // assert (rv::hasMetadata(exitingBB->getTerminator(), rv::RV_METADATA_OP_UNIFORM) == mvecInfo.getVectorShape(*exitingBB->getTerminator()).isUniform());
            // assert (rv::hasMetadata(exitBB, rv::RV_METADATA_MANDATORY) == mvecInfo.isMandatory(exitBB));

            const bool targetIsMandatory = mvecInfo.isMandatory(exitBB);
            const bool isUniform = mvecInfo.getVectorShape(*exitingBB->getTerminator())
                                           .isUniform();

            // If the exit branch is uniform, we do not maintain an exit mask
            // since it would be equal to the loop active mask.
            const bool needLoopExitMasks = !isUniform;

            // The exiting block mask is given by the exit mask of the edge.
            Value* exitMask = needLoopExitMasks ?
                mMaskAnalysis.getLoopExitMaskUpdate(*loop, *exitingBB) : // disj. of all x updates?
                mMaskAnalysis.getExitMask(*exitingBB, *exitBB);

            // The header mask of the mandatory exit block is given by the
            // combined exit mask phi. Since this doesn't exist, we first have
            // to join the exit mask phis of all mandatory exits.
            // The header mask of each optional exit block is given by the loop
            // mask phi (the active mask).
            Value* headerMask = needLoopExitMasks ?
                mMaskAnalysis.getLoopExitMaskPhi(*loop, *exitingBB) :
                mMaskAnalysis.getLoopMaskPhi(*loop);

            LoopExitInfo* LEI = new LoopExitInfo(exitBB,
                                                 exitingBB,
                                                 isUniform,
                                                 targetIsMandatory,
                                                 exitMask,
                                                 headerMask);
            (*loopExitMap)[exitingBB] = LEI;
        }

        LoopLiveValueAnalysis::LoopLiveValueInfo& liveValues =
            *mLoopLiveValueAnalysis.getLiveValueInfo(loop);

        if (liveValues.empty()) continue;

        // Get loop result information.
        LoopResults& loopResults = *mLoopLiveValueAnalysis.getLoopResults(loop);

        // TODO: We basically duplicate the information from loopLiveValueAnalysis here.
        // TODO: This is not used anyway at the moment because we repair every incoming
        //       value of every phi and blend in repairSSA().
        for (auto &exitBB : exitBlocks)
        {
            BasicBlock* exitingBB = exitBB->getUniquePredecessor();
            LoopExitInfo* LEI = (*loopExitMap)[exitingBB];
            for (auto &I : *exitBB)
            {
                if (!isa<PHINode>(I)) break;
                PHINode* phi = cast<PHINode>(&I);
                for (unsigned i=0, e=phi->getNumIncomingValues(); i<e; ++i)
                {
                    for (auto &liveValue : liveValues)
                    {
                        if (liveValue != phi->getIncomingValue(i)) continue;
                        LEI->mExitLiveVals.push_back(liveValue);
                        LEI->mLatchLiveVals.push_back(loopResults.getResult(liveValue));
                        LEI->mHeaderLiveVals.push_back(loopResults.getResultPhi(liveValue));
                    }
                }
            }
        }
    }

#if 0
    for (auto &BB : f)
    {
        BlockInfo* LBI = new BlockInfo(&BB);
        mBlockMap[&BB] = LBI;

        for (pred_iterator P=pred_begin(&BB), PE=pred_end(&BB); P!=PE; ++P)
        {
            LBI->mOriginalPredecessors.insert(*P);
        }
        TerminatorInst* terminator = BB.getTerminator();
        for (unsigned i=0, e=terminator->getNumSuccessors(); i<e; ++i)
        {
            LBI->mOriginalSuccessors.insert(terminator->getSuccessor(i));
        }
    }
#endif
}



////////////////////////////////////////////////////////////////////////////////
void
CFGLinearizer::determineClusters(Function* f)
{
    SetVector<BasicBlock*> dcBlockSet;

    DEBUG_RV( outs() << "\ndc blocks:\n"; );
    for (auto &BB : *f)
    {
        if (isa<ReturnInst>(BB.getTerminator())) continue;

        // assert(!rv::hasMetadata(BB.getTerminator(), rv::RV_METADATA_OP_VARYING) == !mvecInfo.getVectorShape(*BB.getTerminator()).isVarying());

        if (mvecInfo.getVectorShape(*BB.getTerminator()).isUniform()) continue;
        dcBlockSet.insert(&BB);
    }

    typedef std::vector<BasicBlock*> VecType;
    DEBUG_RV_NO_VERBOSE(
        for (auto &BB : *f)
        {
            VecType dcBlocks = mDivergenceCauseMap[&BB];
            for (auto &dcBB : dcBlocks)
            {
                assert (dcBlockSet.count(cast<BasicBlock>(dcBB)));
            }
        }
    );

    if (dcBlockSet.empty()) return;

    createClusters(dcBlockSet);

    DEBUG_RV(
        std::set<const Cluster*> cSet;
        for (const auto pair : mClusterMap) cSet.insert(pair.second);
        outs() << "\nClusters:\n";
        for (const auto &cluster : cSet)
        {
            outs() << "Cluster '" << cluster->mEntry->getName() << "':\n";
            outs() << "  post dom: '" << cluster->mPostDom->getName() << "'\n";
            outs() << "  dc blocks:";
            for (const auto dcBB : *cluster->mDCBlocks)
            {
                outs() << " '" << dcBB->getName() << "'";
            }
            outs() << "\n";
            outs() << "  rewire target set:";
            for (const auto &rtBB : *cluster->mRewireTargets)
            {
                outs() << " '" << rtBB->getName() << "'";
            }
            outs() << "\n";
        }
        outs() << "\nClusterMap:\n";
        for (const auto &pair : mClusterMap)
        {
            const BasicBlock* block = pair.first;
            const Cluster* cluster  = pair.second;
            outs() << "Block '" << block->getName() << "' -> Cluster '";
            outs()  << cluster->mEntry->getName() << "'\n";
        }
        outs() << "\n";
    );

    DEBUG_RV_NO_VERBOSE(
        for (const auto &BB : dcBlockSet)
        {
            DEBUG_RV( if (!mClusterMap.count(BB)) outs() << BB->getName() << "\n"; );
            assert (mClusterMap.count(BB));
        }
    );

    DEBUG_RV_NO_VERBOSE(
        for (auto &BB : *f)
        {
            if (isa<ReturnInst>(BB.getTerminator())) continue;
            // assert (!rv::hasMetadata(BB.getTerminator(), rv::RV_METADATA_OP_VARYING) == mvecInfo.getVectorShape(*BB.getTerminator()).isUniform());
            if (mvecInfo.getVectorShape(*BB.getTerminator()).isUniform()) continue;
            DEBUG_RV( if (!mClusterMap.count(&BB)) outs() << BB.getName() << "\n"; );
            assert (mClusterMap.count(&BB));
        }
    );
}

void
CFGLinearizer::createClusters(SetVector<BasicBlock*>& dcBlockSet)
{
    assert (!dcBlockSet.empty());
    assert (mClusterMap.empty());
    DEBUG_RV( outs() << "\ncreateClusters()\n"; );

    DominatorTreeBase<BasicBlock> PTB(true);
    PTB.recalculate(*(*dcBlockSet.begin())->getParent());

    // Create a cluster for each DC block and initialize.
    for (auto BB : dcBlockSet)
    {
        Cluster* cluster = new Cluster();
        cluster->mEntry = BB;
        cluster->mPostDom = PTB.getNode(BB)->getIDom()->getBlock();
        cluster->mDCBlocks = new SmallPtrSet<BasicBlock*, 2>();
        cluster->mDCBlocks->insert(BB);
        cluster->mRewireTargets = new SmallPtrSet<BasicBlock*, 2>();

        // Initialize unordered rewire target set.
        std::vector<BasicBlock*> rewireTargets = mRewireTargetMap[BB];

        for (auto RT : rewireTargets)
        {
            cluster->mRewireTargets->insert(RT);
        }

        // Ordered rewire target list is filled in determineRewireOrder().
        cluster->mRewireList = new RewireList();
        mClusterMap[BB] = cluster;
    }

    bool changed = true;
    while (changed)
    {
        changed = false;
        for (auto BB0 : dcBlockSet)
        {
            Cluster* cluster0 = mClusterMap[BB0];
            assert (cluster0);
            //Loop* loop = mLoopInfo->getLoopFor(BB0);

            SmallPtrSet<BasicBlock*, 2> remapSet;
            for (auto BB1 : dcBlockSet)
            {
                if (BB0 == BB1) continue;

                // If BB1 already belongs to cluster0, do not merge.
                Cluster* cluster1 = mClusterMap[BB1];
                assert (cluster1);
                if (cluster0 == cluster1) continue;

                // If BB1 is the post dominator of cluster0, do not merge.
                if (cluster0->mPostDom == BB1) continue;

                // If BB1 is not reachable from BB0, do not merge.
                // Disallow passing post dominator, allow loop exits, disallow any back edges.
                if (!rv::isReachable(BB1, BB0, cluster0->mPostDom, nullptr, nullptr, true, &mLoopInfo)) continue;

                assert (!rv::isReachable(BB0, BB1, cluster1->mPostDom, nullptr, nullptr, true, &mLoopInfo));
                assert (cluster0->mPostDom == cluster1->mPostDom ||
                        rv::isReachable(cluster0->mPostDom, cluster1->mPostDom));

                DEBUG_RV( outs() << "  '" << BB1->getName() << "' can be reached from '"; );
                DEBUG_RV( outs() << BB0->getName() << "', merging clusters...\n"; );

                // BB1 can be reached from BB0.
                // -> Merge cluster1 into cluster0
                cluster0->mDCBlocks->insert(cluster1->mDCBlocks->begin(),
                                            cluster1->mDCBlocks->end());
                cluster0->mRewireTargets->insert(cluster1->mRewireTargets->begin(),
                                                 cluster1->mRewireTargets->end());

                // Remap all blocks that referenced cluster1 to cluster0.
                for (auto pair : mClusterMap)
                {
                    if (pair.second != cluster1) continue;
                    remapSet.insert(pair.first);
                }

                // Delete cluster1.
                delete cluster1;
                mClusterMap[BB1] = cluster0;//erase(BB1);

                changed = true;
                break;
            }

            for (auto BB : remapSet)
            {
                assert (mClusterMap[BB]->mDCBlocks->count(BB));
                mClusterMap[BB] = cluster0;
            }

            if (changed) break;
        }
    }
}

////////////////////////////////////////////////////////////////////////////////
void
CFGLinearizer::determineRewireOrders()
{
    SmallPtrSet<Cluster*, 2> visitedSets;
    for (auto &pair : mClusterMap)
    {
        Cluster* cluster = pair.second;
        if (visitedSets.count(cluster)) continue;
        visitedSets.insert(cluster);
        determineRewireOrder(*cluster);
    }

    // Remove clusters without rewire targets (e.g. MND loop exits of loops with only this exit).
    SmallPtrSet<BasicBlock*, 2> deleteSet;
    for (auto &pair : mClusterMap)
    {
        Cluster* cluster  = pair.second;
        if (!cluster->mRewireList->empty()) continue;
        assert (cluster->mDCBlocks->size() == 1);
        deleteSet.insert(pair.first);
    }
    for (auto BB : deleteSet)
    {
        delete mClusterMap[BB];
        mClusterMap.erase(BB);
    }

    DEBUG_RV(
        std::set<Cluster*> cSet;
        for (auto pair : mClusterMap) cSet.insert(pair.second);
        outs() << "\nClusters:\n";
        for (auto &cluster : cSet)
        {
            outs() << "Cluster '" << cluster->mEntry->getName() << "':\n";
            outs() << "  post dom: '" << cluster->mPostDom->getName() << "'\n";
            outs() << "  dc blocks:";
            for (const auto &dcBB : *cluster->mDCBlocks)
            {
                outs() << " '" << dcBB->getName() << "'";
            }
            outs() << "\n";
            outs() << "  rewire target set:";
            for (const auto &rtBB : *cluster->mRewireTargets)
            {
                outs() << " '" << rtBB->getName() << "'";
            }
            outs() << "\n";
            outs() << "  rewire target list:";
            for (const auto &rtBB : *cluster->mRewireList)
            {
                outs() << " '" << rtBB->getName() << "'";
            }
            outs() << "\n";
        }
        outs() << "\n";
    );
}

void
CFGLinearizer::determineRewireOrder(Cluster& cluster)
{
    assert (cluster.mRewireList->empty());

#if 1
    BasicBlock* startBB = &cluster.mEntry->getParent()->getEntryBlock();
#elif 0
    // This could prove to be more efficient for large functions, since we do not
    // traverse the entire function for every cluster.
    DominatorTreeBase<BasicBlock>* DTB = new DominatorTreeBase<BasicBlock>(false);
    DTB->recalculate(*cluster.mEntry->getParent());
    BasicBlock* startBB = DTB->getNode(cluster.mPostDom)->getIDom()->getBlock();
    assert (startBB);
    assert (startBB == cluster.mEntry ||
            rv::hasMetadata(startBB->getTerminator(), rv::RV_METADATA_OP_UNIFORM));

    Loop* clusterLoop = mLoopInfo->getLoopFor(cluster.mEntry);
    while (clusterLoop && clusterLoop->getParentLoop())
    {
        clusterLoop = clusterLoop->getParentLoop();
    }
    if (clusterLoop)
    {
        startBB = DTB->findNearestCommonDominator(startBB, clusterLoop->getHeader());
    }
#elif 0
    // This would be cleaner, but is problematic because we do not want to allow back edges.
    BasicBlock* startBB = cluster.mEntry;
#endif

    SmallPtrSet<BasicBlock*, 16> scheduledBlocks;
    typedef SmallVector<BasicBlock*, 32> WorkList;
    WorkList workList;
    workList.push_back(startBB);

    while (!workList.empty())
    {
        BasicBlock* block = workList.pop_back_val();

        if (scheduledBlocks.count(block)) continue;
        scheduledBlocks.insert(block);
        DEBUG_RV( outs() << "determineRewireOrder(" << block->getName() << ")\n"; );

        Loop* loop = mLoopInfo.getLoopFor(block);
        const bool isHeader = loop && loop->getHeader() == block;
        //const bool postDomIsOutside = loop && !loop->contains(cluster.mPostDom);

        // If this is a non-header MANDATORY block, check if it is MND because
        // of a block of this cluster. This is not necessarily the case, e.g.
        // when clusters overlap without the start blocks being reachable from
        // each other. Thus, the current block is only a rewire target for this
        // cluster, if one of the cluster blocks causes it to be MND.
        if (!isHeader && cluster.mRewireTargets->count(block))
        {
            // assert (rv::hasMetadata(block, rv::RV_METADATA_MANDATORY));
            assert (mvecInfo.isMandatory(block));
            cluster.mRewireList->push_back(block);
        }

        if (block == cluster.mPostDom) continue;

        // If this is a header of a loop, and the post dominator where we
        // want to stop is outside, add loop exits to the worklist.
        if (isHeader)
        {
            SmallVector<BasicBlock*, 2> exitBlocks;
            loop->getExitBlocks(exitBlocks);
            for (auto &exitBB : exitBlocks) // "E"
            {
                BasicBlock* exitingBB = exitBB->getUniquePredecessor(); // "X"
                assert (exitingBB && "loop not simplified?!");

                // Determine whether this is the outermost loop that is left.
                if (loop->getParentLoop() &&
                    loop->getParentLoop()->isLoopExiting(exitingBB))
                {
                    continue; // It is not.
                }

                workList.push_back(exitBB);
            }
        }

        TerminatorInst* TI = block->getTerminator();
        for (unsigned i=0, e=TI->getNumSuccessors(); i<e; ++i)
        {
            BasicBlock* succBB = TI->getSuccessor(i);

            // If the successor is on a higher nesting level, this is a loop exit.
            Loop* targetLoop = mLoopInfo.getLoopFor(succBB);
            if (loop)
            {
                if (loop->isLoopExiting(block) && targetLoop != loop) continue;
                // Also ignore back edges.
                if (loop->getHeader() == succBB && loop->getLoopLatch() == block) continue;
            }

            if (hasUnseenNonLatchPred(succBB, cluster, mLoopInfo, scheduledBlocks)) continue;
            workList.push_back(succBB);
        }
    }
}

bool
CFGLinearizer::hasUnseenNonLatchPred(BasicBlock*                         block,
                                     const Cluster&                      cluster,
                                     const LoopInfo&                     loopInfo,
                                     const SmallPtrSet<BasicBlock*, 16>& scheduledBlocks)
{
    // Do not recurse into successors for which we have not yet
    // visited all incoming edges (except for loop headers).
    // Ignore predecessors which are additional (uniform) entries
    // to the cluster (= can not be reached from cluster entry).
    SmallVector<const BasicBlock*, 2> unseenPreds;
    for (pred_iterator P=pred_begin(block), PE=pred_end(block); P!=PE; ++P)
    {
        BasicBlock* predBB = *P;
        if (predBB == block) continue;
        if (scheduledBlocks.count(predBB)) continue;
        unseenPreds.push_back(predBB);
        if (unseenPreds.size() > 1) return true;
    }

    if (unseenPreds.empty()) return false;
    assert (unseenPreds.size() == 1);

    const bool unseenPredIsLoopLatch = loopInfo.getLoopFor(block) &&
        loopInfo.getLoopFor(block)->getLoopLatch() == unseenPreds.front();

    assert (!unseenPredIsLoopLatch || loopInfo.isLoopHeader(block));
    return !unseenPredIsLoopLatch;
}

////////////////////////////////////////////////////////////////////////////////
void
CFGLinearizer::determineNewEdges(Function* f)
{
    for (auto &BB : *f)
    {
        determineNewEdges(&BB);
    }

    DEBUG_RV(
        outs() << "\nLinearize info:\n";
        for (auto &pair : mLinearizeInfoMap)
        {
            BasicBlock* block = pair.first;
            auto& edgeInfos = *pair.second;

            outs() << "Block '" << block->getName() << "':\n";
            for (auto &LI : edgeInfos)
            {
                outs() << "  old successor : '";
                outs() << (LI->mSuccessor ? LI->mSuccessor->getName() : "") << "'\n";
                outs() << "  edge type     : " << LI->mEdgeType << "\n";
                outs() << "  new successors:";
                for (auto &BB : *LI->mNewTargets)
                {
                    outs() << " '" << BB->getName() << "'";
                }
                outs() << "\n";
                outs() << "  rewire-causing:";
                for (auto &BB : *LI->mNewTargetPreds)
                {
                    if (!BB) continue;
                    outs() << " '" << BB->getName() << "'";
                }
                outs() << "\n";
            }
        }
    );
}

void
CFGLinearizer::determineNewEdges(BasicBlock* block)
{
    assert (block);
    DEBUG_RV( outs() << "determineNewEdges('" << block->getName() << "')\n"; );

    Loop* sourceLoop = mLoopInfo.getLoopFor(block);
    TerminatorInst* TI = block->getTerminator();
    for (unsigned i=0, e=TI->getNumSuccessors(); i<e; ++i)
    {
        BasicBlock* succBB = TI->getSuccessor(i);

        Loop* targetLoop = mLoopInfo.getLoopFor(succBB);

        SmallVector<BasicBlock*, 2>* newTargets = new SmallVector<BasicBlock*, 2>();
        SmallVector<BasicBlock*, 2>* newTargetPreds = new SmallVector<BasicBlock*, 2>();
        OutgoingEdgeType edgeType = LEAVE_UNTOUCHED;
        if (!mvecInfo.isMandatory(succBB) ||
            (targetLoop && targetLoop->getHeader() == succBB))
        {
            newTargets->push_back(succBB);
            newTargetPreds->push_back(nullptr);
        }
        else
        {
            const bool isLoopExit = sourceLoop &&
                                    sourceLoop->isLoopExiting(block) &&
                                    targetLoop != sourceLoop;
            if (isLoopExit)
            {
                // assert (!rv::hasMetadata(sourceLoop, rv::RV_METADATA_LOOP_DIVERGENT_TRUE) == !mvecInfo.isDivergentLoop(sourceLoop));

                // There can be cases where a non-divergent inner loop is left via an exit
                // to the latch of a divergent outer loop (which is MANDATORY). That exit
                // must not be removed. See e.g. LoopNested3UniformInnerLoop.
                if (!mvecInfo.isDivergentLoop(sourceLoop))
                {
                    edgeType = LEAVE_UNTOUCHED;
                }
                else
                {
                    edgeType = REMOVE;
                }
            }
            else
            {
                getRewireTargets(block, succBB, *newTargets, *newTargetPreds);
                edgeType = newTargets->size() > 1 ? REWIRE_MULTI : REWIRE;
            }
        }

        LinearizeInfo* info = new LinearizeInfo(succBB,
                                                edgeType,
                                                newTargets,
                                                newTargetPreds);
        if (!mLinearizeInfoMap.count(block))
        {
            mLinearizeInfoMap[block] = new SmallVector<LinearizeInfo*, 2>();
        }
        mLinearizeInfoMap[block]->push_back(info);
    }

    const bool isLatch = sourceLoop &&
                         sourceLoop->getLoopLatch() == block;
    if (!isLatch) return;

    // assert (!rv::hasMetadata(sourceLoop, rv::RV_METADATA_LOOP_DIVERGENT_TRUE) == !mvecInfo.isDivergentLoop(sourceLoop));

    if (!mvecInfo.isDivergentLoop(sourceLoop)) return;

    SmallVector<BasicBlock*, 2>* newTargets = new SmallVector<BasicBlock*, 2>();
    SmallVector<BasicBlock*, 2>* newTargetPreds = new SmallVector<BasicBlock*, 2>();
    BasicBlock* firstExit = getFirstMandatoryLoopExit(sourceLoop);
    if (!firstExit)
    {
        // If there is no appropriate exit block for this loop,
        // rewire to the latch of the next outer loop.
        Loop* parentLoop = sourceLoop->getParentLoop();
        assert (parentLoop);
        firstExit = parentLoop->getLoopLatch();
    }

    newTargets->push_back(firstExit);
    newTargetPreds->push_back(nullptr);
    LinearizeInfo* info = new LinearizeInfo(nullptr,
                                            NEW,
                                            newTargets,
                                            newTargetPreds);
    if (!mLinearizeInfoMap.count(block))
    {
        mLinearizeInfoMap[block] = new SmallVector<LinearizeInfo*, 2>();
    }
    mLinearizeInfoMap[block]->push_back(info);
}

BasicBlock*
CFGLinearizer::getFirstMandatoryLoopExit(Loop* loop)
{
    assert (loop);
    // assert (rv::hasMetadata(loop, rv::RV_METADATA_LOOP_DIVERGENT_TRUE));
    assert (mvecInfo.isDivergentLoop(loop));

    SmallVector<BasicBlock*, 2> exitBlocks;
    loop->getExitBlocks(exitBlocks);
    //for (auto &exitBB : exitBlocks) // "E"
    for (auto rit=exitBlocks.rbegin(), RE=exitBlocks.rend(); rit!=RE; ++rit)
    {
        BasicBlock* exitBB = *rit;
        BasicBlock* exitingBB = exitBB->getUniquePredecessor(); // "X"
        assert (exitingBB && "loop not simplified?!");

        if (!mvecInfo.isMandatory(exitBB)) continue;

        // Determine whether this is the outermost loop that is left.
        if (loop->getParentLoop() &&
            loop->getParentLoop()->isLoopExiting(exitingBB))
        {
            continue; // It is not.
        }

        return exitBB;
    }

    return nullptr;
}

void
CFGLinearizer::getRewireTargets(BasicBlock* block,
                                BasicBlock* succBB,
                                SmallVector<BasicBlock*, 2>& rewireTargets,
                                SmallVector<BasicBlock*, 2>& rewireCausingBlocks)
{
    assert (block && succBB);
    // assert (rv::hasMetadata(succBB, rv::RV_METADATA_MANDATORY));
    assert (mvecInfo.isMandatory(succBB));
    assert (rewireTargets.empty());
    assert (rewireCausingBlocks.empty());

    std::vector<BasicBlock*> dcBlocks = mDivergenceCauseMap[succBB];

    DEBUG_RV_NO_VERBOSE(
        for (auto &dcBB : dcBlocks)
        {
            assert (mClusterMap.count(dcBB));
        }
    );

    // Since the DC blocks do not contain direct predecessors of varying
    // branches, add them now.
    for (auto predBB : predecessors(succBB))
    {
        TerminatorInst* TI = predBB->getTerminator();
        if (isa<ReturnInst>(TI)) continue;

        // assert (!rv::hasMetadata(TI, rv::RV_METADATA_OP_VARYING) == !mvecInfo.getVectorShape(*TI).isVarying());

        if (mvecInfo.getVectorShape(*TI).isUniform()) continue;
        dcBlocks.push_back(predBB);
    }

    // This is only necessary because loop latches do not store dc blocks.
    if (dcBlocks.empty())
    {
        rewireTargets.push_back(succBB);
        return;
    }

    // Find out which disjoint clusters these dc-blocks belong to.
    // Store them deterministically in order.
    SmallPtrSet<Cluster*, 2> clusterSet;
    SmallVector<Cluster*, 2> clusters;
    for (auto &dcBB : dcBlocks)
    {
        // There may be blocks without cluster associated if they do
        // not require any rewiring (e.g. loop exits of loops with one exit).
        if (!mClusterMap.count(cast<BasicBlock>(dcBB))) continue;
        Cluster* cluster = mClusterMap[cast<BasicBlock>(dcBB)];
        if (clusterSet.count(cluster)) continue;
        clusterSet.insert(cluster);
        clusters.push_back(cluster);
    }

    // Get next rewire target for each of the clusters.
    for (auto cluster : clusters)
    {
        assert (rv::isReachable(succBB,
                                 cluster->mEntry,
                                 cluster->mPostDom,
                                 nullptr,
                                 mLoopInfo.getLoopFor(cluster->mEntry)));

        RewireList& rewireList = *cluster->mRewireList;
        BasicBlock* newTarget = nullptr;

        bool findNext = true;
        RewireList::iterator it = rewireList.begin();
        while (it != rewireList.end())
        {
            BasicBlock* rewireBlock = *it++;
            Loop* rewireLoop = mLoopInfo.getLoopFor(rewireBlock);
            const bool isReachable = rv::isReachable(block,
                                                      rewireBlock,
                                                      cluster->mPostDom,
                                                      rewireLoop,
                                                      rewireLoop);

            if (!findNext)
            {
                if (isReachable) findNext = true;
                continue;
            }

            if (isReachable) continue;
            Loop* origSuccLoop  = mLoopInfo.getLoopFor(succBB);
            Loop* newTargetLoop = mLoopInfo.getLoopFor(rewireBlock);
            if (origSuccLoop != newTargetLoop) continue;
            newTarget = rewireBlock;
            findNext = false;
        }
        assert (newTarget);

        rewireTargets.push_back(newTarget);
        rewireCausingBlocks.push_back(cluster->mEntry); // required when creating switch
    }

    // If there are multiple clusters from which this block can be reached,
    // but only some of them are divergence-causing, we still need to retain
    // paths for the other clusters. These paths simply retain the original
    // edge, i.e., their rewire target is the original successor.
    // We have to make this explicit, because otherwise the edge may only be
    // rewired to the one target. This may result in additional dynamic tests
    // where control flow came from.
    SmallPtrSet<Cluster*, 2> otherClusters;
    for (auto &pair : mClusterMap)
    {
        Cluster* cluster = pair.second;
        if (clusterSet.count(cluster)) continue;
        if (!rv::isReachable(succBB,
                              cluster->mEntry,
                              cluster->mPostDom,
                              nullptr,
                              nullptr,
                              true,
                              &mLoopInfo))
        {
            continue;
        }
        otherClusters.insert(cluster);
    }

    for (auto cluster : otherClusters)
    {
        rewireTargets.push_back(succBB);
        rewireCausingBlocks.push_back(cluster->mEntry);
    }
}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////


CFGLinearizer::BlockInfo::BlockInfo(BasicBlock* block)
: mBlock(block)
{
}

CFGLinearizer::LoopExitInfo::LoopExitInfo(BasicBlock* exitBB,
                                          BasicBlock* exitingBB,
                                          const bool  isUniform,
                                          const bool  isTargetMandatory,
                                          Value*      exitMask,
                                          Value*      headerMask)
: mExitBB(exitBB),
    mExitingBB(exitingBB),
    mIsUniform(isUniform),
    mIsTargetMandatory(isTargetMandatory),
    mExitMask(exitMask),
    mHeaderMask(headerMask)
{
}

void
CFGLinearizer::linearize(Function*         f,
                         MemInfoMapType&   memInfos,
                         MaskValueMapType& maskValueMap,
                         MaskValueMapType& maskPhiValueMap)
{
    // Get a list of the different clusters (ordered, since we want to be
    // deterministic).
    SmallPtrSet<Cluster*, 2> clusterSet;
    SmallVector<Cluster*, 2> clusters;
    for (auto &BB : *f)
    {
        if (!mClusterMap.count(&BB)) continue;
        Cluster* cluster = mClusterMap[&BB];
        if (clusterSet.count(cluster)) continue;
        clusters.push_back(cluster);
        clusterSet.insert(cluster);
    }

    // Create alloca in each "header" of divergence-causing block clusters so
    // that we can identify where we were coming from during execution.
    AllocaInst* idxAlloca = nullptr;
    LoadVecType* reloads  = nullptr;
    DenseMap<BasicBlock*, unsigned> indexMap;
    if (!clusters.empty())
    {
        // Create alloca
        Function* parentFn = clusters[0]->mEntry->getParent();
        Instruction* allocaPos = &*(parentFn->getEntryBlock().getFirstInsertionPt());
        idxAlloca = new AllocaInst(Type::getInt32Ty(*mInfo.mContext),
                                   nullptr,
                                   "alloca.idx",
                                   allocaPos);
        // rv::setMetadata(idxAlloca, rv::RV_METADATA_OP_UNIFORM);
        mvecInfo.setVectorShape(*idxAlloca, VectorShape::uni());

        // Create one store per predecessor of new target (divergence-causing block)
        //  - stored value: identifier of new target
        reloads = new LoadVecType();
        StoreVecType* stores  = new StoreVecType();
        unsigned clusterIdx = 0;
        for (auto cluster : clusters)
        {
            ConstantInt* idxVal = ConstantInt::get(*mInfo.mContext, APInt(32, clusterIdx));
            StoreInst* store = new StoreInst(idxVal, idxAlloca, cluster->mEntry->getTerminator());
            stores->push_back(store);
            // rv::setMetadata(store, rv::RV_METADATA_OP_UNIFORM);
            mvecInfo.setVectorShape(*store, VectorShape::uni());
            indexMap[cluster->mEntry] = clusterIdx++;
        }

        // There is no "origInst", but we can not supply a nullptr either since
        // this already indicates a phi.
        memInfos.push_back(new MemInfo(idxAlloca, idxAlloca, reloads, stores));
    }

    // TODO keep cfg valid
    // Now, linearize/modify CFG.
    for (auto &BB : *f)
    {
        BasicBlock* block = &BB;
        if (!mLinearizeInfoMap.count(block)) continue;
        SmallVector<LinearizeInfo*, 2>& edgeInfos = *mLinearizeInfoMap[block];

        DEBUG_RV( outs() << "\nrewiring edges of block '" << block->getName() << "'...\n"; );

        TerminatorInst* terminator = block->getTerminator();
        if (isa<ReturnInst>(terminator))
        {
            DEBUG_RV( outs() << "  is return block.\n"; );
            continue; // Nothing to do (no outgoing edges).
        }
        assert (terminator->getNumSuccessors() > 0);

        unsigned numLeaveUntouched = 0;
        unsigned numRemove = 0;
        unsigned numRewire = 0;
        unsigned numRewireMulti = 0;
        unsigned numNew = 0;

        LinearizeInfo* removeInfo = nullptr;
        LinearizeInfo* newExitInfo = nullptr;
        for (const auto &targetInfo : edgeInfos)
        {
            DEBUG_RV(
                outs() << "  successor '";
                outs() << (targetInfo->mSuccessor ? targetInfo->mSuccessor->getName() : "new");
                outs() << "'\n";
            );
            switch (targetInfo->mEdgeType)
            {
                case LEAVE_UNTOUCHED:
                {
                    ++numLeaveUntouched;
                    break;
                }
                case REMOVE:
                {
                    ++numRemove;
                    assert (numRemove == 1 && "must not attempt to remove multiple edges!");
                    removeInfo = targetInfo;
                    break;
                }
                case REWIRE:
                {
                    ++numRewire;

                    BasicBlock* target    = targetInfo->mSuccessor;
                    BasicBlock* newTarget = targetInfo->mNewTargets->front();
                    assert (targetInfo->mNewTargets->size() == 1);
                    if (target == newTarget) break;

                    // Rewire edge.
                    // NOTE: This does not influence iteration over successors.
                    terminator = rewireEdge(terminator, target, newTarget);

                    // Immediately replace conditional branches that have both
                    // edges point to the same block by an unconditional branch.
                    if (BranchInst* branch = dyn_cast<BranchInst>(terminator))
                    {
                        if (branch->isConditional() &&
                            branch->getSuccessor(0) == branch->getSuccessor(1))
                        {
                            BasicBlock * firstSuccBlock = branch->getSuccessor(0);
                            mvecInfo.dropVectorShape(*terminator);
                            terminator->eraseFromParent();
                            // TODO keep cfg valid
                            terminator = BranchInst::Create(firstSuccBlock, block);
                            // rv::setMetadata(terminator, rv::RV_METADATA_OP_UNIFORM);
                            mvecInfo.setVectorShape(*terminator, VectorShape::uni());
                        }
                    }
                    else if (SwitchInst* sw = dyn_cast<SwitchInst>(terminator))
                    {
                        // Do we need/want to simplify this?
                        SmallPtrSet<BasicBlock*, 2> uniqueTargets;
                        SmallPtrSet<BasicBlock*, 2> duplicateTargets;
                        for (SwitchInst::CaseIt it=sw->case_begin(),
                             E=sw->case_end(); it!=E; ++it)
                        {
                            BasicBlock* caseTarget = it.getCaseSuccessor();
                            if (uniqueTargets.count(caseTarget))
                            {
                                duplicateTargets.insert(caseTarget);
                                continue;
                            }
                            uniqueTargets.insert(caseTarget);
                        }
                        // Case values < 0 signal old edges that will be
                        // or were already rewired. If the block is already
                        // a target of the switch, the rewiring is already
                        // done, and we can remove the edge (if the edge
                        // targeted that block before already, it was not
                        // or will not be touched, so removing the edge
                        // does not break anything).
                        // We can only remove one case at a time...
                        bool changed = true;
                        while (changed)
                        {
                            changed = false;
                            for (SwitchInst::CaseIt it=sw->case_begin(),
                                 E=sw->case_end(); it!=E; ++it)
                            {
                                BasicBlock* caseTarget = it.getCaseSuccessor();
                                if (duplicateTargets.count(caseTarget) &&
                                    it.getCaseValue()->getSExtValue() < 0)
                                {
                                    sw->removeCase(it);
                                    changed = true;
                                    break;
                                }
                            }
                        }

                        const unsigned numCases = sw->getNumCases();
                        if (numCases == 2)
                        {
                            // Successor 0 is the default block.
                            BasicBlock* defaultBB = sw->getDefaultDest();
                            assert (sw->getDefaultDest()->getName().startswith("rv_default"));
                            assert (defaultBB == sw->getSuccessor(0));

                            BasicBlock* target0 = sw->getSuccessor(1);
                            BasicBlock* target1 = sw->getSuccessor(2);
                            ConstantInt* caseVal0 = sw->findCaseDest(target0);
                            Value* condition = sw->getCondition();
                            assert (caseVal0->getType()->isIntegerTy(32));
                            assert (condition->getType()->isIntegerTy(32));
                            // TODO keep cfg valid
                            ICmpInst* cmp = new ICmpInst(sw,
                                                         ICmpInst::ICMP_EQ,
                                                         condition,
                                                         caseVal0,
                                                         "rv.switch.cond");
                            // rv::setMetadata(cmp, rv::RV_METADATA_OP_UNIFORM);
                            mvecInfo.setVectorShape(*cmp, VectorShape::uni());

                            mvecInfo.dropVectorShape(*sw);
                            sw->eraseFromParent();
                            mvecInfo.dropPredicate(*defaultBB);
                            defaultBB->eraseFromParent();
                            // TODO keep cfg valid
                            terminator = BranchInst::Create(target0, target1, cmp, block);
                            // rv::setMetadata(terminator, rv::RV_METADATA_OP_UNIFORM);
                            mvecInfo.setVectorShape(*terminator, VectorShape::uni());
                        }
                    }
                    else
                    {
                        assert (false && "unsupported terminator found!");
                    }

                    break;
                }
                case REWIRE_MULTI:
                {
                    ++numRewireMulti;
                    //assert (numRewireMulti == 1 && "not implemented!");
                    assert (idxAlloca && reloads);

                    // This only works for uniform terminators:
                    // If the terminator was varying, and we break the edge, the new
                    // block should become the rewire target of blocks from the other
                    // side. I don't think this is easy to implement this way ->
                    // disallowing critical edges in the first place is much cleaner.
                    //assert (rv::hasMetadata(terminator, rv::RV_METADATA_OP_UNIFORM));
                    // NOTE: Apparently, it *does* work... at least for our current
                    //       test suite.

                    // If the source block of this multi-rewire edge does not end with
                    // an unconditional branch, we have to introduce a new block on this
                    // edge from which edges go out to the different rewire targets.
                    // NOTE: This is equivalent to breaking critical edges beforehand.
                    BasicBlock* oldTarget = targetInfo->mSuccessor;
                    // TODO keep cfg valid
                    BasicBlock* ceBlock = BasicBlock::Create(*mInfo.mContext,
                                                             block->getName()+"."+oldTarget->getName()+".mrewire",
                                                             oldTarget->getParent(),
                                                             oldTarget);

                    // rv::setMetadata(ceBlock, rv::RV_METADATA_OPTIONAL);

                    // TODO keep cfg valid
                    // Change target block of current edge.
                    for (unsigned i=0, e=terminator->getNumSuccessors(); i<e; ++i)
                    {
                        BasicBlock* succBB = terminator->getSuccessor(i);
                        if (succBB != oldTarget) continue;
                        terminator->setSuccessor(i, ceBlock);
                    }

                    // In the new block, we now create the conditional branch that tests
                    // where we came from.

                    // Create reload of cluster index in current block
                    // Create switch in current block
                    //  - jump to rewire target that corresponds to cluster index
                    //  - if the edge did not require a rewire for a cluster index,
                    //    the old target block is jumped to (as default of the switch).
                    LoadInst* load = new LoadInst(idxAlloca, "reload.idx", ceBlock);
                    reloads->push_back(load);

                    const unsigned numCases = targetInfo->mNewTargets->size();
                    // TODO keep cfg valid
                    SwitchInst* sw = SwitchInst::Create(load, oldTarget, numCases, ceBlock);

                    // rv::setMetadata(load, rv::RV_METADATA_OP_UNIFORM);
                    // rv::setMetadata(sw,   rv::RV_METADATA_OP_UNIFORM);
                    // rv::setMetadata(terminator,   rv::RV_METADATA_OP_UNIFORM);
                    mvecInfo.setVectorShape(*load, VectorShape::uni());
                    mvecInfo.setVectorShape(*sw,   VectorShape::uni());
                    mvecInfo.setVectorShape(*terminator,   VectorShape::uni());

                    for (unsigned i=0, e=numCases; i<e; ++i)
                    {
                        BasicBlock* target      = (*targetInfo->mNewTargets)[i];
                        BasicBlock* clusterHead = (*targetInfo->mNewTargetPreds)[i];

                        assert (indexMap.count(clusterHead));
                        const unsigned clusterIdx = indexMap[clusterHead];
                        ConstantInt* idxVal = ConstantInt::get(*mInfo.mContext, APInt(32, clusterIdx));
                        sw->addCase(idxVal, target);
                    }

                    //DEBUG_RV( outs() << "  new load: " << *load << "\n"; );
                    //DEBUG_RV( outs() << "  new switch: " << *sw << "\n"; );
                    //DEBUG_RV( outs() << " REWIRE_MULTI done!\n"; );

                    break;
                }
                case NEW:
                {
                    ++numNew;
                    assert (numNew == 1 && "must not attempt to remove multiple edges!");
                    newExitInfo = targetInfo;
                    break;
                }
                default:
                {
                    assert (false && "unknown edge type found!");
                }
            }
        }

        // We must not remove any branch before all rewiring has been done.
        if (numRemove)
        {
            assert (removeInfo);
            assert (removeInfo->mSuccessor);
            assert (removeInfo->mNewTargets->empty());
            BasicBlock* removeTarget = removeInfo->mSuccessor;

            // Check if a loop shall be removed and update loopinfo if so
            Loop* L = mLoopInfo.getLoopFor(removeTarget);
            if (L && L->getLoopLatch() == terminator->getParent())
            {
                mLoopInfo.markAsRemoved(L);
            }

            if (BranchInst* branch = dyn_cast<BranchInst>(terminator))
            {
                // Update mask analysis information.
                mMaskAnalysis.removeExitMask(*block, *removeTarget);

                // TODO keep cfg valid
                BranchInst* newBranch = removeTarget == branch->getSuccessor(0) ?
                    BranchInst::Create(branch->getSuccessor(1), block) :
                    BranchInst::Create(branch->getSuccessor(0), block);

                // rv::setMetadata(newBranch, rv::RV_METADATA_OP_UNIFORM);
                mvecInfo.setVectorShape(*newBranch, VectorShape::uni());

                mvecInfo.dropVectorShape(*branch);
                branch->eraseFromParent();
            }
            else if (SwitchInst* sw = dyn_cast<SwitchInst>(terminator))
            {
                if (sw->getDefaultDest() == removeTarget)
                {
                    // TODO keep cfg valid
                    BasicBlock* defaultBB = BasicBlock::Create(*mInfo.mContext,
                                                               "default",
                                                               removeTarget->getParent(),
                                                               block);
                    UnreachableInst* uinst = new UnreachableInst(*mInfo.mContext, defaultBB);
                    // rv::setMetadata(uinst,  rv::RV_METADATA_OP_UNIFORM);
                    mvecInfo.setVectorShape(*uinst, VectorShape::uni());
                    sw->setDefaultDest(defaultBB);
                }
                else
                {
                    // TODO keep cfg valid
                    ConstantInt* caseVal = sw->findCaseDest(removeTarget);
                    SwitchInst::CaseIt it = sw->findCaseValue(caseVal);
                    sw->removeCase(it);
                }
            }
            else
            {
                assert (false && "unsupported terminator found!");
            }
        }

        if (numNew)
        {
            assert (newExitInfo);
            assert (!newExitInfo->mSuccessor);
            assert (newExitInfo->mNewTargets->size() == 1);
            const Loop* loop = mLoopInfo.getLoopFor(block);
            assert (loop && loop->getLoopLatch() == block);
            BasicBlock* exitTarget = newExitInfo->mNewTargets->front();
            createSingleVaryingLoopExitEdge(loop,
                                            block,
                                            exitTarget,
                                            memInfos,
                                            maskValueMap,
                                            maskPhiValueMap);
        }

        for (auto &targetInfo : edgeInfos)
        {
            delete targetInfo;
        }
        //delete pair.second; // edgeInfos
        delete mLinearizeInfoMap[block];
        mLinearizeInfoMap.erase(block);
    }
}

// This method is required to replace exactly one edge
// and not change the order of the edges. This way, the
// caller can still iterate over the terminator's
// successors. Thus, it is also required to not change
// the number of successors.
TerminatorInst*
CFGLinearizer::rewireEdge(TerminatorInst* terminator,
                          BasicBlock*     oldTarget,
                          BasicBlock*     newTarget)
{
    assert (terminator && oldTarget && newTarget);
    assert (terminator->getParent());
    assert (oldTarget != newTarget);

    BasicBlock* parentBB = terminator->getParent();

    // Check if a loop shall be removed and update loopinfo if so
    Loop* L = mLoopInfo.getLoopFor(oldTarget);
    if (L && L->getLoopLatch() == parentBB)
    {
        mLoopInfo.markAsRemoved(L);
    }

    // TODO Can back edges, thus new loops be introduced?

    if (BranchInst* br = dyn_cast<BranchInst>(terminator))
    {
        if (br->isUnconditional())
        {
            mvecInfo.dropVectorShape(*br);
            br->eraseFromParent();
            // TODO keep cfg valid
            BranchInst* newBr = BranchInst::Create(newTarget, parentBB);
            // rv::setMetadata(newBr, rv::RV_METADATA_OP_UNIFORM);
            mvecInfo.setVectorShape(*newBr, VectorShape::uni());
            return newBr;
        }
        else
        {
            const bool trueIsTarget = br->getSuccessor(0) == oldTarget;
            // TODO keep cfg valid
            BranchInst* newBr =
                    BranchInst::Create(trueIsTarget ? newTarget : br->getSuccessor(0),
                                       trueIsTarget ? br->getSuccessor(1) : newTarget,
                                       br->getCondition(),
                                       parentBB);
            rv::copyMetadata(newBr, *br);
            if (mvecInfo.isMetadataMask(br)) mvecInfo.markMetadataMask(newBr);
            mvecInfo.setVectorShape(*newBr, mvecInfo.getVectorShape(*br));
            mvecInfo.dropVectorShape(*br);
            br->eraseFromParent();
            return newBr;
        }
    }

    if (SwitchInst* sw = dyn_cast<SwitchInst>(terminator))
    {
        bool rewired = false; RV_UNUSED(rewired);
        // getNumCases() returns number of cases without default.
        // getSuccessor() returns blocks including default (index 0).
        for (unsigned i=0, e=sw->getNumCases()+1; i<e; ++i)
        {
            BasicBlock* caseTarget = sw->getSuccessor(i);
            if (caseTarget != oldTarget) continue;
            // TODO keep cfg valid
            sw->setSuccessor(i, newTarget);
            rewired = true;
        }
        assert (rewired);
        return sw;
    }

    assert (false && "unsupported terminator found!");
    return nullptr;
}

void
CFGLinearizer::createSingleVaryingLoopExitEdge(const Loop*       loop,
                                               BasicBlock*       block,
                                               BasicBlock*       exitTarget,
                                               MemInfoMapType&   memInfos,
                                               MaskValueMapType& maskValueMap,
                                               MaskValueMapType& maskPhiValueMap)
{
    assert (loop && block && exitTarget);
    // assert (rv::hasMetadata(loop, rv::RV_METADATA_LOOP_DIVERGENT_TRUE));
    assert (mvecInfo.isDivergentLoop(loop));

    DEBUG_RV( outs() << "  block is latch of DIVERGENT loop, "
        "inserting all-false-exit edge...\n"; );

    TerminatorInst* terminator = block->getTerminator();
    assert (terminator->getNumSuccessors() == 1);

    BasicBlock* headerBB = loop->getHeader();
    Value* cond = mMaskAnalysis.getExitMask(*block, *headerBB);

    // Since we do reg2mem before linearization, this mask may not exist
    // anymore. We add an additional use to it, so the memInfo object also has
    // to be updated.
    const bool inValueMap = maskValueMap.count(cond);
    if (inValueMap || maskPhiValueMap.count(cond))
    {
        Value* maskAlloca = inValueMap ? maskValueMap[cond] : maskPhiValueMap[cond];
        assert (isa<AllocaInst>(maskAlloca));
        cond = new LoadInst(maskAlloca, "reload.mask", block->getTerminator());

        // Add this reload to the meminfo object of the mask alloca.
        bool added = false; RV_UNUSED(added);
        for (auto &MI : memInfos)
        {
            if (MI->mAlloca != maskAlloca) continue;
            MI->mReloads->push_back(cast<LoadInst>(cond));
            added = true;
            break;
        }
        assert (added);
    }

    // the branch condition is divergent and so we need to communicate to the backend what the branch condition is going to be in vectorized code
    // the legacy pipeline did this by knowing implicitely (through metadata) that this is a loop exit
    // we add an explicit rv_any() reduction on the mask here to make this explicit
    // this way the backend does not need to know about predication in loops and linearization at all
#define MASK_REDUCTION

#ifdef MASK_REDUCTION
    cond = createReduction(*cond, "rv_any", *block);
#endif

    // Jump back to header on "true".
    // TODO keep cfg valid
    // adding an exit edge should not change the loop
    BranchInst* branch = BranchInst::Create(headerBB, exitTarget, cond, block);

    // rv::setMetadata(branch, rv::RV_METADATA_OP_UNIFORM);

    mvecInfo.setVectorShape(*branch, VectorShape::uni());

    // Update mask analysis information.
    // The exit condition is the combined exit mask. This is always correct
    // and includes cases where there are still uniform loop exits: Their
    // corresponding masks will always be entirely false until the exit is
    // taken (in which case this exit condition here will not be changed
    // again).
    // TODO: Can this cause problems with former users of the exit mask?
    //       Or is this exit mask only used during vectorization of the branch?
    // TODO: This seems to be wrong anyway... unless this is the combined mask
    //       of all *persisted* exit masks (i.e., it holds all instances that
    //       left the loop through a MANDATORY exit in any iteration).
    Value* loopExitCond = mMaskAnalysis.getCombinedLoopExitMask(*loop);
    mMaskAnalysis.updateExitMasks(*block,
                                   cond,
                                   loopExitCond,
                                   &*(block->getFirstInsertionPt()));

    DEBUG_RV(
        outs() << "  new edge: '" << block->getName()
        << "' -> '" << exitTarget->getName() << "'\n";
    );

    mvecInfo.dropVectorShape(*terminator);
    terminator->eraseFromParent();
}



namespace {

// From Reg2Mem.cpp
bool
valueEscapes(const Instruction& Inst)
{
    const BasicBlock *BB = Inst.getParent();
    for (Value::const_use_iterator UI = Inst.use_begin(),
         E = Inst.use_end(); UI != E; ++UI)
    {
        const Instruction *I = cast<Instruction>(UI->getUser());
        if (I->getParent() != BB || isa<PHINode>(I))
            return true;
    }
    return false;
}

} // unnamed namespace



void
CFGLinearizer::reg2mem(Function*         f,
                       MemInfoMapType&   memInfos,
                       MaskValueMapType& maskValueMap,
                       MaskValueMapType& maskPhiValueMap,
                       MaskValueMapType& allocaValueMap,
                       MaskBlockMapType& maskBlockMap,
                       StoreSetType&     undefStoreSet)
{
    assert (f);

    std::vector<Instruction*> instVec;
    for (auto &BB : *f)
    {
        for (auto &I : BB)
        {
            // Ignore alloca's in entry block.
            if (isa<AllocaInst>(I) && &BB == &f->getEntryBlock()) continue;
            // Ignore values that do not live out of the block.
            if (!valueEscapes(I)) continue;

            instVec.push_back(&I);
        }
    }

    Instruction* allocaPos = f->getEntryBlock().getFirstNonPHI();

    // Demote escaped instructions.
    for (auto &inst : instVec)
    {
        DEBUG_RV( outs() << "\nreg2mem: " << *inst << "\n"; );
        DEBUG_RV( outs() << "  block        ('" << inst->getParent()->getName() << "')\n"; );

        bool hasLoopHeaderPhiUse = false;
        BasicBlock* parentBB = inst->getParent();
        Loop* parentLoop = mLoopInfo.getLoopFor(parentBB);
        if (parentLoop && !inst->getName().startswith("loopMaskUpdate"))
        {
            // Search for a use that is a phi in a loop header which has the current inst
            // as its incoming value from the latch.
            for (Value::use_iterator U=inst->use_begin(), UE=inst->use_end(); U!=UE; ++U)
            {
                if (!isa<PHINode>(U->getUser())) continue;
                PHINode* usePhi = cast<PHINode>(U->getUser());
                BasicBlock* useParentBB = usePhi->getParent();
                Loop* useParentLoop = mLoopInfo.getLoopFor(useParentBB);
                if (!useParentLoop || useParentLoop->getHeader() != useParentBB) continue;
                BasicBlock* latchBB = useParentLoop->getLoopLatch();
                if (usePhi->getIncomingValueForBlock(latchBB) != inst) continue;
                hasLoopHeaderPhiUse = true;
                break;
            }
        }

        assert(mvecInfo.hasKnownShape(*inst));
        AllocaInst* alloca = SafelyDemoteRegToStack(*inst, false, allocaPos);
#if 0
        if (!alloca) continue;
#endif
        rv::copyMetadata(alloca, *inst); // Store metadata in alloca.
        mvecInfo.setVectorShape(*alloca, mvecInfo.getVectorShape(*inst));
        if (mvecInfo.isMetadataMask(inst)) mvecInfo.markMetadataMask(alloca);

        DEBUG_RV( outs() << "  alloca       ('" << allocaPos->getParent()->getName(); );
        DEBUG_RV( outs() << "'): " << *alloca << "\n"; );

        // If this is a mask, we have to update the mask analysis.
        // Similar to metadata, we use the alloca and store a value mapping.
        // This mapping is used after mem2reg.
        const bool isMask = mvecInfo.isMetadataMask(inst);
        if (isMask)
        {
            allocaValueMap[alloca] = inst;
            maskValueMap[inst] = alloca;
            maskBlockMap[inst] = parentBB;
        }

        LoadVecType*  reloads = new LoadVecType();
        StoreVecType* stores  = new StoreVecType();
        for (Value::use_iterator U=alloca->use_begin(), UE=alloca->use_end(); U!=UE; ++U)
        {
            Instruction* useI = cast<Instruction>(U->getUser());
            assert (isa<StoreInst>(useI) || isa<LoadInst>(useI));
            if (StoreInst* store = dyn_cast<StoreInst>(useI))
            {
                DEBUG_RV( outs() << "  new store    ('" << useI->getParent()->getName(); );
                DEBUG_RV( outs() << "'): " << *useI << "\n"; );
                stores->push_back(store);
            }
            else
            {
                DEBUG_RV( outs() << "  new reload   ('" << useI->getParent()->getName(); );
                DEBUG_RV( outs() << "'): " << *useI << "\n"; );
                reloads->push_back(cast<LoadInst>(useI));
            }
        }
        assert (stores->size() == 1);
        memInfos.push_back(new MemInfo(inst, alloca, reloads, stores));

        // If this value is used in an LCSSA phi of a MANDATORY exit, we have to
        // move the corresponding reload to the latch of the outermost loop that
        // is left to make sure that the updated value from the last iteration
        // is used (the result vec blend in the latch).
        for (auto &reload : *reloads)
        {
            BasicBlock* exitingBB = reload->getParent();
            Loop* reloadLoop = mLoopInfo.getLoopFor(exitingBB);
            if (!reloadLoop) continue;

            for (Value::use_iterator U=reload->use_begin(), UE=reload->use_end(); U!=UE; ++U)
            {
                if (!isa<PHINode>(U->getUser())) continue;
                PHINode* lcssaPhi = cast<PHINode>(U->getUser());
                BasicBlock* exitBB = lcssaPhi->getParent();
                if (rv::isExitOfDivergentLoop(*exitBB, mLoopInfo, mvecInfo) &&
                    mvecInfo.isMandatory(exitBB))
                {
                    Loop* outermostLoop = rv::getOutermostExitedLoop(*exitBB, mLoopInfo);
                    assert (outermostLoop);
                    BasicBlock* outermostLatch = outermostLoop->getLoopLatch();
                    if (outermostLatch == exitingBB) continue;
                    reload->moveBefore(outermostLatch->getTerminator());
                    DEBUG_RV( outs() << "  moved reload ('" << outermostLatch->getName(); );
                    DEBUG_RV( outs() << "'): " << *reload << "\n"; );
                    break;
                }
            }
        }

        // If there is no use as the value from the latch in a loop header phi,
        // and all defs and uses are inside some loop, move the alloca to the header.
        // Store "undef" directly after the alloca to prevent persisting the value
        // across loop iterations (or "false" in case of a mask).
        // NOTE: This actually looks like a workaround for a shortcoming of SSAUpdater
        //       that ignores where the alloca is located (thus, moving is not
        //       necessary, since it is ignored anyway).
        if (!hasLoopHeaderPhiUse)
        {
            if (const Loop* loop = getInnermostLoopForAlloca(*reloads, *stores))
            {
                Instruction* insertBefore = loop->getHeader()->getFirstNonPHI();
                Value* val = isMask ?
                    Constant::getNullValue(alloca->getAllocatedType()) :
                    UndefValue::get(alloca->getAllocatedType());
                StoreInst* lastStore = new StoreInst(val, alloca, insertBefore);
                stores->push_back(lastStore);
                undefStoreSet.insert(lastStore);
                DEBUG_RV( outs() << "  generated undef store: "; );
                DEBUG_RV( outs() << loop->getHeader()->getName() << "\n"; );
                //alloca->moveBefore(insertBefore);
                //DEBUG_RV( outs() << "  moved alloca to header: "; );
                //DEBUG_RV( outs() << loop->getHeader()->getName() << "\n"; );
            }
        }
    }

    // Find all phi's.
    instVec.clear();
    for (auto &BB : *f)
    {
        // Ignore loop header phis (predecessors never change).
        if (mLoopInfo.isLoopHeader(&BB)) continue;
        for (auto &I : BB)
        {
            if (!isa<PHINode>(I)) continue;
            instVec.push_back(&I);
        }
    }

    // Demote phi nodes
    for (auto &inst : instVec)
    {
        PHINode* phi = cast<PHINode>(inst);

        BasicBlock* parentBB = phi->getParent();
        Loop* parentLoop = mLoopInfo.getLoopFor(parentBB);

        bool hasLoopHeaderPhiUse = false;
        if (parentLoop)
        {
            // Search for a use that is a phi in a loop header which has the current inst
            // as its incoming value from the latch.
            for (Value::use_iterator U=phi->use_begin(), UE=phi->use_end(); U!=UE; ++U)
            {
                if (!isa<PHINode>(U->getUser())) continue;
                PHINode* usePhi = cast<PHINode>(U->getUser());
                BasicBlock* useParentBB = usePhi->getParent();
                Loop* useParentLoop = mLoopInfo.getLoopFor(useParentBB);
                if (!useParentLoop || useParentLoop->getHeader() != useParentBB) continue;
                BasicBlock* latchBB = useParentLoop->getLoopLatch();
                if (usePhi->getIncomingValueForBlock(latchBB) != phi) continue;
                hasLoopHeaderPhiUse = true;
                break;
            }
        }

        DEBUG_RV( outs() << "\nphi2mem: " << phi << *phi << "\n"; );
        DEBUG_RV( outs() << "  block        ('" << phi->getParent()->getName() << "')\n"; );
        DEBUG_RV( outs() << "  addr         ('" << (void*) phi << "')\n"; );

        const bool isMask = mvecInfo.isMetadataMask(phi);
        Instruction* dummy = rv::createDummy(phi->getType(), allocaPos);
        rv::copyMetadata(dummy, *phi); // Store metadata in tmp inst.
        if (mvecInfo.isMetadataMask(phi)) mvecInfo.markMetadataMask(dummy);
        auto shape = mvecInfo.getVectorShape(*phi);

        AllocaInst* alloca = SafelyDemotePHIToStack(phi, allocaPos);
#if 0
        if (!alloca) {
        	// work around:
        	maskPhiValueMap[phi] = nullptr;
        	maskValueMap[phi] = nullptr;
            dummy->eraseFromParent();
            continue;
        }
#endif

        rv::copyMetadata(alloca, *dummy); // Store metadata in alloca.
        if (mvecInfo.isMetadataMask(dummy)) mvecInfo.markMetadataMask(alloca);
        mvecInfo.setVectorShape(*alloca, shape);
        dummy->eraseFromParent();
        DEBUG_RV( outs() << "  alloca       ('" << allocaPos->getParent()->getName(); );
        DEBUG_RV( outs() << "'): " << *alloca << "\n"; );

        if (isMask) {
            // inst is not valid anymore, but we only need the pointer values.
            // TODO: This screams for problems, use some simple identifier instead.
            // NOTE: The entry in maskBlockMap may already exist (added during inst demotion),
            //       but that does not hurt. It would hurt, though, if we did not add it ;).
            allocaValueMap[alloca] = phi;
            maskPhiValueMap[phi] = alloca;
            maskBlockMap[phi] = parentBB;
        }

        LoadVecType*  reloads = new LoadVecType();
        StoreVecType* stores  = new StoreVecType();
        for (Value::use_iterator U=alloca->use_begin(), UE=alloca->use_end(); U!=UE; ++U)
        {
            Instruction* useI = cast<Instruction>(U->getUser());
            assert (isa<StoreInst>(useI) || isa<LoadInst>(useI));
            if (StoreInst* store = dyn_cast<StoreInst>(useI))
            {
                DEBUG_RV( outs() << "  phi store    ('" << useI->getParent()->getName(); );
                DEBUG_RV( outs() << "'): " << *useI << "\n"; );
                stores->push_back(store);
            }
            else
            {
                DEBUG_RV( outs() << "  phi reload   ('" << useI->getParent()->getName(); );
                DEBUG_RV( outs() << "'): " << *useI << "\n"; );
                reloads->push_back(cast<LoadInst>(useI));
            }
        }
        assert (reloads->size() == 1);
        memInfos.push_back(new MemInfo(nullptr, alloca, reloads, stores));

        // If this is an LCSSA phi of a MANDATORY exit, we have to move its
        // store from the exiting block to the latch to make sure that the
        // updated value from the last iteration is used (the result vec
        // blend in the latch).
        if (rv::isExitOfDivergentLoop(*parentBB, mLoopInfo, mvecInfo) &&
            mvecInfo.isMandatory(parentBB))
        {
            BasicBlock* predBB = parentBB->getUniquePredecessor();
            assert (predBB);
            Loop* predLoop = rv::getOutermostExitedLoop(*parentBB, mLoopInfo);
            assert (predLoop);
            BasicBlock* predLatch = predLoop->getLoopLatch();
            if (predBB != predLatch)
            {
                assert (stores->size() == 1);
                StoreInst* store = stores->front();
                store->moveBefore(predLatch->getTerminator());
                DEBUG_RV( outs() << "  moved store  ('" << predLatch->getName(); );
                DEBUG_RV( outs() << "'): " << *store << "\n"; );
            }
        }

        if (!hasLoopHeaderPhiUse)
        {
            if (const Loop* loop = getInnermostLoopForAlloca(*reloads, *stores))
            {
                Instruction* insertBefore = loop->getHeader()->getFirstNonPHI();
                Value* val = isMask ?
                    Constant::getNullValue(alloca->getAllocatedType()) :
                    UndefValue::get(alloca->getAllocatedType());
                StoreInst* lastStore = new StoreInst(val, alloca, insertBefore);
                stores->push_back(lastStore);
                undefStoreSet.insert(lastStore);
                DEBUG_RV( outs() << "  generated undef store: "; );
                DEBUG_RV( outs() << loop->getHeader()->getName() << "\n"; );
                //alloca->moveBefore(insertBefore);
                //DEBUG_RV( outs() << "  moved alloca to header: "; );
                //DEBUG_RV( outs() << loop->getHeader()->getName() << "\n"; );
            }
        }
    }
}

void
CFGLinearizer::repairOverlappingPaths(MemInfoMapType&   memInfos,
                                      MaskValueMapType& maskValueMap,
                                      MaskValueMapType& maskPhiValueMap,
                                      MaskValueMapType& allocaValueMap,
                                      MaskBlockMapType& maskBlockMap,
                                      StoreSetType&     undefStoreSet)
{
    // Repair overlapping paths:
    // If some of the original incoming edges do not exist anymore,
    // we have to introduce a select operation for each removed one.
    // To make sure that the incoming values of these selects are also properly
    // dominated, we rewire each store whose block is not a direct predecessor
    // of the phi's parent block anymore to its own alloca.
    // We then have to find out which store succeeds another one. At each of
    // these positions, we reload the preceeding store's alloca, blend the
    // values, and store to the succeeding store's alloca.
    // At the "lowest" store of such a path (in the direct predecessor of the
    // phi's parent block), we store to the phi's alloca directly.
    //
    // Note that we also have to create reloads of the required masks' alloca's.
    // During all this, we possibly have to create additional alloca's etc.,
    // which are added to the memInfos vector afterwards.
    SmallVector<MemInfo*, 8> newMemInfos;
    for (auto &MI : memInfos)
    {
        if (!MI->mTargetIsPHI) continue;

        const StoreVecType& stores  = *MI->mStores;
        const LoadVecType&  reloads = *MI->mReloads;
        assert (reloads.size() == 1);
        LoadInst* reload = reloads.front();
        BasicBlock* parentBB = reload->getParent();

        // Ignore loop header phis (can never have overlapping paths).
        if (mLoopInfo.isLoopHeader(parentBB)) continue;

        // Ignore phis in OPTIONAL blocks (overlapping paths are okay if there
        // is a critical edge on the neighboring path).
        // TODO: HERE! This results in no fixup select ever getting introduced (in unit tests)!!
        if (!mvecInfo.isMandatory(parentBB)) continue;

        DEBUG_RV( outs() << "\nrepair overlapping paths to phi reload: " << *reload << "\n"; );
        DEBUG_RV( outs() << "  block        ('" << parentBB->getName() << "')\n"; );

        DEBUG_RV(
            std::set<BasicBlock*> predBlocks;
            for (pred_iterator P=pred_begin(parentBB); P!=pred_end(parentBB); ++P)
            {
                predBlocks.insert(*P);
            }
            outs() << "  pred blocks:\n";
            for (const auto &BB : predBlocks)
            {
                outs() << "   * " << BB->getName() << "\n";
            }
            outs() << "  stores:\n";
            for (const auto &st : stores)
            {
                outs() << "   * " << st->getParent()->getName() << ": " << *st << "\n";
            }
        );

        // Sanity check: We expect each store of a phi to be in a different block.
        DEBUG_RV(
            std::set<const BasicBlock*> tmpSet;
            for (const auto &store : stores)
            {
                if (undefStoreSet.count(store)) continue;
                const BasicBlock* parentBB = store->getParent();
                assert (!tmpSet.count(parentBB) && "expected each store in a different block!");
                tmpSet.insert(parentBB);
            }
        );

        // First, we have to find out which stores are not reachable from others
        // and which ones are overwritten by which others.
        OverwriteMapType overwriteMap;
        findOverwritingStores(stores, undefStoreSet, overwriteMap, parentBB);

        if (overwriteMap.empty())
        {
            DEBUG_RV( outs() << "  no stores are overwritten.\n"; );
            continue;
        }

        // Now, create a new alloca for all stores that are overwritten by the same other store.
        // Then, create a reload of that alloca, followed by a select with the mask of this
        // block, the value of the overwriting store, and the incoming mask of this block,
        // and the reload.
        AllocaInst* oldAlloca = MI->mAlloca;
        // TODO: HERE! use oldAlloca?
        Instruction* allocaPos =
            oldAlloca->getParent()->getParent()->getEntryBlock().getFirstNonPHI();
        // TODO: HERE! This never gets used?!?!
        for (auto &pair : overwriteMap)
        {
            StoreSetType& directPreds = *pair.second;

            const bool overwritesOthers = !directPreds.empty();
            if (!overwritesOthers) continue;

            StoreInst* store = pair.first;

            DEBUG_RV( outs() << "  overwrite    ('" << store->getParent()->getName(); );
            DEBUG_RV( outs() << "'): " << *store << "\n"; );

            LoadVecType*  newReloads = new LoadVecType();
            StoreVecType* newStores  = new StoreVecType();

            AllocaInst* newAlloca = new AllocaInst(oldAlloca->getAllocatedType(),
                                                   nullptr,
                                                   "alloca.tmp",
                                                   allocaPos);
            rv::copyMetadata(newAlloca, *oldAlloca); // Retain metadata.
            if (mvecInfo.isMetadataMask(oldAlloca)) mvecInfo.markMetadataMask(newAlloca);
            mvecInfo.setVectorShape(*newAlloca, mvecInfo.getVectorShape(*oldAlloca));
            DEBUG_RV( outs() << "  new alloca   ('" << newAlloca->getParent()->getName(); );
            DEBUG_RV( outs() << "'): " << *newAlloca << "\n"; );

            const bool isNewMask = mvecInfo.isMetadataMask(newAlloca);

            // Create new store to new location for each overwritten store.
            // If the old one is overwritten on all paths, it does not have an
            // effect. If it is not overwritten on some path, not removing this
            // store ensures that the value is still available there.
            for (auto ovwStore : directPreds)
            {
                StoreInst* newOvwStore = new StoreInst(ovwStore->getValueOperand(),
                                                       newAlloca,
                                                       ovwStore);

                DEBUG_RV( outs() << "  new store    ('" << newOvwStore->getParent()->getName(); );
                DEBUG_RV( outs() << "'): " << *newOvwStore << "\n"; );

                // Save the store in the new meminfo object.
                newStores->push_back(newOvwStore);
            }

            // Reload from this location.
            LoadInst* newReload = new LoadInst(newAlloca, "reload.tmp", store);
            newReloads->push_back(newReload);
            DEBUG_RV( outs() << "  new reload   ('" << newReload->getParent()->getName(); );
            DEBUG_RV( outs() << "'): " << *newReload << "\n"; );

            // Get the mask of the corresponding (now non-existant) edge.
            BasicBlock* storeBB = store->getParent();
            Value* mask = mMaskAnalysis.getExitMask(*storeBB, *parentBB);
            if (Instruction* maskI = dyn_cast<Instruction>(mask))
            {
                const bool inValueMap = maskValueMap.count(maskI);
                if (inValueMap || maskPhiValueMap.count(maskI))
                {
                    Value* maskAlloca = inValueMap ? maskValueMap[maskI] : maskPhiValueMap[maskI];
                    assert (isa<AllocaInst>(maskAlloca));
                    mask = new LoadInst(maskAlloca, "reload.mask", store);

                    // Add this reload to the meminfo object of the mask alloca.
                    bool added = false; RV_UNUSED(added);
                    for (auto &MI : memInfos)
                    {
                        if (MI->mAlloca != maskAlloca) continue;
                        MI->mReloads->push_back(cast<LoadInst>(mask));
                        added = true;
                        break;
                    }
                    assert (added);
                }
            }

            // Blend the overwriting store's value with the reload.
            SelectInst* select = SelectInst::Create(mask,
                                                    store->getValueOperand(),
                                                    newReload,
                                                    "ssa.repair.phi",
                                                    store);
            // rv::setMetadata(select, rv::RV_METADATA_RES_VECTOR); //TODO
            // rv::setMetadata(select, rv::RV_METADATA_OP_VARYING);
            // rv::setMetadata(select, rv::RV_METADATA_INDEX_RANDOM);
            // rv::setMetadata(select, rv::RV_METADATA_ALIGNED_FALSE);
            // rv::setMetadataForBlend(select, storeBB, nullptr, true /* isLast */);

            mvecInfo.setVectorShape(*select, VectorShape::varying());

            DEBUG_RV( outs() << "  new select   ('" << select->getParent()->getName(); );
            DEBUG_RV( outs() << "'): " << *select << "\n"; );

            if (isNewMask)
            {
                mvecInfo.markMetadataMask(select);
                allocaValueMap[newAlloca] = select;
                maskValueMap[select] = newAlloca;
                maskBlockMap[select] = select->getParent();
            }

            // Finally, update the stored value of the current store.
            assert (store->getValueOperand() == store->getOperand(0));
            store->setOperand(0, select);

            // Delay the push_back into 'memInfos' to not confuse the iteration.
            newMemInfos.push_back(new MemInfo(select, newAlloca, newReloads, newStores));
        }
    }

    for (auto &MI : newMemInfos)
    {
        memInfos.push_back(MI);
    }
}

void
CFGLinearizer::findOverwritingStores(const StoreVecType& stores,
                                     const StoreSetType& undefStoreSet,
                                     OverwriteMapType&   overwriteMap,
                                     const BasicBlock*   doNotTraverse) const
{
    for (const auto &store : stores)
    {
        BasicBlock* storeBB = store->getParent();

        // Ignore undef stores (they are always overwritten by construction).
        if (undefStoreSet.count(store)) continue;

        Loop* ignoreLoop = mLoopInfo.getLoopFor(storeBB);

        // Collect all stores that the current one overwrites (except for undef stores).
        StoreSetType overwrittenStores;
        for (auto &other : stores)
        {
            if (other == store) continue;
            if (undefStoreSet.count(other)) continue;
            BasicBlock* otherBB = other->getParent();
            assert (ignoreLoop == mLoopInfo.getLoopFor(otherBB));
            if (!rv::isReachable(storeBB, otherBB, doNotTraverse, ignoreLoop, ignoreLoop)) continue;
            overwrittenStores.insert(other);
        }

        if (overwrittenStores.empty()) continue;

        // Exclude those that are overwritten already by a different one.
        StoreSetType* directPreds = new StoreSetType();
        for (auto ov0 : overwrittenStores)
        {
            BasicBlock* bb0 = ov0->getParent();
            bool isOverwrittenByOther = false;
            for (auto ov1 : overwrittenStores)
            {
                if (ov0 == ov1) continue;
                BasicBlock* bb1 = ov1->getParent();
                if (!rv::isReachable(bb1, bb0, doNotTraverse, ignoreLoop, ignoreLoop)) continue;
                isOverwrittenByOther = true;
                break;
            }
            if (isOverwrittenByOther) continue;
            directPreds->insert(ov0);
        }
        overwriteMap[store] = directPreds;
    }
}

const Loop*
CFGLinearizer::getInnermostLoopForAlloca(const LoadVecType&  reloads,
                                         const StoreVecType& stores) const
{
    assert (!stores.empty());

    // Find the innermost common loop of all defs.
    const Loop* commonLoop = nullptr;
    for (const auto &store : stores)
    {
        const BasicBlock* defBB = store->getParent();
        const Loop* loop = mLoopInfo.getLoopFor(defBB);

        // If the def is not in a loop, we can return immediately because this
        // means that the alloca has to be in the entry block of the function
        // (which it already is).
        if (!loop) return nullptr;

        if (!commonLoop)
        {
            commonLoop = loop;
            continue;
        }

        assert (loop->contains(commonLoop) || commonLoop->contains(loop));

        if (loop->contains(commonLoop)) commonLoop = loop;
        assert (loop->contains(defBB) && commonLoop->contains(defBB));
    }

    assert (commonLoop);

    // Find the outermost loop of all uses.
    for (const auto &reload : reloads)
    {
        const BasicBlock* useBB = reload->getParent();
        const Loop* loop = mLoopInfo.getLoopFor(useBB);

        // If the use is not in a loop, we can return immediately because this
        // means that the alloca has to be in the entry block of the function
        // (which it already is).
        if (!loop) return nullptr;

        assert (loop->contains(commonLoop) || commonLoop->contains(loop));

        if (loop->contains(commonLoop)) commonLoop = loop;
    }

    return commonLoop;
}

void
CFGLinearizer::mem2reg(Function*              f,
                       std::vector<MemInfo*>& memInfos,
                       MaskValueMapType&      maskValueMap,
                       MaskValueMapType&      maskPhiValueMap,
                       MaskValueMapType&      allocaValueMap,
                       MaskBlockMapType&      maskBlockMap)
{
    assert (f);

    // SROA/mem2reg phases do not allow setting metadata to generated phis.
    // LASP
    SmallVector<PHINode*, 4> insertedPhis;
    SSAUpdater ssaUpdater(&insertedPhis);

    SmallVector<Instruction*, 2> insts;
    for (auto &MI : memInfos)
    {
        insertedPhis.clear();
        insts.clear();

        AllocaInst* alloca = MI->mAlloca;

        DEBUG_RV( outs() << "\nmem2reg: " << *alloca << "\n"; );
        DEBUG_RV( outs() << "  block   ('" << alloca->getParent()->getName() << "')\n"; );

        for (auto &store : *MI->mStores)
        {
            insts.push_back(store);
            DEBUG_RV( outs() << "  store   ('" << store->getParent()->getName() << "'): " << *store << "\n"; );
        }
        for (auto &reload : *MI->mReloads)
        {
            insts.push_back(reload);
            DEBUG_RV( outs() << "  reload  ('" << reload->getParent()->getName() << "'): " << *reload << "\n"; );
        }

        // Run promoter.
        // NOTE: This only works correctly if all phis have incoming values
        //       that correspond to the block's actual incoming edges!
        //       This is because SSAUpdater has code that prefers to look
        //       at the incoming blocks of other phis instead of iterating
        //       over the actual predecessors via pred_begin()/end().
        assert (verifyIncomingEdges(*f));
        LoadAndStorePromoter LASP(insts, ssaUpdater);
        LASP.run(insts);

        // If this alloca originally was a mask, update the mask value map
        // with the new value.
        // NOTE: We must never directly access "origMask" since it may have been
        //       erased. We must only use its pointer address to access the
        //       mapped values in maskValueMap and maskBlockMap.
        if (mvecInfo.isMetadataMask(alloca))
        {
            assert (allocaValueMap.count(alloca));
            Value* origMask = allocaValueMap[alloca];

            const bool isMaskPhiAlloca = maskPhiValueMap.count(origMask) &&
                maskPhiValueMap[origMask] == alloca;
            MaskValueMapType& maskMap = isMaskPhiAlloca ?
                maskPhiValueMap : maskValueMap;
            assert (maskMap.count(origMask));

            if (insertedPhis.empty())
            {
                // Nothing changed, so the mask graph is still valid.
                maskMap.erase(origMask);
            }
            else
            {
                // Get the value in the target block and set it as the new mask.
                assert (maskMap.count(origMask));
                assert (maskMap[origMask] == alloca);
                assert (maskBlockMap.count(origMask));
                BasicBlock* maskBlock = maskBlockMap[origMask];
                // TODO: Why can it happen that we insert additional phis here?
                // NOTE: In some cases, this produces dead code, e.g. for unit tests
                //       Test063LoopNestedMultiExit07, Test065LoopNestedMultiExit09.
                Value* newMask = ssaUpdater.GetValueInMiddleOfBlock(maskBlock);
                assert (newMask);
                maskMap[origMask] = newMask;
            }
        }

        for (auto &phi : insertedPhis)
        {
            // The alloca temporarily stored the relevant metadata.
            rv::copyMetadata(phi, *alloca);
            mvecInfo.setVectorShape(*phi, mvecInfo.getVectorShape(*alloca));
            if (mvecInfo.isMetadataMask(alloca)) mvecInfo.markMetadataMask(phi);
            // Phi functions are always OP_UNIFORM unless they have RES_SCALARS operands.
            // Since we copied from a normal instruction, that metadata may be wrong.
            // TODO: There may be more things to consider here...
            bool hasResScalarsOp = false;
            for (auto O=phi->op_begin(), OE=phi->op_end(); O!=OE; ++O)
            {
                if (!isa<Argument>(*O) && !isa<Instruction>(*O)) continue;
                // if (!rv::hasMetadata(*O, rv::RV_METADATA_RES_SCALARS)) continue; //TODO
                hasResScalarsOp = true;
                break;
            }
            // rv::setMetadata(phi,                             hasResScalarsOp ?                                 rv::RV_METADATA_OP_SEQUENTIAL :                                 rv::RV_METADATA_OP_UNIFORM);
            DEBUG_RV( outs() << "  new phi ('" << phi->getParent()->getName(); );
            DEBUG_RV( outs() << "'): " << *phi << "\n"; );
        }

        // If this is a new mask phi, incoming values that are "undef" have to
        // be changed to "false". This maintains correct behavior when rewiring
        // edges of uniform control flow (e.g. in test_fastwalshtransform of
        // test suite 3).
        for (auto &phi : insertedPhis)
        {
            if (!mvecInfo.isMetadataMask(phi)) continue;
            for (unsigned i=0, e=phi->getNumIncomingValues(); i<e; ++i)
            {
                Value* incVal = phi->getIncomingValue(i);
                if (!isa<UndefValue>(incVal)) continue;
                phi->setIncomingValue(i, ConstantInt::getFalse(phi->getContext()));
            }
        }

        assert (alloca->use_empty());
        alloca->eraseFromParent();
    }

    // vectorizationInfo might map to stale PHI nodes that have been invalidated by reg2mem/mem2reg
    // reconstruct the actual values using fake uses
    remapFakeUses(*f);

    // strip the fake use intrinsic eagerly
    auto * fakeFunc = f->getParent()->getFunction("rv_fake");
    if (fakeFunc && fakeFunc->use_empty()) {
      fakeFunc->eraseFromParent();
    }

    for (auto &MI : memInfos)
    {
        delete MI;
    }
}

static inline
const DomTreeNode*
GetIDom(const DominatorTree & domTree, const BasicBlock & block) {
    const DomTreeNode * domNode = domTree.getNode(const_cast<BasicBlock*>(&block));
    return domNode->getIDom();
}

void
CFGLinearizer::getDivergenceCausingBlocks(Function& scalarFn)
{
    for (BasicBlock& BB : scalarFn)
    {
        if (mvecInfo.getVectorShape(BB).isVarying())
        {
            getDivergenceCausingBlocksFor(BB);
        }
    }
}

void
CFGLinearizer::getDivergenceCausingBlocksFor(BasicBlock& divergentBlock)
{
    assert (mvecInfo.getVectorShape(divergentBlock).isVarying());
    DEBUG_VA( outs() << "\ngetDivergenceCausingBlocksFor(" << divergentBlock.getName() << ")\n"; );

    ConstBlockSet divCauseBlocks;
    checkForDisjointPaths(&divergentBlock, true, divCauseBlocks); // FIXME const_cast
    for (auto it : divCauseBlocks)
        mDivergenceCauseMap[&divergentBlock].push_back(const_cast<BasicBlock*>(it));

    assert (!mDivergenceCauseMap[&divergentBlock].empty());
}

void
CFGLinearizer::getRewireTargets(Function& scalarFn)
{
    for (BasicBlock& BB : scalarFn)
    {
        if (mvecInfo.isMandatory(&BB))
        {
            getRewireTargetsForMandatoryBlock(BB, mLoopInfo);
        }
    }
}

void
CFGLinearizer::getRewireTargetsForMandatoryBlock(BasicBlock& block,
                                                 const LoopInfo&   loopInfo)
{
    assert (mvecInfo.isMandatory(&block));
    DEBUG_VA( outs() << "\ngetRewireTargetsForMandatoryBlock(" << block.getName() << ")\n"; );

    //-----------------------------------------------------------------------//
    // 1. b is a direct successor of v.
    //-----------------------------------------------------------------------//
    for (BasicBlock* v : predecessors(&block))
    {
        const VectorShape& termShape = mvecInfo.getVectorShape(*v->getTerminator());

        // The branch is varying if the terminator is not OP_UNIFORM.
        // This is a little more safe than just checking for OP_VARYING
        // although at this point there should be no OP_SEQUENTIAL and
        // no unmarked terminators.
        if (!termShape.isUniform())
        {
            mRewireTargetMap[v].push_back(&block);
        }
    }

    //-----------------------------------------------------------------------//
    // 2. b is a divergent block.
    //-----------------------------------------------------------------------//
    if (mvecInfo.getVectorShape(block).isVarying())
    {
        std::vector<BasicBlock*> divergenceCausingBlocks = mDivergenceCauseMap[&block];

        for (auto &dcBB : divergenceCausingBlocks)
        {
            mRewireTargetMap[dcBB].push_back(&block);
        }
    }

    //-----------------------------------------------------------------------//
    // 3. b is a latch of a DIVERGENT loop
    //-----------------------------------------------------------------------//

    // Rewire targets are all blocks that cause exits to be MANDATORY.
    // Since it is easier to add this information when such an exit is found, we do it
    // there.

    //-----------------------------------------------------------------------//
    // 4. b is an exit block of a loop l with loop latch e. v is another block of
    //    the same loop that ends with a varying branch. There exist two *disjoint*
    //    paths p1 and p2 starting at the true and false edge of v, respectively,
    //    that do not include the back edge of l. p1 goes from v to b while p2 goes
    //    from v to e.
    //-----------------------------------------------------------------------//
    // Check if this block is an exit block of a loop. This can never be the case
    // if it has more than one or no predecessor (thanks to loop simplification).
    const BasicBlock* exitBlockPredBB = block.getUniquePredecessor();
    if (exitBlockPredBB)
    {
        // Obviously, if the predecessor is not in a different loop than the current
        // block, this is not a loop exit.
        Loop* loop = loopInfo.getLoopFor(&block);
        Loop* predLoop = loopInfo.getLoopFor(exitBlockPredBB);
        if (predLoop && predLoop != loop)
        {
            // We have to test the criterion for all nested loops that this block is an exit of.
            for (; predLoop; predLoop = predLoop->getParentLoop())
            {
                if (!predLoop->isLoopExiting(exitBlockPredBB)) break;
                getRewireTargetsIfMandatoryExit(block, *predLoop);
            }
        }
    }
}

void
CFGLinearizer::updateRewireTargetsOnDisjointLoopPaths2(BasicBlock&  exitBlock,
                                                       const Loop&  loop,
                                                       BlockSet*    divCauseBlocks) {

    NodeSet exitingNodes;
    const BasicBlock* entryBlock = &mInfo.mScalarFunction->getEntryBlock();
    SketchGraph * graph = nullptr;

    const DomTreeNode * idom = GetIDom(mDomTree, exitBlock);
    BasicBlock * idomBlock = idom ? idom->getBlock() : nullptr;


// Restrict the graph search to @loop and exiting edges from @loop to @exitBlock
    BasicBlock * loopHeader = loop.getHeader();
    const DomTreeNode * loopDom = mDomTree.getNode(loopHeader);

    graph = SketchGraph::createFromDominatorNode(mDomTree, *loopHeader, exitingNodes);

// get the node handle for the exiting node
    const BasicBlock * exitingBlock = exitBlock.getUniquePredecessor();
    assert(exitingBlock);
    auto exitingNode = graph->getNode(*exitingBlock);

// add the exiting node explicitly
    SketchNode * exitNode = graph->push(&exitBlock);
    exitingNode->insert(exitNode);

// only consider reachable edges
    BasicBlock * latchBlock = loop.getLoopLatch();
#if 0
    errs() << "Loop scope "; loop->print(errs());
	llvm::errs() << "Graph "; graph->dump();
	llvm::errs() << "Exit block: "<< exitBlock->getName() << "\n";
	llvm::errs() << "Latch block: "<< latchBlock->getName() << "\n";
#endif
    SketchNode * latchNode = graph->getNode(*latchBlock);
    if (!latchNode) {
        return; // FIXME
    }

    assert(latchNode && "latchNode killed during clean-up");
    // except for the actual path enumeration, this is copied over from isMandatoryExit(..)
    NodeSet accepting;
    accepting.insert(exitNode);
    accepting.insert(latchNode);
    PathFinder finder(*graph, accepting);

    const auto & loopBlocks = loop.getBlocks();

    for (BasicBlock * vBlock : loopBlocks) {
        if (! rv::HasVaryingBranch(*vBlock, mvecInfo))
            continue;

        // this only applies to nodes, that vBlock does not post-dominate
        if ( mPostDomTree.dominates(&exitBlock, vBlock) )
            continue;

        if ( mPostDomTree.dominates(latchBlock, vBlock) )
            continue;

        // new path search algorithm starting from here
        SketchNode * parentNode = graph->getNode(*vBlock);
        assert(parentNode && "branch not modelled");

        if (finder.findPath(parentNode)) { // validity is not checked for first node
            DEBUG_VA( outs() << "  Block '" << exitBlock.getName() << "' is MANDATORY (4)!\n"; );
            DEBUG_VA( outs() << "    due to exit '" << vBlock->getName() << "'.\n"; );
            // Add both the current block as well as the latch as rewire targets for the branch parent.
            mRewireTargetMap[vBlock].push_back(&exitBlock);
            mRewireTargetMap[vBlock].push_back(latchBlock);
            if (divCauseBlocks) {
                divCauseBlocks->insert(&exitBlock);
            }
            break;
        }
    }

    delete graph;
}

void
CFGLinearizer::getRewireTargetsIfMandatoryExit(BasicBlock& block,
                                               const Loop& loop)
{
    const BasicBlock* exitBlockPredBB = block.getUniquePredecessor();

    // If the predecessor block is the loop latch, all paths from varying branches
    // inside the loop by definition can not have disjoint paths to the latch and
    // the block, so the block can not be MANDATORY due to this criterion.
    const BasicBlock* latch = loop.getLoopLatch();
    DEBUG_VA ( if (exitBlockPredBB == latch) outs() << "    exiting block is the latch itself "
                                                    << "- no candidate for divergence criterion (4).\n"; );
    if (exitBlockPredBB == latch) return;

    // Check all edges in the set of the predecessor for one
    // 1. whose parent is inside the loop, and
    // 2. whose other edge reaches the latch.
    // Test case for criterion 2: TestDivergenceAnalysisLoop2.

    updateRewireTargetsOnDisjointLoopPaths2(block, loop, nullptr);
}

bool
CFGLinearizer::checkForDisjointPaths(const BasicBlock*  block,
                                     bool               doNotBranchToOuterLoops,
                                     ConstBlockSet&     divergenceCausingBlocks) const {
    NodeSet exitingNodes;
    const BasicBlock* entryBlock = &block->getParent()->getEntryBlock();
    SketchGraph * graph = nullptr;

    const DomTreeNode * idom = GetIDom(mDomTree, *block);
    BasicBlock * idomBlock = idom ? idom->getBlock() : nullptr;

    // Restrict the search to nodes in the idom region that reach @block
    if (doNotBranchToOuterLoops) {
        const Loop * loop = mLoopInfo.getLoopFor(block);
        const BasicBlock * topBlock = loop ? loop->getHeader() : entryBlock;
        graph = SketchGraph::createFromDominatorNode(mDomTree, *topBlock, exitingNodes);

    } else{
        graph = SketchGraph::createFromDominatorNode(mDomTree, *entryBlock, exitingNodes);
        assert(exitingNodes.empty() && "invalid dominance graph for entry block");
    }

#if 0
    errs() << "Original graph:\n";
	graph->dump();
	mLoopInfo->print(errs());
#endif

// Remove all back-edges to outer loops
    // FIXME
    Loop * targetLoop = mLoopInfo.getLoopFor(block);
    if (doNotBranchToOuterLoops) {
        for (Loop * outerLoop = targetLoop;
             outerLoop;
             outerLoop = outerLoop->getParentLoop())
        {
            SketchNode * outerHeaderNode = graph->getNode(*outerLoop->getHeader());
            NodeSet outerLoopPredSet = graph->getPredecessors(outerHeaderNode);
            // graph->dumpSet(outerLoopPredSet);

            for (SketchNode * outerHeaderPred : outerLoopPredSet) {
                outerHeaderPred->erase(outerHeaderNode);
            }
        }
        // reference
        // original condition restated here
        // if (loop == targetLoop || !targetLoop->contains(loop)) continue;
    }
// Eliminate dead-ends to accelerate path search
    {
        NodeSet keepSet;
        SketchNode* blockNode = graph->getNode(*block);
        assert(blockNode);
        keepSet.insert(blockNode);
        graph->eliminateDeadEnds(keepSet);
    }

    SketchNode * entryNode = graph->getNode(*entryBlock);
    SketchNode * blockNode = graph->getNode(*block);


// Run confluence contraction to merge any two nodes that will eventually pass through the same post-dominating node
    SketchGraph * compactGraph = nullptr;

    if (graph->getNumNodes() < 5) {
        compactGraph = graph;
        DEBUG_VA( errs() << "[VA] Graph with "<<  graph->getNumNodes() << " nodes to small for compaction\n"; );
    } else {
        NodeSet coreNodes = graph->getPredecessors(blockNode);
        coreNodes.insert(blockNode);
        compactGraph = graph->generateConfluenceGraph(coreNodes, nullptr);
        // FIXME also make verbose
        DEBUG_VA( errs() << "[VA] Compacted from " << graph->getNumNodes() << " to " << compactGraph->getNumNodes() << "\n"; );
    }

#ifdef VERBOSE
    errs () << "Mapping "; confluenceMap.dump(*compactGraph, *graph);
	errs() << "Compact "; compactGraph->dump();
	errs() << "Source "; graph->dump();
#endif

// Check for existing path
    SketchNode * compactBlockNode = compactGraph->getNode(*block);
    assert(compactBlockNode);

    NodeSet compactAcceptingNodes = compactGraph->getPredecessors(compactBlockNode);
    PathFinder finder(*compactGraph, compactAcceptingNodes);

    for (auto & pair  : *compactGraph) {
        SketchNode * vCompactNode = pair.first;
        const llvm::BasicBlock * vBlock = pair.second;

        // optimization only look below the idom (if any)
        if (! idomBlock || (vBlock == idomBlock) || mDomTree.dominates(idomBlock, vBlock)) { //  TODO Only nodes below the idom are interesting
            if (rv::HasVaryingBranch(*vBlock, mvecInfo)) {
                /* const BasicBlock * A;
                const BasicBlock * B;
                GetTwoSuccessors(vBlock, A, B);

                // FIXME do look-ups in the contraction map
                SketchNode * aStart = graph->getNode(*A);
                SketchNode * bStart = graph->getNode(*B);
                SketchNode * aCompactStart = confluenceMap.getRootForNode(aStart);
                SketchNode * bCompactStart = confluenceMap.getRootForNode(bStart);
                assert(aCompactStart && bCompactStart);

                if (aCompactStart && aCompactStart == bCompactStart) { // a and b have the same post-dominating node
                    continue;
                } */

                // the node was not represented in this graph (FIXME this could be the reason why IDom filtering does not work. Consider, that one path might stay at vBlock==block)
                // if (vCompactNode->size() <= 2) { continue; }

                if ( finder.findPath(vCompactNode)) { // validity is not checked for first node
                    divergenceCausingBlocks.insert(const_cast<BasicBlock*>(vBlock)); // FIXME const_cast
                }
            }
        }
    }

    delete graph;
    if (compactGraph != graph) delete compactGraph;
    return ! divergenceCausingBlocks.empty();
}

} // namespace rv





// CFGLinearizerWrapper
CFGLinearizerWrapper::CFGLinearizerWrapper()
        : FunctionPass(ID), mLinearizer(nullptr)
{ }

CFGLinearizerWrapper::~CFGLinearizerWrapper()
{
    delete mLinearizer;
    mLinearizer = nullptr;
}

void
CFGLinearizerWrapper::releaseMemory()
{
}

void
CFGLinearizerWrapper::getAnalysisUsage(AnalysisUsage &AU) const
{
    AU.addRequired<RVInfoProxyPass>();
    AU.addPreserved<RVInfoProxyPass>();

    AU.addRequired<MaskAnalysisWrapper>();
    AU.addPreserved<MaskAnalysisWrapper>();

    AU.addRequired<LoopInfoWrapperPass>();
    AU.addPreserved<LoopInfoWrapperPass>();

    AU.addRequired<LoopLiveValueAnalysisWrapper>();
    AU.addPreserved<LoopLiveValueAnalysisWrapper>();

    AU.addRequired<VectorizationInfoProxyPass>();
    AU.addPreserved<VectorizationInfoProxyPass>();
}

bool
CFGLinearizerWrapper::doInitialization(Module& M)
{
    // The return value presumably signals whether the module was changed or not.
    // There is no documentation on this in LLVM.
    return false;
}

bool
CFGLinearizerWrapper::doFinalization(Module& M)
{
    // The return value presumably signals whether the module was changed or not.
    // There is no documentation on this in LLVM.
    return false;
}

bool
CFGLinearizerWrapper::runOnFunction(Function& F)
{
    const rv::RVInfo& rvInfo                  = getAnalysis<RVInfoProxyPass>().getInfo();
    LoopInfo& loopInfo                      = getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
    MaskAnalysis& maskAnalysis              = *getAnalysis<MaskAnalysisWrapper>().getMaskAnalysis();
    const LoopLiveValueAnalysis& LLVA       = *getAnalysis<LoopLiveValueAnalysisWrapper>()
                                               .getLLVAnalysis();
    VectorizationInfo& vecInfo              = getAnalysis<VectorizationInfoProxyPass>().getInfo();
    const PostDominatorTree& postDomTree    = getAnalysis<PostDominatorTree>();
    const DominatorTree& domTree            = getAnalysis<DominatorTreeWrapperPass>().getDomTree();

    mLinearizer = new rv::CFGLinearizer(rvInfo,
                                    loopInfo,
                                    maskAnalysis,
                                    LLVA,
                                    vecInfo,
                                    postDomTree,
                                    domTree);

    return mLinearizer->linearize(F);
}

void
CFGLinearizerWrapper::print(raw_ostream& O, const Module* M) const
{
}

bool
CFGLinearizerWrapper::verify(const Function& F) const
{
    assert (mLinearizer && "Pass needs to run first!");
    return mLinearizer->verify(F);
}


char CFGLinearizerWrapper::ID = 0;
// NOTE: The order of initialized dependencies is important
//       to prevent 'Unable to schedule' errors!
INITIALIZE_PASS_BEGIN(CFGLinearizerWrapper, "cfgLinearizer", "CFGLinearizer", false, false)
INITIALIZE_PASS_DEPENDENCY(RVInfoProxyPass)
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(MaskAnalysisWrapper)
INITIALIZE_PASS_DEPENDENCY(LoopLiveValueAnalysisWrapper)
INITIALIZE_PASS_END(CFGLinearizerWrapper, "cfgLinearizer", "CFGLinearizer", false, false)

// Public interface to the CFGLinearizer pass
FunctionPass*
llvm::createCFGLinearizerPass()
{
	return new CFGLinearizerWrapper();
}

