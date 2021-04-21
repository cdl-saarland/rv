//===- src/transform/maskExpander.cpp - IR generator for edge and block predicates  --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#include "rv/legacy/passes.h"
#include "rv/LinkAllPasses.h"
#include "rv/transform/OMPDeclutter.h"
#include "rvConfig.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Verifier.h"
#include "llvm/InitializePasses.h"
#include "llvm/Passes/PassBuilder.h"

#include "llvm/ADT/GraphTraits.h"
#include "llvm/Analysis/BranchProbabilityInfo.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/MemoryDependenceAnalysis.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/ScalarEvolutionExpressions.h"

#include "llvm/Analysis/LoopDependenceAnalysis.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/IR/Instructions.h"

#include "report.h"

#include <set>

#if 1
#define IF_DEBUG_DEC IF_DEBUG
#else
#define IF_DEBUG_DEC if (true)
#endif

using namespace llvm;
using namespace rv;

// kmpc function names.:
const char *kmpc_reduce_nowait_Name = "__kmpc_reduce_nowait";

static bool
subscriptsAlloca(Value *Ptr, AllocaInst *Alloc) {
  if (Ptr == Alloc) return true;
  if (auto *GEP = dyn_cast<GetElementPtrInst>(Ptr)) {
    return subscriptsAlloca(GEP->getPointerOperand(), Alloc);
  }
  if (auto *Cast = dyn_cast<CastInst>(Ptr)) {
    return subscriptsAlloca(Cast->getOperand(0), Alloc);
  }
  return false;
}

using InstSet = std::set<Instruction*>;

/// Actual implementation.
struct OMPDeclutterSession {
  Function& F;
  LoopInfo& LI;

  std::set<Instruction*> ReductionSlots;

  OMPDeclutterSession(Function &F, LoopInfo &LI) : F(F), LI(LI) {}

  InstSet collectReductionSlotStores(CallInst &CI) {
    InstSet SlotStores;

    // Find the alloca of the pointer to the reduction slot.
    auto *LocalStoreLocBC = CI.getArgOperand(4);
    auto *AllocaBC = dyn_cast<Instruction>(LocalStoreLocBC);
    assert(AllocaBC);

    auto *ReduceSlotAlloca = dyn_cast<AllocaInst>(AllocaBC->getOperand(0));
    assert(ReduceSlotAlloca);

    // Visit all stores in the reduction call's block.
    for (auto &Inst : *CI.getParent()) {
      if (&Inst == &CI)
        break;
      auto *SI = dyn_cast<StoreInst>(&Inst);
      if (!SI)
        continue;
      if (!subscriptsAlloca(SI->getPointerOperand(), ReduceSlotAlloca))
        continue;

      auto *SlotAlloc = dyn_cast<AllocaInst>(SI->getValueOperand());
      if (!SlotAlloc)
        continue;
      SlotStores.insert(SI);
    }
    return SlotStores;
  }

  void pushRecurringStoresToExit(InstSet AllocSet,
                                BasicBlock *ReduceCallBlock) {
    // Find the containing loop.
    auto SlotAlloca = *AllocSet.begin();
    Loop * StoreLoop = nullptr;
    StoreInst *RecStore = nullptr;
    for (auto *User : SlotAlloca->users()) {
      RecStore = dyn_cast<StoreInst>(User);
      if (!RecStore)
        continue;

      // TODO: Find loop that exits to the reduce call block.
      StoreLoop = LI.getLoopFor(RecStore->getParent());
      if (!StoreLoop)
        continue;
      break;
    }

    // Narrow to the outer-most loop that does not contain the ReduceCallBlack.
    auto *ReduceLoop = LI.getLoopFor(ReduceCallBlock);
    while (StoreLoop->getParentLoop() != ReduceLoop)
      StoreLoop = StoreLoop->getParentLoop();

    // Check whether there already is a broken edge...
    auto *ExitingBlock = StoreLoop->getExitingBlock();
    auto *ExitingTerm  = dyn_cast<BranchInst>(ExitingBlock->getTerminator());

    int LoopExitingIdx =
        LI.getLoopFor(ExitingTerm->getSuccessor(0)) == StoreLoop ? 1 : 0;
    // Break the exiting edge.
    BasicBlock *BreakEdge = nullptr;
    Instruction *BreakEdgeTerm = nullptr;
    if (ExitingTerm->getSuccessor(LoopExitingIdx) != ReduceCallBlock) {
      BreakEdge = ExitingTerm->getSuccessor(LoopExitingIdx);
      BreakEdgeTerm = BreakEdge->getTerminator();
      assert(BreakEdgeTerm->getSuccessor(0) == ReduceCallBlock);
    } else {
      BreakEdge = BasicBlock::Create(ReduceCallBlock->getContext(),
                                     "break_edge", &F, ReduceCallBlock);

      ExitingTerm->replaceUsesOfWith(ReduceCallBlock, BreakEdge);
      BreakEdgeTerm = BranchInst::Create(ReduceCallBlock, BreakEdge);
    }

    // FIXME: This will miss stores in other blocks.
    // Move **all** 'stores' into the broken edge.
    InstSet StoreSet;
    for (auto &Inst : *RecStore->getParent()) {
      auto *SI = dyn_cast<StoreInst>(&Inst);
      if (!SI)
        continue;
      auto *PtrInst = dyn_cast<Instruction>(SI->getPointerOperand());
      if (!PtrInst || !AllocSet.count(PtrInst))
        continue;

      StoreSet.insert(SI);
    }

    for (auto *SI : StoreSet) {
      SI->removeFromParent();
      SI->insertBefore(BreakEdgeTerm);
    }
  }

  void deferSlotStores(InstSet &SlotStores, BasicBlock *ReduceCallBlock) {
    std::map<const Value*, AllocaInst*> NewAllocaMap;

    for (auto *SI : SlotStores) {
      auto *Store = dyn_cast<StoreInst>(SI);
      auto *StoredAlloca = dyn_cast<AllocaInst>(Store->getValueOperand());

      // Copy value to fresh allocation.
      auto *FreshAlloc = new AllocaInst(StoredAlloca->getAllocatedType(), StoredAlloca->getType()->getPointerAddressSpace(), StoredAlloca->getArraySize(), ".defer", StoredAlloca);
      auto *RedVal = new LoadInst(StoredAlloca->getAllocatedType(), StoredAlloca, ".get", Store);
      new StoreInst(RedVal, FreshAlloc, Store);

      // Replace allocation in reduction call.
      Store->setOperand(0, FreshAlloc);

      // Keep track of the mapping for follow-up rewrites.
      NewAllocaMap[StoredAlloca] = FreshAlloc;
    }

    // Re-write followup loads to use the new location.
    for (int SuccIdx = 0; SuccIdx < (int) ReduceCallBlock->getTerminator()->getNumSuccessors(); ++SuccIdx) {
      auto *SuccBlock = ReduceCallBlock->getTerminator()->getSuccessor(SuccIdx);
      for (auto &Inst : *SuccBlock) {
        auto *L = dyn_cast<LoadInst>(&Inst);
        if (!L) continue;
        auto ItNewAlloca = NewAllocaMap.find(L->getPointerOperand());
        if (ItNewAlloca == NewAllocaMap.end()) continue;
        L->setOperand(0, ItNewAlloca->second);
      }
    }
  }

  bool run() {
    auto KMPReduceFunc = F.getParent()->getFunction(kmpc_reduce_nowait_Name);
    if (!KMPReduceFunc)
      return false;

    bool Changed = false;

    // Scan for ' __kmpc_reduce_nowait' call.
    for (auto &I : instructions(F)) {
      auto *CI = dyn_cast<CallInst>(&I);
      if (!CI)
        continue;
      if (CI->getCalledFunction() != KMPReduceFunc)
        continue;

      // Identify the thread local 'alloca' that holds the reduced value.
      auto SlotStoreSet = collectReductionSlotStores(*CI);
      IF_DEBUG_DEC {
        for (auto *A : SlotStoreSet)
          errs() << "OMPDeclutter detected reduction slot store: " << *A
                 << ".\n";
      }

      deferSlotStores(SlotStoreSet, CI->getParent());

      // Find the recurring 'store' to the alloca and push it down into the
      // exiting edge to the reduce call.
      // pushRecurringStoresToExit(SlotSet, I.getParent());
      Changed = true;
    }

    IF_DEBUG_DEC {
      errs() << "Function after OMP Declutter:\n";
      F.dump();
    }
    return Changed;
  }
};


OMPDeclutter::OMPDeclutter() : FunctionPass(ID) {}

bool OMPDeclutter::runOnFunction(Function &F) {
  auto *ReduceFunc = F.getParent()->getFunction(kmpc_reduce_nowait_Name);
  if (!ReduceFunc)
    return false;

  auto &LI = getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
  OMPDeclutterSession Session(F, LI);
  return Session.run();
}

/// new pm plumbing.

OMPDeclutterWrapperPass::OMPDeclutterWrapperPass() {
}

llvm::PreservedAnalyses rv::OMPDeclutterWrapperPass::run(Function &F, FunctionAnalysisManager &FAM) {
  LoopInfo &LI = FAM.getResult<LoopAnalysis>(F);

  OMPDeclutterSession Session(F, LI);
  if (Session.run())
    return llvm::PreservedAnalyses::none();
  else
    return llvm::PreservedAnalyses::all();
}


/// legacy pm plumbing.

char OMPDeclutter::ID = 0;

void OMPDeclutter::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequired<TargetTransformInfoWrapperPass>();
  AU.addRequired<TargetLibraryInfoWrapperPass>();
  AU.addRequired<LoopInfoWrapperPass>();
}


FunctionPass *rv::createOMPDeclutterPass() { return new OMPDeclutter(); }

INITIALIZE_PASS_BEGIN(OMPDeclutter, "rv-omp-declutter",
                      "RV - OMP Declutter", false, false)
INITIALIZE_PASS_DEPENDENCY(DominatorTreeWrapperPass)
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(MemoryDependenceWrapperPass)
INITIALIZE_PASS_DEPENDENCY(PostDominatorTreeWrapperPass)
INITIALIZE_PASS_DEPENDENCY(BranchProbabilityInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(ScalarEvolutionWrapperPass)
// PlatformInfo
INITIALIZE_PASS_DEPENDENCY(TargetTransformInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(TargetLibraryInfoWrapperPass)
INITIALIZE_PASS_END(OMPDeclutter, "rv-omp-declutter", "RV - OMP Declutter",
                    false, false)
