//===- src/transform/maskExpander.cpp - IR generator for edge and block predicates  --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#include "rv/rvDebug.h"
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
#include "llvm/IR/Dominators.h"
#include "llvm/Analysis/MemoryDependenceAnalysis.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/ScalarEvolutionExpressions.h"

#include "llvm/Analysis/LoopDependenceAnalysis.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/IR/Instructions.h"

#include "llvm/Support/raw_ostream.h"

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
const char *kmpc_fork_call_Name = "__kmpc_fork_call";
const char *kmpc_for_static_init_4_Name = "__kmpc_for_static_init_4";

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
  DominatorTree& DT;
  LoopInfo& LI;

  std::set<Instruction*> ReductionSlots;

  OMPDeclutterSession(Function &F, DominatorTree &DT, LoopInfo &LI) : F(F), DT(DT), LI(LI) {}

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

  bool callsFork() {
    auto KMPForkFunc = F.getParent()->getFunction(kmpc_fork_call_Name);
    if (!KMPForkFunc)
      return false;
    // Scan for ' __kmpc_fork_call' call.
    for (auto &I : instructions(F)) {
      auto *CI = dyn_cast<CallInst>(&I);
      if (!CI)
        continue;
      if (CI->getCalledFunction() == KMPForkFunc)
        return true;
    }
    return false;
  }

  bool privatizeReductionSlots() {
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

    return Changed;
  }

  void simplifyDataFlow(Value &V) {
    BasicBlock * DomBlock = nullptr;
    if (auto *I = dyn_cast<Instruction>(&V)) 
      DomBlock = I->getParent();

    std::vector<Instruction*> VisitUser;
    for (auto U : V.users())
      if (auto *UI = dyn_cast<PHINode>(U))
        VisitUser.push_back(UI);

    std::set<PHINode*> PossiblyRedundantPhis;

    // Collect all cyclic phi nodes in the users.
    std::set<Value*> Seen;
    while (!VisitUser.empty()) {
      auto *UI = VisitUser.back();
      VisitUser.pop_back();
      if (!Seen.insert(UI).second)
        continue;

      // Cannot replace -> non-dominating
      if (DomBlock && !DT.properlyDominates(DomBlock, UI->getParent()))
        continue;

      auto *Phi = dyn_cast<PHINode>(UI);
      if (!Phi)
        continue;

      // Check whether this phi node has only phi nodes as incoming values.
      // Descend into phi incoming values (no need to go for users -> we are
      // expecting dataflow cycles here).
      bool Redundant = true;
      std::set<PHINode*> IncomingPhis;
      for (int i = 0; i < (int)Phi->getNumIncomingValues(); ++i) {
        auto *InVal = Phi->getIncomingValue(i);
        if (InVal == Phi)
          continue;
        if (InVal == &V)
          continue;

        auto *InPhi = dyn_cast<PHINode>(InVal);
        if (!InPhi) {
          Redundant = false;
          break;
        }

        IncomingPhis.insert(InPhi);
      }

      if (!Redundant)
        continue;

      for (auto *InPhi: IncomingPhis)
        VisitUser.push_back(InPhi);
      PossiblyRedundantPhis.insert(Phi);
    }

    // Check whether the phi nodes are really redundant - rinse and repeat.
    bool Changed = true;
    while (Changed) {
      Changed = false;

      for (auto *Phi : std::set<PHINode *>(PossiblyRedundantPhis)) {
        bool ReallyRedundant = true;
        for (int i = 0; i < (int)Phi->getNumIncomingValues(); ++i) {
          auto *InVal = Phi->getIncomingValue(i);
          if (InVal == Phi)
            continue;
          if (InVal == &V)
            continue;
          ReallyRedundant &= PossiblyRedundantPhis.count(cast<PHINode>(InVal));
        }

        if (ReallyRedundant)
          continue;

        Changed = true;
        auto ItPos = PossiblyRedundantPhis.find(Phi);
        PossiblyRedundantPhis.erase(ItPos);
      }

      // Replace all remaining phi nodes.
      for (auto *Phi : PossiblyRedundantPhis) {
        IF_DEBUG_DEC {
          errs() << "\tReplacing redundant " << *Phi << " with " << V << "\n";
        }
        Phi->replaceAllUsesWith(&V);
        Phi->eraseFromParent();
      }
    }
  }

  bool privatizeIterationBounds() {
    auto KMPStaticInitFunc = F.getParent()->getFunction(kmpc_for_static_init_4_Name);
    if (!KMPStaticInitFunc)
      return false;

    std::set<Instruction*> DeadLoads;
    std::set<Value*> SimplifyDataFlow;

    // Scan for ' __kmpc_reduce_nowait' call.
    for (auto &I : instructions(F)) {
      auto *CI = dyn_cast<CallInst>(&I);
      if (!CI)
        continue;
      if (CI->getCalledFunction() != KMPStaticInitFunc)
        continue;

      // Identify last stored value (if any).
      auto *CallParent = CI->getParent();
      auto *UpperBoundSlot = CI->getArgOperand(5);

      IF_DEBUG_DEC { errs() << "Found upper bound slot:" << *UpperBoundSlot << "\n"; }
      Value *UpperBoundValue = nullptr;
      for (auto It = I.getIterator(); It != CallParent->end(); ++It) {
        auto *Store = dyn_cast<StoreInst>(It);
        if (!Store)
          continue;
        if (Store->getPointerOperand() != UpperBoundSlot)
          continue;

        UpperBoundValue = Store->getValueOperand();
      }
      IF_DEBUG_DEC { errs() << "Found upper bound value:" << *UpperBoundValue << "\n"; }

      // Remove redundant phi nodes.
      SimplifyDataFlow.insert(UpperBoundValue);

      for (auto &Use : UpperBoundSlot->uses()) {
        auto Reload = dyn_cast<LoadInst>(Use.getUser());
        if (!Reload)
          continue;
        IF_DEBUG_DEC { errs() << "Dominating? " << *Reload << "\n"; }

        if ((CallParent == Reload->getParent()) ||
            !DT.dominates(CallParent, Reload->getParent()))
          continue;

        IF_DEBUG_DEC { errs() << "REPLACE RELOAD: " << *Reload << " with " << *UpperBoundValue << "\n"; }
        Reload->replaceAllUsesWith(UpperBoundValue);
        DeadLoads.insert(Reload);
      }

#if 0
        IF_DEBUG_DEC {
          errs() << "Found upper bound store:\n";
          Store->print(errs(), true);
          abort();
        }
#endif
    }

    for (auto *Load : DeadLoads) {
      Load->eraseFromParent();
    }

    for (auto *V : SimplifyDataFlow)
      simplifyDataFlow(*V);

    return false;
  }

  bool run() {
    // Only transform inner-most (non forking) outlined functions to enable vectorization.
    if (callsFork())
      return false;

    bool Changed = false;
    // Create internal 'allocas' for reduction variables to get them into SSA
    // form.
    Changed |= privatizeReductionSlots();

    // Create internal 'allocas' for the lower and upper iteration bounds to get
    // parallel loop exit conditions into SSA form.
    Changed |= privatizeIterationBounds();
    return Changed;
  }
};


OMPDeclutter::OMPDeclutter() : FunctionPass(ID) {}

bool OMPDeclutter::runOnFunction(Function &F) {
#if 0
  static bool Dumped = false;
  if (!Dumped) {
    if (F.getParent()->getTargetTriple().substr(0,2) == "ve") {
      Dumped = true;
      std::error_code EC;
      raw_fd_ostream DumpOut("/tmp/declutter.ll", EC);
      F.getParent()->print(DumpOut, nullptr, true, true);
    }
  }
#endif

  IF_DEBUG_DEC {
    errs() << "PRE!\n";
    Dump(F);
  }

  auto &LI = getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
  auto &DT = getAnalysis<DominatorTreeWrapperPass>().getDomTree();
  OMPDeclutterSession Session(F, DT, LI);
  bool Res = Session.run();

  IF_DEBUG_DEC {
    errs() << "POST!\n";
    Dump(F);
  }

  return Res;
}

/// new pm plumbing.

OMPDeclutterWrapperPass::OMPDeclutterWrapperPass() {
}

llvm::PreservedAnalyses rv::OMPDeclutterWrapperPass::run(Function &F, FunctionAnalysisManager &FAM) {
  LoopInfo &LI = FAM.getResult<LoopAnalysis>(F);
  DominatorTree &DT = FAM.getResult<DominatorTreeAnalysis>(F);

  OMPDeclutterSession Session(F, DT, LI);
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
  AU.addRequired<DominatorTreeWrapperPass>();
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
