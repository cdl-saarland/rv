//===- src/analysis/VectorizationAnalysis.cpp - divergence analysis --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "rv/analysis/VectorizationAnalysis.h"
#include "rv/intrinsics.h"
#include "rv/shape/vectorShapeTransformer.h"

#include "rv/analysis/AllocaSSA.h"
#include "rvConfig.h"
#include "utils/mathUtils.h"
#include "utils/rvTools.h"

#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/IR/Dominators.h"

#include "report.h"

#include <numeric>

#if 1
#define IF_DEBUG_VA IF_DEBUG
#else
#define IF_DEBUG_VA if (true)
#endif

// FIXME
const bool IsLCSSAForm = true;

using namespace llvm;

namespace rv {

VectorizationAnalysis::VectorizationAnalysis(Config _config,
                                             PlatformInfo &platInfo,
                                             VectorizationInfo &VecInfo,
                                             FunctionAnalysisManager &FAM)
    : config(_config), platInfo(platInfo), vecInfo(VecInfo),
      layout(platInfo.getDataLayout()),
      LI(*FAM.getCachedResult<LoopAnalysis>(vecInfo.getScalarFunction())),
      DT(FAM.getResult<DominatorTreeAnalysis>(vecInfo.getScalarFunction())),
      SDA(DT,
          FAM.getResult<PostDominatorTreeAnalysis>(vecInfo.getScalarFunction()),
          LI),
      PredA(vecInfo, FAM.getResult<PostDominatorTreeAnalysis>(
                         vecInfo.getScalarFunction())),
      funcRegion(vecInfo.getScalarFunction()),
      funcRegionWrapper(funcRegion), // FIXME
      allocaSSA(funcRegionWrapper) {
  // compute pointer provenance
  allocaSSA.compute();
  IF_DEBUG_VA allocaSSA.print(errs());
}

bool VectorizationAnalysis::putOnWorklist(const llvm::Instruction &inst) {
  if (mOnWorklist.insert(&inst).second) {
    mWorklist.push(&inst);
    return true;
  } else
    return false;
}

const Instruction *VectorizationAnalysis::takeFromWorklist() {
  if (mWorklist.empty())
    return nullptr;
  const Instruction *I = mWorklist.front();
  mWorklist.pop();
  mOnWorklist.erase(I);
  return I;
}

bool VectorizationAnalysis::updateTerminator(const Instruction &Term) const {
  if (Term.getNumSuccessors() <= 1)
    return false;
  if (auto *BranchTerm = dyn_cast<BranchInst>(&Term)) {
    assert(BranchTerm->isConditional());
    return !getShape(*BranchTerm->getCondition()).isUniform();
  }
  if (auto *SwitchTerm = dyn_cast<SwitchInst>(&Term)) {
    return !getShape(*SwitchTerm->getCondition()).isUniform();
  }
  if (isa<InvokeInst>(Term)) {
    return false; // ignore abnormal executions through landingpad
  }

  llvm_unreachable("unexpected terminator");
}

// marks all users of loop-carried values of the loop headed by LoopHeader as
// divergent
void VectorizationAnalysis::taintLoopLiveOuts(const BasicBlock &LoopHeader) {
  auto *DivLoop = LI.getLoopFor(&LoopHeader);
  assert(DivLoop && "loopHeader is not actually part of a loop");

  SmallVector<BasicBlock *, 8> TaintStack;
  DivLoop->getExitBlocks(TaintStack);

  // Otherwise potential users of loop-carried values could be anywhere in the
  // dominance region of DivLoop (including its fringes for phi nodes)
  DenseSet<const BasicBlock *> Visited;
  for (auto *Block : TaintStack) {
    Visited.insert(Block);
  }
  Visited.insert(&LoopHeader);

  while (!TaintStack.empty()) {
    auto *UserBlock = TaintStack.back();
    TaintStack.pop_back();

    // don't spread divergence beyond the region
    if (!vecInfo.inRegion(*UserBlock))
      continue;

    assert(!DivLoop->contains(UserBlock) &&
           "irreducible control flow detected");

    // phi nodes at the fringes of the dominance region
    if (!DT.dominates(&LoopHeader, UserBlock)) {
      // all PHI nodes of UserBlock become divergent
      for (auto &Phi : UserBlock->phis()) {
        putOnWorklist(Phi);
      }
      continue;
    }

    // taint outside users of values carried by DivLoop
    for (auto &I : *UserBlock) {
      if (vecInfo.isPinned(I))
        continue;
      // if (isDivergent(I))
      //   continue;

      for (auto &Op : I.operands()) {
        auto *OpInst = dyn_cast<Instruction>(&Op);
        if (!OpInst)
          continue;
        if (DivLoop->contains(OpInst->getParent())) {
          putOnWorklist(I);
          // markDivergent(I);
          // pushUsers(I);
          break;
        }
      }
    }

    // visit all blocks in the dominance region
    for (auto *SuccBlock : successors(UserBlock)) {
      if (!Visited.insert(SuccBlock).second) {
        continue;
      }
      TaintStack.push_back(SuccBlock);
    }
  }
}

void VectorizationAnalysis::pushPHINodes(const BasicBlock &Block) {
  // induce divergence into allocas
  const Join *allocaJoin = allocaSSA.getJoinNode(Block);
  if (allocaJoin) {
    for (const auto *allocInst : allocaJoin->provSet.allocs) {
      updateShape(*allocInst, VectorShape::varying());
    }
  }

  for (const auto &Phi : Block.phis()) {
    //  if (isDivergent(Phi))
    //    continue;
    putOnWorklist(Phi);
  }
}

/// \p returns whether divergence in \p JoinBlock causes loop divergence in \p
/// BranchLoop.
bool VectorizationAnalysis::propagateJoinDivergence(const BasicBlock &JoinBlock,
                                                    const Loop *BranchLoop) {
  IF_DEBUG_VA { errs() << "\tVA: propJoinDiv " << JoinBlock.getName() << "\n"; }

  // ignore divergence outside the region
  if (!vecInfo.inRegion(JoinBlock)) {
    Report() << "VA: detected divergent join outside the region in block " << JoinBlock.getName() << "!\n";
    return false;
  }

  // JoinBlock is a divergent loop exit
  // We have to test this first to make sure that divergent loop exits
  // trigger loop-divergence detection for multi-level loop exiting edges.
  if (BranchLoop && !BranchLoop->contains(&JoinBlock)) {
    vecInfo.addJoinDivergentBlock(JoinBlock);
    pushPHINodes(JoinBlock);
    return true;
  }

  // Otw, JoinBlock is an acyclic divergent join
  if (!vecInfo.addJoinDivergentBlock(JoinBlock))
    return false;

  // push non-divergent phi nodes in JoinBlock to the worklist
  pushPHINodes(JoinBlock);
  return false;
}

using SmallConstBlockVec = SmallVector<const BasicBlock*, 4>;
using SmallConstBlockSet = SmallPtrSet<const BasicBlock*, 4>;

SmallConstBlockVec
GetUniqueSuccessors(const Instruction & Term) {
  // const auto & Term = *BB.getTerminator();
  SmallConstBlockSet seenBefore;
  SmallConstBlockVec termSuccs;
  for (int i = 0; i < (int) Term.getNumSuccessors(); ++i) {
    const auto * anotherSucc = Term.getSuccessor(i);
    if (!seenBefore.insert(anotherSucc).second) continue;
    termSuccs.push_back(anotherSucc);
  }
  return termSuccs;
}

template<typename RootNodeType>
void
VectorizationAnalysis::propagateControlDivergence(const Loop * BranchLoop, llvm::ArrayRef<const BasicBlock*> UniqueSuccessors, RootNodeType & rootNode, const BasicBlock & domBoundBlock) {
  bool IsBranchLoopDivergent = false;

  // Iterates over the following:
  // a) Blocks that are reachable by disjoint paths from \p rootNode.
  // b) Loop exits (of the inner most loop carrying \p rootNode) that becomes divergent due to divergence in in \p Term.
  for (const BasicBlock *JoinBlock : SDA.join_blocks(rootNode)) {
    if (!vecInfo.inRegion(*JoinBlock)) {
      IF_DEBUG_VA {
        errs() << "VA: Ignoring divergent join outside region: "
               << JoinBlock->getName() << "\n";
      }
      continue;
    }

    // propagates disjoint paths divergence to join points
    bool causedLoopDivergence = propagateJoinDivergence(*JoinBlock, BranchLoop);

    if (causedLoopDivergence) {
      vecInfo.addDivergentLoopExit(*JoinBlock);
      IsBranchLoopDivergent = true;
    }
  }

  // Block predicates may turn varying due to the divergence of this branch
  PredA.addDivergentBranch(domBoundBlock, UniqueSuccessors, [&](const BasicBlock & varPredBlock) {
      pushPredicatedInsts(varPredBlock);
  });

  // Branch loop is a divergent loop due to the divergent branch in Term
  if (IsBranchLoopDivergent) {
    assert(BranchLoop);
    IF_DEBUG_VA {
      errs() << "VA: Detected divergent loop: " << BranchLoop->getName()
             << "\n";
    }
    if (!vecInfo.addDivergentLoop(*BranchLoop)) {
      return;
    }
    propagateLoopDivergence(*BranchLoop);
  }
}

void VectorizationAnalysis::propagateBranchDivergence(const Instruction &Term) {
  IF_DEBUG_VA {
    errs() << "VE: propBranchDiv " << Term.getParent()->getName() << "\n";
  }

  const auto *BranchLoop = LI.getLoopFor(Term.getParent());

  auto termSuccVec = GetUniqueSuccessors(Term);

  const auto & termBlock = *Term.getParent();
  propagateControlDivergence<const Instruction>(BranchLoop, termSuccVec, Term, termBlock);
}

void VectorizationAnalysis::propagateLoopDivergence(const Loop &ExitingLoop) {
  IF_DEBUG_VA { errs() << "VA: propLoopDiv " << ExitingLoop.getName() << "\n"; }

  // don't propagate beyond region
  if (!vecInfo.inRegion(*ExitingLoop.getHeader()))
    return;

  const auto & loopHeader = *ExitingLoop.getHeader();

  const auto *BranchLoop = ExitingLoop.getParentLoop();

  // Uses of loop-carried values could occur anywhere
  // within the dominance region of the definition. All loop-carried
  // definitions are dominated by the loop header (reducible control).
  // Thus all users have to be in the dominance region of the loop header,
  // except PHI nodes that can also live at the fringes of the dom region
  // (incoming defining value).
  if (!IsLCSSAForm) {
    taintLoopLiveOuts(loopHeader); // FIXME AllocaSSA
  }

  SmallConstBlockVec exitBlockVec;
  {
    // TODO fix un-constness of decltype(::getUniqueExitBlocks(..)) in LLVM
    llvm::SmallVector<BasicBlock*, 4> tmpUniqueExitBlockVec;
    ExitingLoop.getUniqueExitBlocks(tmpUniqueExitBlockVec);
    for (const auto * BB : tmpUniqueExitBlockVec) exitBlockVec.push_back(BB);
  }

  propagateControlDivergence<const Loop>(BranchLoop, exitBlockVec, ExitingLoop, loopHeader);
}

void VectorizationAnalysis::compute(const Function &F) {
  IF_DEBUG_VA { errs() << "\n\n-- VA::compute() log -- \n"; }

  VectorShapeTransformer vecShapeTrans(LI, platInfo, vecInfo);

  // main fixed point loop
  while (const Instruction *nextI = takeFromWorklist()) {
    const Instruction &I = *nextI;

    if (vecInfo.isPinned(I)) {
      continue;
    }

    IF_DEBUG_VA { errs() << "# next: " << I << "\n"; }

    // propagate divergence caused by terminator
    if (I.isTerminator()) {
      if (updateTerminator(I)) {
        vecInfo.setVectorShape(
            I, VectorShape::varying()); // TODO use the condition shape instead
                                        // of terminator shapes
        // propagate control divergence to affected instructions
        propagateBranchDivergence(I);
        continue;
      }
    }

    // compute the output shape
    VectorShape New;
    if (isa<PHINode>(I)) {
      // allow incomplete inputs for PHI nodes
      New = vecShapeTrans.computeShapeForPHINode(cast<PHINode>(I));
      if (!New.isDefined())
        pushMissingOperands(I); // FIXME this could diverge
    } else if (pushMissingOperands(I)) {
      // If any operand is bottom put them in the work list.
      continue;
    } else {
      // Otw, we can compute the instruction shape
      SmallValVec taintedPtrOps;
      New = vecShapeTrans.computeShapeForInst(I, taintedPtrOps);

      // taint allocas
      for (auto *ptr : taintedPtrOps) {
        const auto &prov = allocaSSA.getProvenance(*ptr);
        for (const auto *allocaInst : prov.allocs) {
          updateShape(*allocaInst, VectorShape::varying());
        }
      }
    }

    IF_DEBUG_VA { errs() << "\t computed: " << New.str() << "\n"; }

    // if no output shape could be computed, skip.
#if 0
    if (!New.isDefined())
      continue;
#endif

    // TODO factor this into vectorShapeTransform. This does not belong here!
    // shape is non-bottom. Apply general refinement rules.
    if (I.getType()->isPointerTy()) {
      // adjust result type to match alignment
      unsigned minAlignment = I.getPointerAlignment(layout);
      New.setAlignment(
          std::max<unsigned>(minAlignment, New.getAlignmentFirst()));
    } else if (isa<FPMathOperator>(I)) {
      // allow strided/aligned fp values only in fast math mode
      FastMathFlags flags = I.getFastMathFlags();
      if (!flags.isFast() && !New.isUniform()) {
        New = VectorShape::varying();
      }
    }

    // if shape changed put users on worklist
    updateShape(I, New);
  };
}

void VectorizationAnalysis::analyze() {
  auto &F = vecInfo.getScalarFunction();
  assert(!F.isDeclaration());

  // seed sources of divergence
  init(F);

  // propagate divergence
  compute(F);

  // replace undef instruction shapes with uniform
  promoteUndefShapesToUniform(F);

  // mark all non-loop exiting branches as divergent to trigger a full
  // linearization
  // FIXME factor this out into a separate transformation
  if (config.foldAllBranches) {
    for (auto &BB : F) {
      auto &term = *BB.getTerminator();
      if (term.getNumSuccessors() <= 1)
        continue; // uninteresting

      if (!vecInfo.inRegion(BB))
        continue; // no begin vectorized

      auto *loop = LI.getLoopFor(&BB);
      bool keepShape = loop && loop->isLoopExiting(&BB);

      if (!keepShape) {
        vecInfo.setVectorShape(term, VectorShape::varying());
      }
    }
  }

  IF_DEBUG_VA {
    errs() << "VecInfo after VA:\n";
    vecInfo.dump();
  }
}

void VectorizationAnalysis::promoteUndefShapesToUniform(const Function &F) {
  for (const BasicBlock &BB : F) {
    if (!vecInfo.inRegion(BB))
      continue;
    for (const Instruction &I : BB) {
      if (!getShape(I).isDefined())
        vecInfo.setVectorShape(I, VectorShape::uni());
    }
  }
}

void VectorizationAnalysis::adjustValueShapes(const Function &F) {
  // Enforce shapes to be existing, if absent, set to VectorShape::undef()
  // If already there, also optimize alignment in case of pointer type

  // Arguments
  for (auto &arg : F.args()) {
    if (!vecInfo.hasKnownShape(arg)) {
      // assert(vecInfo.getRegion() && "will only default function args if in
      // region mode"); set argument shapes to uniform if not known better
      vecInfo.setVectorShape(arg, VectorShape::uni());
    } else {
      // Adjust pointer argument alignment
      if (arg.getType()->isPointerTy()) {
        VectorShape argShape = getShape(arg);
        unsigned minAlignment = arg.getPointerAlignment(layout);
        // max is the more precise one
        argShape.setAlignment(
            std::max<unsigned>(minAlignment, argShape.getAlignmentFirst()));
        vecInfo.setVectorShape(arg, argShape);
      }
    }
  }

  // Instructions in region(!)
#if 0
  for (auto &BB : F) {
    if (vecInfo.inRegion(BB)) {
      for (auto &I : BB) {
        if (!vecInfo.hasKnownShape(I))
          vecInfo.setVectorShape(I, VectorShape::undef());
      }
    }
  }
#endif
}

void VectorizationAnalysis::init(const Function &F) {
  adjustValueShapes(F);

  // Propagation of vector shapes starts at values that do not depend on other
  // values:
  // - function argument's users
  // - Allocas (which are uniform at the beginning)
  // - PHIs with constants as incoming values
  // - Calls without arguments

  // push all users of pinned values
  for (auto *val : vecInfo.pinned_values()) {
    pushUsers(*val);
  }

  // push non-user instructions
  for (const BasicBlock &BB : F) {
    vecInfo.setVectorShape(BB, VectorShape::uni());

    for (const Instruction &I : BB) {
      if (isa<AllocaInst>(&I)) {
        updateShape(I, VectorShape::uni(vecInfo.getMapping().vectorWidth));
      } else if (const CallInst *call = dyn_cast<CallInst>(&I)) {
        if (call->getNumArgOperands() != 0)
          continue;

        putOnWorklist(I);

        IF_DEBUG_VA {
          errs() << "Inserted call in initialization: ";
          I.printAsOperand(errs(), false);
          errs() << "\n";
        };
      } else if (isa<PHINode>(I) && any_of(I.operands(), isa<Constant, Use>)) {
        // Phis that depend on constants are added to the WL
        putOnWorklist(I);

        IF_DEBUG_VA {
          errs() << "Inserted PHI in initialization: ";
          I.printAsOperand(errs(), false);
          errs() << "\n";
        };
      }
    }
  }
}

bool VectorizationAnalysis::updateShape(const Value &V, VectorShape AT) {
  VectorShape Old = getShape(V);
  VectorShape New = VectorShape::join(Old, AT);

  // if the value has an initialized shape identical to the new one stop here
  if (vecInfo.hasKnownShape(V) && (Old == New)) {
    return false; // nothing changed
  }

  IF_DEBUG_VA {
    errs() << "Marking " << New << ": ";
    V.print(errs(), false);
    errs() << "\n";
  };

  // register new shape
  vecInfo.setVectorShape(V, New);

  // Add dependent elements to worklist
  pushUsers(V);

  return true;
}

void
VectorizationAnalysis::pushPredicatedInsts(const llvm::BasicBlock & BB) {
  IF_DEBUG_VA { errs() << "VA: Pushing predicate-dependent insts of " << BB.getName() << ":\n"; }
  for (const auto & Inst : BB) {
    // skip over insts whose result shape does not depend on the block predicate
    if (isa<PHINode>(Inst)) continue;
    if (isa<BinaryOperator>(Inst)) continue;
    if (Inst.isTerminator()) continue;

    IF_DEBUG_VA { errs() << "\tPushed: " << Inst << "\n"; }
    putOnWorklist(Inst);
  }
}

void VectorizationAnalysis::pushUsers(const Value &V) {
  IF_DEBUG_VA { errs() << "VA: Pushing users of " << V << "\n"; }
  // Push users of this value
  for (const auto user : V.users()) {
    if (!isa<Instruction>(user))
      continue;
    const Instruction &inst = cast<Instruction>(*user);

    // We are only analyzing the region
    if (!vecInfo.inRegion(inst))
      continue;

    putOnWorklist(inst);
    IF_DEBUG_VA { errs() << "\tPushed: " << inst << "\n"; }
  }
}

bool VectorizationAnalysis::pushMissingOperands(const Instruction &I) {
  auto pushIfMissing = [this](bool prevpushed, Value *op) {
    bool push = isa<Instruction>(op) && !vecInfo.hasKnownShape(*op);
    if (push) {
      auto &opInst = *cast<Instruction>(op);
      IF_DEBUG_VA { errs() << "\tmissing op shape " << opInst << "!\n"; }
      putOnWorklist(opInst);
    }

    return prevpushed || push;
  };

  return std::accumulate(I.op_begin(), I.op_end(), false, pushIfMissing);
}

VectorShape VectorizationAnalysis::getShape(const Value &V) const {
  return vecInfo.hasKnownShape(V) ? vecInfo.getVectorShape(V) : VectorShape::undef();
}

} // namespace rv
