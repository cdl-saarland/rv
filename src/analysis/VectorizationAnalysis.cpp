//===- VectorizationAnalysis.cpp -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//

#include "rv/analysis/VectorizationAnalysis.h"
#include "rv/intrinsics.h"
#include "rv/shape/vectorShapeTransformer.h"

#include "rvConfig.h"
#include "utils/rvTools.h"
#include "utils/mathUtils.h"
#include "rv/analysis/AllocaSSA.h"

#include "llvm/IR/Dominators.h"
#include "llvm/Analysis/PostDominators.h"

#include <numeric>

#if 1
#define IF_DEBUG_VA IF_DEBUG
#else
#define IF_DEBUG_VA if (false)
#endif



using namespace llvm;

namespace rv {

char VAWrapperPass::ID = 0;

void
VAWrapperPass::getAnalysisUsage(AnalysisUsage& Info) const {
  Info.addRequired<DominatorTreeWrapperPass>();
  Info.addRequired<PostDominatorTreeWrapperPass>();
  Info.addRequired<LoopInfoWrapperPass>();
  Info.addRequired<VectorizationInfoProxyPass>();

  Info.setPreservesAll();
}

bool
VAWrapperPass::runOnFunction(Function& F) {
  auto& Vecinfo = getAnalysis<VectorizationInfoProxyPass>().getInfo();
  auto& platInfo = getAnalysis<VectorizationInfoProxyPass>().getPlatformInfo();

  const auto& domTree = getAnalysis<DominatorTreeWrapperPass>().getDomTree();
  const auto& postDomTree = getAnalysis<PostDominatorTreeWrapperPass>().getPostDomTree();
  const LoopInfo& LoopInfo = getAnalysis<LoopInfoWrapperPass>().getLoopInfo();

  VectorizationAnalysis vea(config, platInfo, Vecinfo, domTree, postDomTree, LoopInfo);
  vea.analyze();

  return false;
}

VectorizationAnalysis::VectorizationAnalysis(Config _config,
                                             PlatformInfo& platInfo,
                                             VectorizationInfo& VecInfo,
                                             const DominatorTree & domTree,
                                             const PostDominatorTree& postDomTree,
                                             const LoopInfo& LoopInfo)

        : config(_config),
          platInfo(platInfo),
          vecInfo(VecInfo),
          layout(platInfo.getDataLayout()),
          mLoopInfo(LoopInfo),
          SDA(domTree, postDomTree, LoopInfo),
          funcRegion(vecInfo.getScalarFunction()),
          funcRegionWrapper(funcRegion), // FIXME
          allocaSSA(funcRegionWrapper)
{
  // compute pointer provenance
  allocaSSA.compute();
  IF_DEBUG_VA allocaSSA.print(errs());
}

bool
VectorizationAnalysis::putOnWorklist(const llvm::Instruction& inst) {
  if (mOnWorklist.insert(&inst).second) { mWorklist.push(&inst); return true; }
  else return false;
}

const Instruction*
VectorizationAnalysis::takeFromWorklist() {
  if (mWorklist.empty()) return nullptr;
  const Instruction* I = mWorklist.front();
  mWorklist.pop();
  mOnWorklist.erase(I);
  return I;
}

void
VectorizationAnalysis::updateAnalysis(InstVec & updateList) {
  auto & F = vecInfo.getScalarFunction();
  assert (!F.isDeclaration());

  // start from instructions on the update list
  for (auto * inst : updateList) putOnWorklist(*inst);

  compute(F);
  fixUndefinedShapes(F);
  computeLoopDivergence();

  // mark all non-loop exiting branches as divergent to trigger a full linearization
  if (config.foldAllBranches) {
    for (auto & BB: F) {
      auto & term = *BB.getTerminator();
      if (term.getNumSuccessors() <= 1) continue; // uninteresting

      if (!vecInfo.inRegion(BB)) continue; // no begin vectorized

      auto * loop = mLoopInfo.getLoopFor(&BB);
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
void
VectorizationAnalysis::analyze() {
  auto & F = vecInfo.getScalarFunction();
  assert (!F.isDeclaration());

  init(F);
  compute(F);
  fixUndefinedShapes(F);
  computeLoopDivergence();

  // mark all non-loop exiting branches as divergent to trigger a full linearization
  if (config.foldAllBranches) {
    for (auto & BB: F) {
      auto & term = *BB.getTerminator();
      if (term.getNumSuccessors() <= 1) continue; // uninteresting

      if (!vecInfo.inRegion(BB)) continue; // no begin vectorized

      auto * loop = mLoopInfo.getLoopFor(&BB);
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

void VectorizationAnalysis::fixUndefinedShapes(const Function& F) {
  for (const BasicBlock& BB : F) {
    if (!vecInfo.inRegion(BB)) continue;
    for (const Instruction& I : BB) {
      if (!getShape(I).isDefined())
        vecInfo.setVectorShape(I, VectorShape::uni());
    }
  }
}

void VectorizationAnalysis::adjustValueShapes(const Function& F) {
  // Enforce shapes to be existing, if absent, set to VectorShape::undef()
  // If already there, also optimize alignment in case of pointer type

  // Arguments
  for (auto& arg : F.args()) {
    if (!vecInfo.hasKnownShape(arg)) {
      // assert(vecInfo.getRegion() && "will only default function args if in region mode");
      // set argument shapes to uniform if not known better
      vecInfo.setVectorShape(arg, VectorShape::uni());
    } else {
      // Adjust pointer argument alignment
      if (arg.getType()->isPointerTy()) {
        VectorShape argShape = vecInfo.getVectorShape(arg);
        unsigned minAlignment = getBaseAlignment(arg, layout);
        // max is the more precise one
        argShape.setAlignment(std::max<unsigned>(minAlignment, argShape.getAlignmentFirst()));
        vecInfo.setVectorShape(arg, argShape);
      }
    }
  }

  // Instructions in region(!)
  for (auto& BB : F) {
    if (vecInfo.inRegion(BB)) {
      for (auto& I : BB) {
        if (!vecInfo.hasKnownShape(I))
          vecInfo.setVectorShape(I, VectorShape::undef());
      }
    }
  }
}

void VectorizationAnalysis::init(const Function& F) {
  adjustValueShapes(F);

  // Propagation of vector shapes starts at values that do not depend on other values:
  // - function argument's users
  // - Allocas (which are uniform at the beginning)
  // - PHIs with constants as incoming values
  // - Calls without arguments

// push all users of pinned values
  for (auto * val : vecInfo.pinned_values()) {
    addDependentValuesToWL(val);
  }

// push non-user instructions
  for (const BasicBlock& BB : F) {
    vecInfo.setVectorShape(BB, VectorShape::uni());

    for (const Instruction& I : BB) {
      if (isa<AllocaInst>(&I)) {
        update(&I, VectorShape::uni(vecInfo.getMapping().vectorWidth));
      } else if (const CallInst* call = dyn_cast<CallInst>(&I)) {
        if (call->getNumArgOperands() != 0) continue;

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

void VectorizationAnalysis::update(const Value* const V, VectorShape AT) {
  bool changed = updateShape(V, AT);
  auto * term = dyn_cast<Instruction>(V);
  if (changed && term->isTerminator() && term->getNumSuccessors() > 1)
    analyzeDivergence(*term);
}

bool VectorizationAnalysis::updateShape(const Value* const V, VectorShape AT) {
  VectorShape Old = getShape(*V);
  VectorShape New = VectorShape::join(Old, AT);

  if (Old == New) {
    return false;// nothing changed
  }

  IF_DEBUG_VA {
    errs() << "Marking " << New << ": ";
    V->print(errs(), false);
    errs() << "\n";
  };

  vecInfo.setVectorShape(*V, New);

  // Add dependent elements to worklist
  addDependentValuesToWL(V);

  return true;
}

void VectorizationAnalysis::analyzeDivergence(const Instruction & termInst) {
  // Vectorization is caused by non-uniform branches
  if (getShape(termInst).isUniform()) return;

  // Find out which regions diverge because of this non-uniform branch
  // The branch is regarded as varying, even if its condition is only strided
  for (const auto* BB : SDA.join_blocks(termInst)) {
    if (!vecInfo.inRegion(*BB) || getShape(*BB).isVarying()) {
      continue;
    } // filter out irrelevant nodes (FIXME filter out directly in BDA)

    vecInfo.setVectorShape(*BB, VectorShape::varying());

    // Loop exit handling
    if (BB->getUniquePredecessor()) {
      vecInfo.setNotKillExit(BB);
    }

    IF_DEBUG_VA {
      errs() << "\nThe block:\n    ";
      BB->printAsOperand(errs(), false);
      errs() << "\nis divergent because of the non-uniform control in:\n    ";
      termInst.getParent()->printAsOperand(errs(), false);
      errs() << "\n\n";
    }

    // induce divergence into allocas
    const Join * allocaJoin = allocaSSA.getJoinNode(*BB);
    if (allocaJoin) {
      for (const auto * allocInst : allocaJoin->provSet.allocs) {
        updateShape(allocInst, VectorShape::varying());
      }
    }

    // add LCSSA phis to worklist
    for (auto & inst : *BB) {
      if (!isa<PHINode>(inst)) break;
      putOnWorklist(inst);

      IF_DEBUG_VA {
        errs() << "Inserted LCSSA PHI: ";
        inst.printAsOperand(errs(), false);
        errs() << "\n";
      };
    }
  }
}

void VectorizationAnalysis::addDependentValuesToWL(const Value* V) {
  // Push users of this value
  for (const auto user : V->users()) {
    if (!isa<Instruction>(user)) continue;
    const Instruction* inst = cast<Instruction>(user);

    // We are only analyzing the region
    if (!vecInfo.inRegion(*inst)) continue;

    putOnWorklist(*inst);
    IF_DEBUG_VA errs() << "Inserted users of " << *V << ":" << *user << "\n";
  }
}

VectorShape VectorizationAnalysis::joinIncomingValues(const PHINode& phi) {
  VectorShape Join = VectorShape::undef();
  for (size_t i = 0; i < phi.getNumIncomingValues(); ++i) {
    Join = VectorShape::join(Join, getShape(*phi.getIncomingValue(i)));
  }
  return Join;
}

bool VectorizationAnalysis::pushMissingOperands(const Instruction* I) {
  auto pushIfMissing = [this](bool prevpushed, Value* op)
  {
    bool push = isa<Instruction>(op) && !getShape(*op).isDefined();
    if (push) {
      auto & opInst = *cast<Instruction>(op);
      IF_DEBUG_VA { errs() << "\tmissing op shape " << opInst << "!\n"; }
      putOnWorklist(opInst);
    }

    return prevpushed || push;
  };

  return std::accumulate(I->op_begin(), I->op_end(), false, pushIfMissing);
}

VectorShape VectorizationAnalysis::computePHIShape(const PHINode & phi) {
   // The PHI node is not actually varying iff all input operands are the same
   // If the block is divergent the phi is varying
   if (getShape(*phi.getParent()).isVarying()) {
     // TODO infer greatest common alignment
     return VectorShape::varying();
   } else {
     return joinIncomingValues(phi);
   }
}

void VectorizationAnalysis::compute(const Function& F) {
  IF_DEBUG_VA { errs() << "\n\n-- VA::compute() log -- \n"; }

  VectorShapeTransformer vecShapeTrans(platInfo, vecInfo);

  // main fixed point loop
  while (true) {
    const Instruction * I = takeFromWorklist();
    if (!I) break; // worklist is empty

    if (vecInfo.isPinned(*I)) {
      continue;
    }

    IF_DEBUG_VA { errs() << "# next: " << *I << "\n"; }

    // compute the output shape
    VectorShape New;
    if (isa<PHINode>(I)) {
      // allow incomplete inputs for PHI nodes
      New = computePHIShape(cast<PHINode>(*I));
      if (!New.isDefined()) pushMissingOperands(I);
    } else if (pushMissingOperands(I)) {
      // If any operand is bottom put them in the work list.
      continue;
    } else {
      // Otw, we can compute the instruction shape
      SmallValVec taintedPtrOps;
      New = vecShapeTrans.computeShapeForInst(*I, taintedPtrOps);

      // taint allocas
      for (auto * ptr : taintedPtrOps) {
        const auto & prov = allocaSSA.getProvenance(*ptr);
        for (const auto * allocaInst : prov.allocs) {
          if (updateShape(allocaInst, VectorShape::varying())) {
            addDependentValuesToWL(allocaInst);
          }
        }
      }

      if (config.vaMethod == Config::VA_Karrenberg) {
        // degrade non-cont negatively strided shapes

        if (New.isVarying()) {
          // keep
        } else if (New.getStride() < 0) {
          // negative stride
          New = VectorShape::varying();
        } else if (New.isUniform()) {
          // keep uniform
        } else {
          // contiguous case
          int64_t contStride = 1;

          if (I->getType()->isPointerTy()) {
            // contiguous stride for pointers (bytes)
            auto *elemTy = I->getType()->getPointerElementType();
            contStride = layout.getTypeStoreSize(elemTy);
          }

          if (New.getStride() != contStride) {
            New = VectorShape::varying();
          }
        }

      } else if (config.vaMethod == Config::VA_Coutinho) {
        // only degrade alignment information
        New.setAlignment(1);
      }
    }

    IF_DEBUG_VA { errs() << "\t computed: " << New.str() << "\n"; }

    // if no output shape could be computed, skip.
    if (!New.isDefined()) continue;

    // shape is non-bottom. Apply general refinement rules.
    if (I->getType()->isPointerTy()) {
      // adjust result type to match alignment
      unsigned minAlignment = getBaseAlignment(*I, layout);
      New.setAlignment(std::max<unsigned>(minAlignment, New.getAlignmentFirst()));
    } else if (isa<FPMathOperator>(I)) {
      // allow strided/aligned fp values only in fast math mode
      FastMathFlags flags = I->getFastMathFlags();
      if (!flags.isFast() && !New.isUniform()) {
        New = VectorShape::varying();
      }
    }

    // if shape changed put users on worklist
    update(I, New);
  };
}



VectorShape VectorizationAnalysis::getShape(const Value& V) {
  return vecInfo.getVectorShape(V);
}

FunctionPass* createVectorizationAnalysisPass(rv::Config config) {
  return new VAWrapperPass(config);
}

void VectorizationAnalysis::computeLoopDivergence() {
  std::stack<const Loop*> loops;
  for (const Loop* l : mLoopInfo) {
    loops.push(l);
  }

  while (!loops.empty()) {
    const Loop* l = loops.top();
    loops.pop();

    SmallVector<BasicBlock*, 4> exits;
    l->getExitBlocks(exits);

    for (const BasicBlock* exit : exits) {
      if (vecInfo.getVectorShape(*exit).isVarying()) {
        vecInfo.setLoopDivergence(*l, false);
        break;
      }
    }

    for (const Loop* subLoop : l->getSubLoops()) {
      loops.push(subLoop);
    }
  }
}



}
