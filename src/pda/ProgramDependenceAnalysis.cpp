//===- ProgramDependenceAnalysis.cpp -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// @authors haffner, kloessner, simon
//

#include "rv/pda/ProgramDependenceAnalysis.h"

#include "rvConfig.h"
#include "utils/rvTools.h"
#include "rv/utils/mathUtils.h"

#if 1
#define IF_DEBUG_PDA IF_DEBUG
#else
#define IF_DEBUG_PDA if (false)
#endif

namespace rv {

using ValueMap = std::map<const Value*, VectorShape>;
using FuncInfo = native::VectorMappingMap;

// #define BYTE_SIZE 8

char PDAWrapperPass::ID = 0;

bool
PDAWrapperPass::runOnFunction(Function& F) {
  VectorizationInfo& Vecinfo = getAnalysis<VectorizationInfoProxyPass>().getInfo();

  const CDG& cdg = *getAnalysis<llvm::CDGWrapper>().getDFG();
  const DFG& dfg = *getAnalysis<llvm::DFGWrapper>().getDFG();
  const FuncInfo& FuncInfo = getAnalysis<RVInfoProxyPass>().getInfo().getVectorFuncMap();
  const LoopInfo& LoopInfo = getAnalysis<LoopInfoWrapperPass>().getLoopInfo();

  PDA pda(Vecinfo, cdg, dfg, FuncInfo, LoopInfo);
  pda.analyze(F);

  return false;
}

PDA::PDA(VectorizationInfo& VecInfo,
         const CDG& cdg,
         const DFG& dfg,
         const FuncInfo& Funcinfo,
         const LoopInfo& LoopInfo)

        : layout(""),
          mVecinfo(VecInfo),
          mCDG(cdg),
          mDFG(dfg),
          mLoopInfo(LoopInfo),
          mFuncinfo(Funcinfo),
          mRegion(mVecinfo.getRegion())
{ }

void
PDA::analyze(Function& F) {
  assert (!F.isDeclaration());

  mWorklist.clear();

  init(F);
  compute(F);
  fillVectorizationInfo(F);

  markDivergentLoopLatchesMandatory();
  // checkEquivalentToOldAnalysis(F);
}

bool PDA::isInRegion(const BasicBlock* BB) {
  return mRegion ? mRegion->contains(BB) : true;
}

bool PDA::isInRegion(const Instruction& inst) {
  return !mRegion || isInRegion(inst.getParent());
}

void PDA::fillVectorizationInfo(Function& F) {
  for (const BasicBlock& BB : F) {
    mVecinfo.setVectorShape(BB, mValue2Shape[&BB]);
    for (const Instruction& I : BB) {
      VectorShape& shape = mValue2Shape[&I];
      mVecinfo.setVectorShape(I, shape.isDefined() ? shape : VectorShape::uni());
    }
  }
}

unsigned PDA::getAlignment(const Constant* c) const {
  assert (c);
  assert (!isa<BasicBlock>(c));
  assert (!isa<Function>(c));

  // An undef value is never aligned.
  if (isa<UndefValue>(c)) return 1;

  if (const ConstantInt* cint = dyn_cast<ConstantInt>(c)) {
    return static_cast<unsigned>(std::abs(cint->getSExtValue()));
  }

  // Other than that, only integer vector constants can be aligned.
  if (!c->getType()->isVectorTy()) return 1;

  // A zero-vector is aligned.
  if (isa<ConstantAggregateZero>(c)) return 0;

  if (const ConstantDataVector* cdv = dyn_cast<ConstantDataVector>(c)) {
    if (!cdv->getElementType()->isIntegerTy()) return 1;

    const int intValue = (int) cast<ConstantInt>(cdv->getAggregateElement(0U))->getSExtValue();

    return static_cast<unsigned>(std::abs(intValue));
  }

  assert (isa<ConstantVector>(c));
  const ConstantVector* cv = cast<ConstantVector>(c);

  if (!cv->getType()->getElementType()->isIntegerTy()) return 1;

  assert (isa<ConstantInt>(cv->getOperand(0)));
  const ConstantInt* celem = cast<ConstantInt>(cv->getOperand(0));
  const int intValue = (int) celem->getSExtValue();

  // The vector is aligned if its first element is aligned
  return static_cast<unsigned>(std::abs(intValue));
}

void PDA::init(Function& F) {
  layout = DataLayout(F.getParent());

  // Initialize with undefined values
  for (auto& arg : F.args()) mValue2Shape[&arg] = VectorShape::undef();
  for (auto& BB : F) for (auto& I : BB) mValue2Shape[&I] = VectorShape::undef();

  // Update initialized instructions
  for (auto& arg : F.args()) {
    if (mVecinfo.hasKnownShape(arg)) {
      VectorShape argShape = mVecinfo.getVectorShape(arg);

      /* Adjust pointer arguments to continous stride */
      if (arg.getType()->isPointerTy() && argShape.isVarying()) {
        argShape = VectorShape::cont();
      }

      update(&arg, argShape);
    }
    else {
      assert(mRegion && "will only default function args if in region mode");
      // set argument shapes to uniform if not known better
      update(&arg, VectorShape::uni());
    }
  }

  // bootstrap with user defined shapes
  for (auto& BB : F) {
    for (auto& I : BB) {
      if (mVecinfo.hasKnownShape(I)) {
        overrides.insert(&I);
        update(&I, mVecinfo.getVectorShape(I));
        mVecinfo.dropVectorShape(I);
      }
      else {
        mVecinfo.setVectorShape(I, VectorShape::uni());
      }
    }
  }

  // Propagation of vectorshapes starts at:
  // - Allocas
  // - Constants
  // - Calls (theres no connection to them in the iteration if they have no parameters)
  for (const BasicBlock& BB : F) {
    mValue2Shape[&BB] = VectorShape::uni();

    for (const Instruction& I : BB) {
      if (isa<AllocaInst>(&I)) {
        update(&I, VectorShape::uni(mVecinfo.getMapping().vectorWidth));
      }
        /* Need to initialize WL with calls, they may not be reached o.w. */
      else if (const CallInst* call = dyn_cast<CallInst>(&I)) {
        // Only makes sense if a value is returned
        if (call->getCalledFunction()->getReturnType()->isVoidTy()) continue;

        mWorklist.insert(&I);
        IF_DEBUG_PDA errs() << "Inserted call in initialization: " << I.getName() << "\n";
      }
        /* Phis that depend on constants are added to the WL */
      else if (isa<PHINode>(I) && any_of(I.operands(), isa<Constant, Use>)) {
        mWorklist.insert(&I);
        IF_DEBUG_PDA outs() << "Inserted PHI in initialization: " << I.getName() << "\n";
      }
    }
  }
}

void PDA::update(const Value* const V, VectorShape AT) {
  const VectorShape& New = VectorShape::join(getShape(V), AT);

  if (mValue2Shape[V] == New) return;// nothing changed
  if (overrides.count(V)) return;//prevented by override

  IF_DEBUG_PDA outs() << "Marking " << New << ": " << *V << "\n";

  mValue2Shape[V] = New;

  /* Add dependent elements to worklist */
  addRelevantUsersToWL(V);

  // Nothing else needs to be done for uniform values
  if (New.isUniform()) return;

  /* If an alloca was used it needs to be updated */
  if (isa<Instruction>(V)) {
    updateAllocaOperands(cast<Instruction>(V));
  }


  /* Begin with divergence analysis */

  // Divergence is caused directly by branches only
  if (!isa<BranchInst>(V)) return;

  // Find out which regions diverge because of this non-uniform branch
  // The branch is regarded as varying, even if its condition is only strided
  const BranchInst* branch = cast<BranchInst>(V);
  assert (branch->isConditional());

  const BasicBlock* endsVarying = branch->getParent();
  const Loop* endsVaryingLoop = mLoopInfo.getLoopFor(endsVarying);

  markSuccessorsMandatory(endsVarying);
  markDependentLoopExitsMandatory(endsVarying);

  const CDNode* cd_node = mCDG[endsVarying];
  // Iterate over the predeccessors in the dfg, of the successors in the cdg
  for (const CDNode* cd_succ : cd_node->succs()) {
    const DFNode* df_node = mDFG[cd_succ->getBB()];
    for (const DFNode* df_pred : df_node->preds()) {
      // Get the block BB that is affected by the varying branch
      const BasicBlock* const BB = df_pred->getBB();

      IF_DEBUG errs() << "Branch " << *branch << " affects " << *BB << "\n";

      assert (isInRegion(BB)); // Otherwise the region is ill-formed

      if (mValue2Shape[BB].isVarying()) continue;

      bool causedDivergence;

      // Loop headers are not marked divergent, but can be loop divergent
      if (mLoopInfo.isLoopHeader(BB)) {
        const Loop* BBLoop = mLoopInfo.getLoopFor(BB);

        // BB needs to be a header of the same or an outer loop
        if (!BBLoop->contains(endsVaryingLoop)) continue;

        const bool BranchExitsBBLoop = BBLoop->isLoopExiting(endsVarying);
        if (BBLoop == endsVaryingLoop || BranchExitsBBLoop) {
          mVecinfo.setDivergentLoop(BBLoop);

          IF_DEBUG_PDA {
            outs() << "\n"
                   << "The loop with header: \n"
                   << "    " << BB->getName() << "\n"
                   << "is divergent because of the non-uniform branch in:\n"
                   << "    " << endsVarying->getName() << "\n\n";
          }

          for (auto it = BB->begin(); BB->getFirstNonPHI() != &*it; ++it) {
            updateOutsideLoopUsesVarying(BBLoop);
          }
        }

        continue;
      }

      // If the dependence exits the loop, we need to blackbox the loop
      // If any loop exit is varying, the block is divergent since the loop might
      // leak information before every thread is done
      if (endsVaryingLoop && !endsVaryingLoop->contains(BB)) {
        // In case of only one exit block we can optimize considering it gets
        // linearized
        if (endsVaryingLoop->getUniqueExitBlock()) continue;

        // For multiple exits, the loop shall be blackboxed
        const VectorShape& JoinedExitShape = joinExitShapes(endsVaryingLoop);
        causedDivergence = !JoinedExitShape.isUniform();
        mValue2Shape[BB] = VectorShape::join(getShape(BB), JoinedExitShape);
      }
      else {
        causedDivergence = true;
        mValue2Shape[BB] = VectorShape::varying();
      }

      if (causedDivergence) {
        IF_DEBUG_PDA {
          outs() << "\n"
                 << "The block:\n"
                 << "    " << BB->getName() << "\n"
                 << "is divergent because of the non-uniform branch in:\n"
                 << "    " << endsVarying->getName() << "\n\n";
        }

        // MANDATORY case 2: divergent block
        mVecinfo.markMandatory(BB);
      }

      // add phis to worklist
      for (auto it = BB->begin(); BB->getFirstNonPHI() != &*it; ++it) {
        mWorklist.insert(&*it);
        IF_DEBUG_PDA outs() << "Inserted PHI: " << (&*it)->getName() << "\n";
      }
    }
  }
}

void PDA::updateAllocaOperands(const Instruction* I) {
  const int alignment = mVecinfo.getMapping().vectorWidth;

  for (const Value* op : I->operands()) {
    if (!isa<AllocaInst>(op)) continue;

    // Already processed
    if (!getShape(op).isUniform()) continue;

    eraseUserInfoRecursively(op);

    auto* PtrElemType = op->getType()->getPointerElementType();
    const bool Vectorizable = rv::isVectorizableNonDerivedType(*PtrElemType);

    int typeStoreSize = static_cast<int>(layout.getTypeStoreSize(PtrElemType));

    update(op, Vectorizable ?
               VectorShape::strided(typeStoreSize, alignment) :
               VectorShape::varying(alignment));
  }
}

void PDA::eraseUserInfoRecursively(const Value* V) {
  if (!mValue2Shape[V].isDefined()) return;

  mValue2Shape[V] = VectorShape::undef();

  for (const Value* use : V->users()) {
    eraseUserInfoRecursively(use);
  }
}

void PDA::addRelevantUsersToWL(const Value* V) {
  for (auto user : V->users()) {
    // We are only analyzing the region
    if (!isInRegion(*cast<Instruction>(user))) continue;

    // Ignore calls without return value
    if (const CallInst* callI = dyn_cast<CallInst>(user)) {
      if (callI->getCalledFunction()->getReturnType()->isVoidTy()) {
        continue;
      }
    }

    mWorklist.insert(cast<Instruction>(user));
    IF_DEBUG_PDA outs() << "Inserted relevant user of " << V->getName() << ":" << *user << "\n";
  }
}

void PDA::updateOutsideLoopUsesVarying(const Loop* divLoop) {
  SmallVector<BasicBlock*, 3> exitBlocks;
  divLoop->getExitBlocks(exitBlocks);
  for (auto* exitBlock : exitBlocks) {
    if (!isInRegion(exitBlock)) continue;

    for (auto& inst : *exitBlock) {
      auto* phi = dyn_cast<PHINode>(&inst);
      if (!phi) break;

      // find outside uses and update varying
      if (!divLoop->contains(mLoopInfo.getLoopFor(phi->getParent()))) {
        update(phi, VectorShape::varying());
      }

    }
  }
}

VectorShape PDA::joinExitShapes(const Loop* loop) {
  SmallVector<BasicBlock*, 4> exitingBlocks;
  loop->getExitingBlocks(exitingBlocks);

  VectorShape CombinedExitShape = VectorShape::uni();
  for (BasicBlock* exitingBB : exitingBlocks) {
    const TerminatorInst* terminator = exitingBB->getTerminator();
    CombinedExitShape = VectorShape::join(getShape(terminator), CombinedExitShape);
  }

  return CombinedExitShape;
}

void PDA::markDependentLoopExitsMandatory(const BasicBlock* endsVarying) {
  Loop* loop = mLoopInfo.getLoopFor(endsVarying);

  // No exits that can be mandatory
  if (!loop) return;

  // MANDATORY case 4: can be nicely put with just the cdg,
  //                   if an exit is control dependent on
  //                   this varying branch, then there is
  //                   one path that stays in the loop and
  //                   eventually reaches the latch, and one
  //                   that reaches the exit, as its control dependent

  SmallVector<BasicBlock*, 8> LoopExits;
  loop->getExitBlocks(LoopExits);

  const CDNode* cd_node = mCDG[endsVarying];
  for (const CDNode* cd_succ : cd_node->succs()) {
    for (const BasicBlock* exit : LoopExits) {
      if (exit == cd_succ->getBB()) {
        mVecinfo.markMandatory(exit);
      }
    }
  }
}

void PDA::markSuccessorsMandatory(const BasicBlock* endsVarying) {
  // MANDATORY case 1: successors of varying branches are mandatory
  for (const BasicBlock* succBB : successors(endsVarying)) {
    mVecinfo.markMandatory(succBB);
  }
}

VectorShape PDA::joinOperands(const Instruction* const I) {
  VectorShape Join = VectorShape::undef();
  for (auto& op : I->operands()) Join = VectorShape::join(Join, getShape(op));
  return Join;
}

bool PDA::allOperandsHaveShape(const Instruction* I) {
  auto hasKnownShape = [this](Value* op)
  {
    return !isa<Instruction>(op) || mValue2Shape[op].isDefined();
  };

  return all_of(I->operands(), hasKnownShape);
}

void PDA::compute(Function& F) {
  /* Worklist algorithm to compute the least fixed-point */
  while (!mWorklist.empty()) {
    const Instruction* I = *mWorklist.begin();
    mWorklist.erase(I);

    // Skip until all operands have a shape
    if (!isa<PHINode>(I) && !allOperandsHaveShape(I)) continue;

    const VectorShape& New = computeShapeForInst(I);
    update(I, New);
  }
}

VectorShape PDA::computeShapeForInst(const Instruction* I) {
  if (I->isBinaryOp()) return computeShapeForBinaryInst(cast<BinaryOperator>(I));
  if (I->isCast()) return computeShapeForCastInst(cast<CastInst>(I));

  switch (I->getOpcode()) {
    case Instruction::Br:
    {
      const BranchInst* branch = cast<BranchInst>(I);
      assert(branch->isConditional());
      return getShape(branch->getCondition());
    }
    case Instruction::Switch:
    {
      const SwitchInst* sw = cast<SwitchInst>(I);
      return getShape(sw->getCondition());
    }
    case Instruction::ICmp:
    {
      const Value* op1 = I->getOperand(0);
      const Value* op2 = I->getOperand(1);

      const VectorShape& shape1 = getShape(op1);
      const VectorShape& shape2 = getShape(op2);

      // If both operands have the same stride the comparison is uniform
      if (shape1.isVarying() || shape2.isVarying()) return VectorShape::varying();
      if (shape1.getStride() == shape2.getStride()) return VectorShape::uni();

      using Pred_t = CmpInst::Predicate;
      Pred_t predicate = cast<CmpInst>(I)->getPredicate();
      const int strideDiff = shape1.getStride() - shape2.getStride();

      const unsigned vectorWidth = mVecinfo.getMapping().vectorWidth;

      const unsigned alignment1 = shape1.getAlignment();
      const unsigned alignment2 = shape2.getAlignment();

      if (predicate == Pred_t::ICMP_SLT || predicate == Pred_t::ICMP_ULT) {
        // There are 2 possibilities:
        // 1. The first vector1 entry is not smaller than the first vector2 entry
        //    If strideDiff >= 0, the gap does not decrease and so all other entries
        //    are pairwise not smaller
        // 2. The first vector1 entry is smaller...
        //    If the alignment a is the same, we can at least add a until we are possibly
        //    equal. But since strideDiff * vectorWidth <= a, the accumulated difference
        //    does not exceed a and all entries are pairwise smaller
        if (strideDiff >= 0 && alignment1 == alignment2 && strideDiff * vectorWidth <= alignment1)
          return VectorShape::uni();
      }
      else if (predicate == Pred_t::ICMP_SGT || predicate == Pred_t::ICMP_UGT) {
        // Analogous reasoning to the one above
        if (strideDiff <= 0 && alignment1 == alignment2 && -strideDiff * vectorWidth <= alignment1)
          return VectorShape::uni();
      }

      return VectorShape::varying();
    }

    case Instruction::GetElementPtr:
    {
      const GetElementPtrInst* gep = cast<GetElementPtrInst>(I);
      const Value* pointer = gep->getPointerOperand();

      VectorShape result = getShape(pointer);
      Type* subT = gep->getPointerOperandType();

      for (const Value* index : make_range(gep->idx_begin(), gep->idx_end())) {
        const VectorShape& indexShape = getShape(index);
        const int indexStride = indexShape.getStride();
        const int indexAlignment = indexShape.getAlignment();

        if (indexShape.isVarying()) return VectorShape::varying();

        if (isa<StructType>(subT)) {
          if (!isa<ConstantInt>(index)) return VectorShape::varying();

          subT = cast<StructType>(subT)->getTypeAtIndex(index);

          if (result.isVarying()) {
            result = VectorShape::varying(1); // unaligned FIXME
          }
          else {
            result = VectorShape::strided(result.getStride() + indexStride, 1);
          }
        }
        else {
          subT = cast<SequentialType>(subT)->getPointerElementType();

          unsigned typeSize = (unsigned) layout.getTypeStoreSize(subT);

          if (result.isVarying()) {
            result = VectorShape::varying(result.getAlignment());
          }
          else {
            result = VectorShape::strided(result.getStride() + typeSize * indexStride,
                                          result.getAlignment());
          }
        }
      }

      return result;
    }

    case Instruction::Call:
    {
      const Function* callee = cast<CallInst>(I)->getCalledFunction();
      assert (!callee->getReturnType()->isVoidTy());

      // Find the shape that is mapped to this function
      // No mapping -> assume most unprecise, varying
      auto found = mFuncinfo.find(callee);
      if (found == mFuncinfo.end()) return VectorShape::varying();

      const VectorMapping* mapping = found->second;
      const VectorShapeVec Arginfo = mapping->argShapes;

      unsigned int i = 0;
      for (auto& op : callee->operands()) {
        const VectorShape& expected = Arginfo[i++];
        const VectorShape& actual = getShape(op);

        // Anything goes
        if (expected.isVarying()) continue;

        // Too unprecise
        if (actual.isVarying()) return VectorShape::varying();

        // The strides must be the same
        // and alignment has to be strict enough
        if (expected.getStride() != actual.getStride() ||
            actual.getAlignment() % expected.getAlignment() == 0)
        {
          return VectorShape::varying();
        }
      }

      return mapping->resultShape;
    }

    case Instruction::PHI:
    {
      // check if this PHINode actually joins different values
      bool mixingPhi = false;
      auto * phi = cast<PHINode>(I);
      Value * singleVal = phi->getIncomingValue(0);
      for (int i = 1; i < phi->getNumIncomingValues(); ++i) {
        if (singleVal != phi->getIncomingValue(i)) {
          mixingPhi = true;
          break;
        }
      }

      // the PHI node is not actually varying iff all input operands are the same
      // If the block is divergent the phi is varying
      if (mixingPhi && getShape(I->getParent()).isVarying()) return VectorShape::varying();
      return joinOperands(I);
    }

    case Instruction::Load:
    {
      const Value* pointer = I->getOperand(0);
      return VectorShape::join(VectorShape::uni(), getShape(pointer));
    }

    case Instruction::Select:
    {
      const Value* condition = I->getOperand(0);
      const Value* selection1 = I->getOperand(1);
      const Value* selection2 = I->getOperand(2);

      const VectorShape& condShape = getShape(condition);
      const VectorShape& sel1Shape = getShape(selection1);
      const VectorShape& sel2Shape = getShape(selection2);

      if (!condShape.isUniform()) return VectorShape::varying();

      return VectorShape::join(sel1Shape, sel2Shape);
    }

      // Join all operands
    default:
      return joinOperands(I);
  }
}

VectorShape PDA::computeShapeForBinaryInst(const BinaryOperator* I) {
  const Value* op1 = I->getOperand(0);
  const Value* op2 = I->getOperand(1);

  const VectorShape& shape1 = getShape(op1);
  const VectorShape& shape2 = getShape(op2);

  const int stride1 = shape1.getStride();
  const int stride2 = shape2.getStride();

  const unsigned alignment1 = shape1.getAlignment();
  const unsigned alignment2 = shape2.getAlignment();

  switch (I->getOpcode()) {
    // Addition can be optimized in case of strided shape (adding strides)
    case Instruction::Add:
    case Instruction::FAdd:
    {
      const bool fadd = I->getOpcode() == Instruction::FAdd;

      const unsigned resAlignment = gcd(alignment1, alignment2);

      if (shape1.isVarying() || shape2.isVarying()) return VectorShape::varying(resAlignment);

      VectorShape res = VectorShape::strided(stride1 + stride2, resAlignment);

      // Only allow strided results for floating point addition if
      // according fast math flags are set
      if (fadd) {
        FastMathFlags flags = I->getFastMathFlags();
        if (!flags.unsafeAlgebra()) {
          res = res.isUniform() ? res : VectorShape::varying();
        }
      }

      return res;
    }

      // Substraction can be optimized in case of strided shape (substracting strides)
    case Instruction::Sub:
    case Instruction::FSub:
    {
      const bool fsub = I->getOpcode() == Instruction::FSub;

      const unsigned resAlignment = gcd(alignment1, alignment2);

      if (shape1.isVarying() || shape2.isVarying()) return VectorShape::varying(resAlignment);

      VectorShape res = VectorShape::strided(stride1 - stride2, resAlignment);

      // Only allow strided results for floating point substraction if
      // according fast math flags are set
      if (fsub) {
        FastMathFlags flags = I->getFastMathFlags();
        if (!flags.unsafeAlgebra()) {
          res = res.isUniform() ? res : VectorShape::varying();
        }
      }

      return res;
    }

    // Integer multiplication with a constant simply multiplies the shape offset
    // if existent with the constant value
    // Alignment constants are multiplied
    case Instruction::Mul:
    {
      if (shape1.isVarying() || shape2.isVarying()) return VectorShape::varying();
      if (shape1.isUniform() && shape2.isUniform()) return VectorShape::uni(alignment1 * alignment2);

      // If the constant is known, compute the new shape directly
      if (const ConstantInt* constantOp = dyn_cast<ConstantInt>(op1)) {
        const int c = (int) constantOp->getSExtValue();
        return VectorShape::strided(c * stride2, c * alignment2);
      }

      // Symmetric case
      if (const ConstantInt* constantOp = dyn_cast<ConstantInt>(op2)) {
        const int c = (int) constantOp->getSExtValue();
        return VectorShape::strided(c * stride1, c * alignment1);
      }

      return VectorShape::varying();
    }

    case Instruction::SDiv:
    case Instruction::UDiv:
    {
      if (shape1.isUniform() && shape2.isUniform()) return VectorShape::uni(alignment1 / alignment2);

      if (const ConstantInt* constantOp = dyn_cast<ConstantInt>(op2)) {
        const int c = (int) constantOp->getSExtValue();
        if (stride1 % c == 0) return VectorShape::strided(stride1 / c, alignment1 / c);
      }

      return VectorShape::varying();
    }

    default:
      return shape1.isUniform() && shape2.isUniform() ? VectorShape::uni() : VectorShape::varying();
  }
}

static unsigned GetReferencedObjectSize(const DataLayout& layout, Type* ptrType) {
  auto* elemTy = ptrType->getPointerElementType();
  auto* arrTy = dyn_cast<ArrayType>(elemTy);
  if (arrTy && arrTy->getArrayNumElements() == 0) {
    elemTy = arrTy->getElementType();
  }
  return static_cast<unsigned>(layout.getTypeStoreSize(elemTy));
}

VectorShape PDA::computeShapeForCastInst(const CastInst* castI) {
  const Value* castOp = castI->getOperand(0);
  const VectorShape& castOpShape = getShape(castOp);
  const int castOpStride = castOpShape.getStride();
  const int castOpAlignment = castOpShape.getAlignment();
  const DataLayout layout(castI->getModule());

  const int aligned = !rv::returnsVoidPtr(*castI) ? castOpShape.getAlignment() : 1;

  switch (castI->getOpcode()) {
    case Instruction::IntToPtr:
    {
      PointerType* DestType = cast<PointerType>(castI->getDestTy());
      Type* DestPointsTo = DestType->getPointerElementType();

      // FIXME: void pointers are char pointers (i8*), but what
      // difference is there between a real i8* and a void pointer?
      if (DestPointsTo->isIntegerTy(8)) return VectorShape::varying();

      unsigned typeSize = (unsigned) layout.getTypeStoreSize(DestPointsTo);

      if (castOpStride % typeSize != 0) return VectorShape::varying();

      return VectorShape::strided(castOpStride / typeSize, 1);
    }

    case Instruction::PtrToInt:
    {
      PointerType* SrcType = cast<PointerType>(castI->getSrcTy());
      Type* SrcPointsTo = SrcType->getPointerElementType();

      unsigned typeSize = (unsigned) layout.getTypeStoreSize(SrcPointsTo);

      return VectorShape::strided(typeSize * castOpStride, 1);
    }

      // Truncation reinterprets the stride modulo the target type width
      // i16 to i1: stride(odd) -> consecutive, stride(even) -> uniform
    case Instruction::Trunc:
    {
      Type* destTy = castI->getDestTy();

      return VectorShape::truncateToTypeSize(castOpShape,
                                             (unsigned) layout.getTypeStoreSize(destTy));
    }

    // FIXME: is this correct?
    case Instruction::ZExt:
    case Instruction::SExt:
    case Instruction::FPExt:
      // NOTE: This consciously ignores large absolute values above 2²⁴
    case Instruction::UIToFP:
    case Instruction::SIToFP:
    {
      return castOpShape;
    }

    // Rounds towards zero: <-1.5f, -0.5f, 0.5f, 1.5f> -> <-1, 0, 0, 1>
    // "If the cast produces an inexact result, how rounding is performed [...]
    // is undefined."
    case Instruction::FPTrunc:
    case Instruction::FPToSI:
    case Instruction::FPToUI:
    {
      return VectorShape::join(VectorShape::uni(aligned), castOpShape);
    }

    case Instruction::BitCast:
    {
      Type* srcType = castI->getSrcTy();
      Type* destType = castI->getDestTy();

      // Cases like bitcasting floats to i32 to make the mantissa
      // available cannot retain stridedness
      if (!srcType->isPointerTy() || !destType->isPointerTy())
        return VectorShape::join(VectorShape::uni(aligned), castOpShape);

      // Reassociate stride with new underlying type
      PointerType* srcPtr = cast<PointerType>(srcType);
      PointerType* destPtr = cast<PointerType>(destType);

      int srcElementSize = static_cast<int>(GetReferencedObjectSize(layout, srcPtr));
      int destElementSize = static_cast<int>(GetReferencedObjectSize(layout, destPtr));

      return VectorShape::strided(srcElementSize * castOpStride / destElementSize, 1);
    }

    default:
      return VectorShape::join(VectorShape::uni(aligned), castOpShape);
  }
}

const VectorShape& PDA::getShape(const Value* const V) {
  auto found = mValue2Shape.find(V), end = mValue2Shape.end();
  if (found != end) return found->second;

  assert (isa<Constant>(V) && "Value is not available");
  return mValue2Shape[V] = VectorShape::uni(getAlignment(cast<Constant>(V)));
}

void PDA::markDivergentLoopLatchesMandatory() {
  for (const Loop* loop : mLoopInfo) {
    markLoopLatchesRecursively(loop);
  }
}

void PDA::markLoopLatchesRecursively(const Loop* loop) {
  // MANDATORY case 3: latches of divergent loops are mandatory
  if (mVecinfo.isDivergentLoop(loop)) {
    mVecinfo.markMandatory(loop->getLoopLatch());
  }

  for (const Loop* sLoop : loop->getSubLoops()) {
    markLoopLatchesRecursively(sLoop);
  }
}

typename ValueMap::iterator PDA::begin() { return mValue2Shape.begin(); }
typename ValueMap::iterator PDA::end() { return mValue2Shape.end(); }
typename ValueMap::const_iterator PDA::begin() const { return mValue2Shape.begin(); }
typename ValueMap::const_iterator PDA::end() const { return mValue2Shape.end(); }

}
