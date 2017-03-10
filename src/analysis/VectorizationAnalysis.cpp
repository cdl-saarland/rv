//===- VectorizationAnalysis.cpp -----------------------------===//
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

#include "rv/analysis/VectorizationAnalysis.h"

#include "rvConfig.h"
#include "utils/rvTools.h"
#include "rv/utils/mathUtils.h"

#include <llvm/Analysis/PostDominators.h>
#include <llvm/IR/Dominators.h>
#include <llvm/Analysis/LoopInfo.h>

#if 1
#define IF_DEBUG_VA IF_DEBUG
#else
#define IF_DEBUG_VA if (false)
#endif

//
//
// generic transfer functions
rv::VectorShape
GenericTransfer(rv::VectorShape a) {
  return a;
}

template<class ... Shapes>
rv::VectorShape
GenericTransfer(rv::VectorShape a, Shapes... nextShapes) {
  if (!a.isUniform()) return rv::VectorShape::varying();
  else return GenericTransfer(nextShapes...);
}





namespace rv {

using ValueMap = std::map<const Value*, VectorShape>;

// #define BYTE_SIZE 8

char VAWrapperPass::ID = 0;

void
VAWrapperPass::getAnalysisUsage(AnalysisUsage& Info) const {
  Info.addRequired<DFGBaseWrapper<true>>();
  Info.addRequired<DFGBaseWrapper<false>>();
  Info.addRequired<LoopInfoWrapperPass>();
  Info.addRequired<VectorizationInfoProxyPass>();
  Info.addRequired<DominatorTreeWrapperPass>();
  Info.addRequired<PostDominatorTree>();

  Info.setPreservesAll();
}

bool
VAWrapperPass::runOnFunction(Function& F) {
  auto& Vecinfo = getAnalysis<VectorizationInfoProxyPass>().getInfo();
  auto& platInfo = getAnalysis<VectorizationInfoProxyPass>().getPlatformInfo();

  const CDG& cdg = *getAnalysis<llvm::CDGWrapper>().getDFG();
  const DFG& dfg = *getAnalysis<llvm::DFGWrapper>().getDFG();
  const LoopInfo& LoopInfo = getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
  const auto & domTree = getAnalysis<DominatorTreeWrapperPass>().getDomTree();
  const auto & postDomTree = getAnalysis<PostDominatorTree>();

  VectorizationAnalysis vea(platInfo, Vecinfo, cdg, dfg, LoopInfo, domTree, postDomTree);
  vea.analyze(F);

  return false;
}

VectorizationAnalysis::VectorizationAnalysis(PlatformInfo & platInfo,
         VectorizationInfo& VecInfo,
         const CDG& cdg,
         const DFG& dfg,
         const LoopInfo& LoopInfo, const DominatorTree & domTree, const PostDominatorTree & postDomTree)

        : layout(""),
          mVecinfo(VecInfo),
          mCDG(cdg),
          mDFG(dfg),
          BDA(mVecinfo.getScalarFunction(), mCDG, mDFG, domTree, postDomTree, LoopInfo),
          mLoopInfo(LoopInfo),
          mFuncinfo(platInfo.getFunctionMappings()),
          mRegion(mVecinfo.getRegion())
{ }

void
VectorizationAnalysis::analyze(Function& F) {
  assert (!F.isDeclaration());

  // FIXME mWorklist.clear()
  while (!mWorklist.empty()) mWorklist.pop();

  init(F);
  compute(F);
  fillVectorizationInfo(F);

  // checkEquivalentToOldAnalysis(F);
}

bool VectorizationAnalysis::isInRegion(const BasicBlock& BB) {
  return mRegion ? mRegion->contains(&BB) : true;
}

bool VectorizationAnalysis::isInRegion(const Instruction& inst) {
  return !mRegion || isInRegion(*inst.getParent());
}

void VectorizationAnalysis::fillVectorizationInfo(Function& F) {
  for (const BasicBlock& BB : F) {
    if (!isInRegion(BB)) continue;
    mVecinfo.setVectorShape(BB, mValue2Shape[&BB]);
    for (const Instruction& I : BB) {
      VectorShape& shape = mValue2Shape[&I];

      mVecinfo.setVectorShape(I, shape.isDefined() ? shape : VectorShape::uni());
    }
  }
}

unsigned VectorizationAnalysis::getAlignment(const Constant* c) const {
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

void VectorizationAnalysis::init(Function& F) {
  layout = DataLayout(F.getParent());

  // Initialize with undefined values
  for (auto& arg : F.args()) mValue2Shape[&arg] = VectorShape::undef();
  for (auto& BB : F) for (auto& I : BB) mValue2Shape[&I] = VectorShape::undef();

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

  // Update initialized instructions
  for (auto& arg : F.args()) {
    if (mVecinfo.hasKnownShape(arg)) {
      VectorShape argShape = mVecinfo.getVectorShape(arg);

      if (arg.getType()->isPointerTy()) {
        uint minAlignment = getBaseAlignment(arg, layout);
        argShape.setAlignment(std::max<uint>(minAlignment, argShape.getAlignmentFirst()));
        // max is the more precise one
      }

      /* Adjust pointer arguments to continous stride */
      //if (arg.getType()->isPointerTy() && argShape.isVarying()) {
      //  argShape = VectorShape::cont();
      //}

      update(&arg, argShape);
    }
    else {
      assert(mRegion && "will only default function args if in region mode");
      // set argument shapes to uniform if not known better
      update(&arg, VectorShape::uni());
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
        if (call->getNumArgOperands() != 0) continue;

        mWorklist.push(&I);
        IF_DEBUG_VA errs() << "Inserted call in initialization: " << I.getName() << "\n";
      }
        /* Phis that depend on constants are added to the WL */
      else if (isa<PHINode>(I) && any_of(I.operands(), isa<Constant, Use>)) {
        mWorklist.push(&I);
        IF_DEBUG_VA errs() << "Inserted PHI in initialization: " << I.getName() << "\n";
      }
    }
  }
}

void VectorizationAnalysis::update(const Value* const V, VectorShape AT) {
  updateShape(V, AT);
  if (isa<BranchInst>(V))
    analyzeDivergence(cast<BranchInst>(V));
}

void VectorizationAnalysis::updateShape(const Value* const V, VectorShape AT) {
  const VectorShape& New = VectorShape::join(getShape(V), AT);

  if (mValue2Shape[V] == New) return;// nothing changed
  if (overrides.count(V) && getShape(V).isDefined()) return;//prevented by override

  IF_DEBUG_VA errs() << "Marking " << New << ": " << *V << "\n";
  mValue2Shape[V] = New;

  /* Add dependent elements to worklist */
  addRelevantUsersToWL(V);

  /* If an alloca was used it needs to be updated */
  if (!New.isUniform() && isa<Instruction>(V)) {
    updateAllocaOperands(cast<Instruction>(V));
  }
}

void VectorizationAnalysis::analyzeDivergence(const BranchInst* const branch) {
  // Vectorization is caused by non-uniform branches
  if (getShape(branch).isUniform()) return;
  assert (branch->isConditional()); // Unconditional branches would be uniform

  // Find out which regions diverge because of this non-uniform branch
  // The branch is regarded as varying, even if its condition is only strided

  const BasicBlock* endsVarying = branch->getParent();
  const Loop* endsVaryingLoop = mLoopInfo.getLoopFor(endsVarying);

  for (const auto* BB : BDA.getEffectedBlocks(*branch)) {
    if (!isInRegion(*BB)) {
      continue;
    } // filter out irrelevant nodes (FIXME filter out directly in BDA)

    // Doesn't matter if already effected previously
    if (mValue2Shape[BB].isVarying()) continue;

    IF_DEBUG errs() << "Branch " << *branch << " affects " << *BB << "\n";

    assert (isInRegion(*BB)); // Otherwise the region is ill-formed

    // Loop headers are not marked divergent, but can be loop divergent
    if (mLoopInfo.isLoopHeader(BB)) {
      const Loop* BBLoop = mLoopInfo.getLoopFor(BB);

      // BB needs to be a header of the same or an outer loop
      if (!BBLoop->contains(endsVaryingLoop)) continue;

      const bool BranchExitsBBLoop = BBLoop->isLoopExiting(endsVarying);
      if (BBLoop == endsVaryingLoop || BranchExitsBBLoop) {
        mVecinfo.setDivergentLoop(BBLoop);

        IF_DEBUG_VA {
          errs() << "\n"
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
      // In case of only one exit block we can optimize considering it gets linearized
      if (endsVaryingLoop->getUniqueExitBlock()) continue;

      // For multiple exits, the loop shall be blackboxed
      // If all exits are uniform, we regard as uniform
      if (allExitsUniform(endsVaryingLoop)) continue;
    }

    mValue2Shape[BB] = VectorShape::varying();

    IF_DEBUG_VA {
      errs() << "\n"
             << "The block:\n"
             << "    " << BB->getName() << "\n"
             << "is divergent because of the non-uniform branch in:\n"
             << "    " << endsVarying->getName() << "\n\n";
    }

    // add phis to worklist
    for (auto it = BB->begin(); BB->getFirstNonPHI() != &*it; ++it) {
      mWorklist.push(&*it);
      IF_DEBUG_VA errs() << "Inserted PHI: " << (&*it)->getName() << "\n";
    }
  }
}

static bool
IsVectorizableType(Type & type) {
  // plain arithmetic types
  if (type.isFloatingPointTy() || type.isIntegerTy()) return true;

  auto * structTy = dyn_cast<StructType>(&type);
  if (structTy) {
    for (auto * elemTy : structTy->elements()) {
      if (!IsVectorizableType(*elemTy)) return false;
    }
    return true;
  }

  return false;
}

void VectorizationAnalysis::updateAllocaOperands(const Instruction* I) {
  const int alignment = mVecinfo.getMapping().vectorWidth;

  for (const Value* op : I->operands()) {
    while (isa<GetElementPtrInst>(op))
      op = cast<Instruction>(op)->getOperand(0);

    if (!isa<AllocaInst>(op)) continue;

    // Already processed
    if (!getShape(op).isUniform()) continue;

    // promote to all users
    // eraseUserInfoRecursively(op);

    auto* PtrElemType = op->getType()->getPointerElementType();
    const bool Vectorizable = false; // FIXME IsVectorizableType(*PtrElemType);

    int typeStoreSize = static_cast<int>(layout.getTypeStoreSize(PtrElemType));

    update(op, Vectorizable ?
               VectorShape::strided(typeStoreSize, alignment) :
               VectorShape::varying());
  }
}

void VectorizationAnalysis::eraseUserInfoRecursively(const Value* V) {
  if (!mValue2Shape[V].isDefined()) return;

  mValue2Shape[V] = VectorShape::undef();

  for (const Value* use : V->users()) {
    eraseUserInfoRecursively(use);
  }
}

void VectorizationAnalysis::addRelevantUsersToWL(const Value* V) {
  for (const auto user : V->users()) {
    if (!isa<Instruction>(user)) continue;
    const Instruction* inst = cast<Instruction>(user);

    // We are only analyzing the region
    if (!isInRegion(*inst)) continue;

    // Ignore calls without return value
    if (const CallInst* callI = dyn_cast<CallInst>(inst)) {
      if (callI->getCalledFunction()->getReturnType()->isVoidTy()) {
        continue;
      }
    }

    auto isUndef = [&](Value* v) {
      return !isa<BasicBlock>(v) && !isa<Function>(v) && !getShape(v).isDefined();
    };

    if (!isa<PHINode>(inst) && any_of(inst->operands(), isUndef)) continue;

    mWorklist.push(inst);
    IF_DEBUG_VA errs() << "Inserted relevant user of " << V->getName() << ":" << *user << "\n";
  }
}

void VectorizationAnalysis::updateOutsideLoopUsesVarying(const Loop* divLoop) {
  SmallVector<BasicBlock*, 3> exitBlocks;
  divLoop->getExitBlocks(exitBlocks);
  for (auto* exitBlock : exitBlocks) {
    if (!isInRegion(*exitBlock)) continue;

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

bool VectorizationAnalysis::allExitsUniform(const Loop* loop) {
  SmallVector<BasicBlock*, 4> exitingBlocks;
  loop->getExitingBlocks(exitingBlocks);

  for (const BasicBlock* exitingBB : exitingBlocks) {
    const TerminatorInst* terminator = exitingBB->getTerminator();
    if (!getShape(terminator).isUniform()) return false;
  }

  return true;
}

VectorShape VectorizationAnalysis::joinOperands(const Instruction* const I) {
  VectorShape Join = VectorShape::undef();
  for (auto& op : I->operands()) Join = VectorShape::join(Join, getShape(op));
  return Join;
}

bool VectorizationAnalysis::allOperandsHaveShape(const Instruction* I) {
  auto hasKnownShape = [this](Value* op)
  {
    if (isa<Instruction>(op) && !mValue2Shape[op].isDefined()) {
      IF_DEBUG_VA { errs() << "\tmissing op shape " << *op << "!\n"; }
      mWorklist.push(cast<Instruction>(op));
    }

    return !isa<Instruction>(op) || mValue2Shape[op].isDefined();
  };

  return all_of(I->operands(), hasKnownShape);
}

void VectorizationAnalysis::compute(Function& F) {
  IF_DEBUG_VA { errs() << "\n\n-- VA::compute() log -- \n"; }
  /* Worklist algorithm to compute the least fixed-point */
  while (!mWorklist.empty()) {
    const Instruction* I = mWorklist.front();
    mWorklist.pop();

    IF_DEBUG_VA { errs() << "# next: " << *I << "\n"; }

    VectorShape New = computeShapeForInst(I);

    // adjust result type to match alignment
    if (I->getType()->isPointerTy()) {
      uint minAlignment = getBaseAlignment(*I, layout);
      New.setAlignment(std::max<uint>(minAlignment, New.getAlignmentFirst()));
    }

    update(I, New);
  }
}

VectorShape VectorizationAnalysis::computeShapeForInst(const Instruction* I) {
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

      const unsigned vectorWidth = mVecinfo.getMapping().vectorWidth;

      const unsigned alignment1 = shape1.getAlignmentFirst();
      const unsigned alignment2 = shape2.getAlignmentFirst();

      const int mingap = int(gcd(alignment1, alignment2));

      int strideDiff;
      if (predicate == Pred_t::ICMP_SLT || predicate == Pred_t::ICMP_ULT) {
        strideDiff = shape1.getStride() - shape2.getStride();
      }
      else if (predicate == Pred_t::ICMP_SGT || predicate == Pred_t::ICMP_UGT) {
        strideDiff = shape2.getStride() - shape1.getStride();
      }
      else {
        return VectorShape::varying();
      }

      int maxincrease = strideDiff * int(vectorWidth);
      // If the gap cannot get zero along the stride we stay less
      if (maxincrease >= 0 && mingap - maxincrease >= 0)
        return VectorShape::uni();

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
        const unsigned indexalignmentFirst = indexShape.getAlignmentFirst();
        const unsigned indexalignmentGeneral = indexShape.getAlignmentGeneral();

        if (indexShape.isVarying()) return VectorShape::varying();

        if (isa<StructType>(subT)) {
          if (!isa<ConstantInt>(index)) return VectorShape::varying();

          auto structlayout = layout.getStructLayout(cast<StructType>(subT));
          unsigned indexconstant = static_cast<uint>(cast<ConstantInt>(index)->getSExtValue());
          unsigned elemoffset = static_cast<uint>(structlayout->getElementOffset(indexconstant));

          subT = cast<StructType>(subT)->getTypeAtIndex(index);

          // Behaves like addition, pointer + offset and offset is uniform, hence the stride stays
          unsigned newalignment = gcd(result.getAlignmentFirst(), elemoffset);
          result.setAlignment(newalignment);
        }
        else {
          subT = cast<SequentialType>(subT)->getPointerElementType();

          unsigned typeSize = (unsigned) layout.getTypeStoreSize(subT);

          int offsetstride = typeSize * indexStride;
          unsigned offsetAlignmentFirst = typeSize * indexalignmentFirst;
          unsigned offsetAlignmentGeneral = typeSize * indexalignmentGeneral;

          if (result.isVarying()) {
            result = VectorShape::varying(gcd(result.getAlignmentGeneral(), offsetAlignmentGeneral));
          } else {
            result = VectorShape::strided(result.getStride() + offsetstride,
                                          gcd(result.getAlignmentFirst(), offsetAlignmentFirst));
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
      if (found == mFuncinfo.end()) {
        // TODO check if this function has side effects
        break;
      }

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
                actual.getAlignmentFirst() % expected.getAlignmentFirst() == 0)
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
      for (uint i = 1; i < phi->getNumIncomingValues(); ++i) {
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

      // use the generic transfer
    default:
      break;
  }

  return computeGenericArithmeticTransfer(*I);
}


VectorShape
VectorizationAnalysis::computeGenericArithmeticTransfer(const Instruction & I) {
  assert(I.getNumOperands() > 0 && "can not compute arithmetic transfer for instructions w/o operands");
  // generic transfer function
  for (uint i = 0; i < I.getNumOperands(); ++i) {
    if (!getShape(I.getOperand(i)).isUniform()) return VectorShape::varying();
  }
  return VectorShape::uni();
}

VectorShape VectorizationAnalysis::computeShapeForBinaryInst(const BinaryOperator* I) {
  const Value* op1 = I->getOperand(0);
  const Value* op2 = I->getOperand(1);

  const VectorShape& shape1 = getShape(op1);
  const VectorShape& shape2 = getShape(op2);

  const int stride1 = shape1.getStride();
  const int stride2 = shape2.getStride();

  const unsigned alignment1 = shape1.getAlignmentFirst();
  const unsigned alignment2 = shape2.getAlignmentFirst();

  const unsigned generalalignment1 = shape1.getAlignmentGeneral();
  const unsigned generalalignment2 = shape2.getAlignmentGeneral();

  switch (I->getOpcode()) {
    // Addition can be optimized in case of strided shape (adding strides)
    case Instruction::Add:
    case Instruction::FAdd:
    {
      const bool fadd = I->getOpcode() == Instruction::FAdd;

      const unsigned resAlignment = gcd(alignment1, alignment2);

      if (shape1.isVarying() || shape2.isVarying())
        return VectorShape::varying(gcd(generalalignment1, generalalignment2));

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

      if (shape1.isVarying() || shape2.isVarying())
        return VectorShape::varying(gcd(generalalignment1, generalalignment2));

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
      if (shape1.isVarying() || shape2.isVarying())
        return VectorShape::varying(generalalignment1 * generalalignment2);

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

      return VectorShape::varying(generalalignment1 * generalalignment2);
    }

    case Instruction::Or: {
    // try to match and Add that as been lowered to an Or
      int constOpId = 0;
      if (isa<Constant>(I->getOperand(0))) {
        constOpId = 0;
      } else if (isa<Constant>(I->getOperand(1))) {
        constOpId = 1;
      } else {
        break;
      }

      uint orConst = cast<ConstantInt>(I->getOperand(constOpId))->getZExtValue();
      int otherOpId = 1 - constOpId;
      VectorShape otherShape = getShape(I->getOperand(otherOpId));

      if (!otherShape.hasStridedShape()) {
        break;
      }

      // the or-ed constant is smaller than the alignment
      // in this case we can interpret as an add
      if (otherShape.getAlignmentFirst() > orConst) {
        auto resAlignment = gcd<uint>(orConst, otherShape.getAlignmentFirst());
        return VectorShape::strided(otherShape.getStride(), resAlignment);
      } else {
        break;
      }
    }

    case Instruction::Shl: {
      // interpret shift by constant as multiplication
      if (isa<Constant>(I->getOperand(1))) {
          auto * shiftConst = cast<ConstantInt>(I->getOperand(1));
          int shiftAmount = (int) shiftConst->getSExtValue();
          const auto valShape = getShape(I->getOperand(0));
          if (shiftAmount > 0 && valShape.hasStridedShape()) {
            int factor = 1 << shiftAmount;
            return VectorShape::strided(valShape.getStride() * factor,
                                        valShape.getAlignmentFirst() * factor);
          } else {
            break;
          }
      }
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
      break;
  }
  return GenericTransfer(shape1, shape2);
}

static unsigned GetReferencedObjectSize(const DataLayout& layout, Type* ptrType) {
  auto* elemTy = ptrType->getPointerElementType();
  auto* arrTy = dyn_cast<ArrayType>(elemTy);
  if (arrTy && arrTy->getArrayNumElements() == 0) {
    elemTy = arrTy->getElementType();
  }
  return static_cast<unsigned>(layout.getTypeStoreSize(elemTy));
}

VectorShape VectorizationAnalysis::computeShapeForCastInst(const CastInst* castI) {
  const Value* castOp = castI->getOperand(0);
  const VectorShape& castOpShape = getShape(castOp);
  const int castOpStride = castOpShape.getStride();
  const DataLayout layout(castI->getModule());

  const int aligned = !rv::returnsVoidPtr(*castI) ? castOpShape.getAlignmentFirst() : 1;

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

VectorShape VectorizationAnalysis::getShape(const Value* const V) {
  auto found = mValue2Shape.find(V), end = mValue2Shape.end();
  if (found != end) return found->second;

  if (isa<GlobalValue>(V)) return VectorShape::uni(0);

  assert (isa<Constant>(V) && "Value is not available");
  return mValue2Shape[V] = VectorShape::uni(getAlignment(cast<Constant>(V)));
}

typename ValueMap::iterator VectorizationAnalysis::begin() { return mValue2Shape.begin(); }
typename ValueMap::iterator VectorizationAnalysis::end() { return mValue2Shape.end(); }
typename ValueMap::const_iterator VectorizationAnalysis::begin() const { return mValue2Shape.begin(); }
typename ValueMap::const_iterator VectorizationAnalysis::end() const { return mValue2Shape.end(); }


FunctionPass*
createVectorizationAnalysisPass() {
  return new VAWrapperPass();
}


}
