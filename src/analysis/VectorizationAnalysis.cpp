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

#include <llvm/Analysis/PostDominators.h>
#include <llvm/IR/Dominators.h>
#include <llvm/Analysis/LoopInfo.h>

#include <numeric>
#include <algorithm>

#include "utils/mathUtils.h"

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
  return a.isUniform() ? rv::VectorShape::uni() : rv::VectorShape::varying();
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
  Info.addRequired<PostDominatorTreeWrapperPass>();

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
  const auto & postDomTree = getAnalysis<PostDominatorTreeWrapperPass>().getPostDomTree();

  VectorizationAnalysis vea(platInfo, Vecinfo, cdg, dfg, LoopInfo, domTree, postDomTree);
  vea.analyze(F);

  return false;
}

VectorizationAnalysis::VectorizationAnalysis(PlatformInfo& platInfo,
                                             VectorizationInfo& VecInfo,
                                             const CDG& cdg,
                                             const DFG& dfg,
                                             const LoopInfo& LoopInfo,
                                             const DominatorTree& domTree,
                                             const PostDominatorTree& postDomTree)

        : layout(platInfo.getDataLayout()),
          mVecinfo(VecInfo),
          mCDG(cdg),
          mDFG(dfg),
          BDA(mVecinfo.getScalarFunction(), mCDG, mDFG, LoopInfo),
          mLoopInfo(LoopInfo),
          mFuncinfo(platInfo.getFunctionMappings()),
          mRegion(mVecinfo.getRegion())
{ }

void
VectorizationAnalysis::analyze(Function& F) {
  assert (!F.isDeclaration());
  assert (mWorklist.empty());

  init(F);
  compute(F);
  fixUndefinedShapes(F);
}

bool VectorizationAnalysis::isInRegion(const BasicBlock& BB) {
  return !mRegion || mRegion->contains(&BB);
}

bool VectorizationAnalysis::isInRegion(const Instruction& inst) {
  return isInRegion(*inst.getParent());
}

void VectorizationAnalysis::fixUndefinedShapes(Function& F) {
  for (const BasicBlock& BB : F) {
    if (!isInRegion(BB)) continue;
    for (const Instruction& I : BB) {
      if (!getShape(&I).isDefined())
        mVecinfo.setVectorShape(I, VectorShape::uni());
    }
  }
}

void VectorizationAnalysis::adjustValueShapes(Function& F) {
  // Enforce shapes to be existing, if absent, set to VectorShape::undef()
  // If already there, also optimize alignment in case of pointer type

  // Arguments
  for (auto& arg : F.args()) {
    if (!mVecinfo.hasKnownShape(arg)) {
      mVecinfo.setVectorShape(arg, VectorShape::undef());
    } else {
      // Adjust pointer argument alignment
      if (arg.getType()->isPointerTy()) {
        VectorShape argShape = mVecinfo.getVectorShape(arg);
        uint minAlignment = getBaseAlignment(arg, layout);
        // max is the more precise one
        argShape.setAlignment(std::max<uint>(minAlignment, argShape.getAlignmentFirst()));
        mVecinfo.setVectorShape(arg, argShape);
      }
    }
  }

  // Instructions in region(!)
  for (auto& BB : F) {
    if (mVecinfo.inRegion(BB)) {
      for (auto& I : BB) {
        if (!mVecinfo.hasKnownShape(I))
          mVecinfo.setVectorShape(I, VectorShape::undef());
      }
    }
  }
}

void VectorizationAnalysis::init(Function& F) {
  adjustValueShapes(F);

  // bootstrap with user defined shapes
  for (auto& BB : F) {
    for (auto& I : BB) {
      VectorShape shape = mVecinfo.getVectorShape(I);

      if (shape.isDefined()) {
        IF_DEBUG_VA errs() << "Override for: " << I << ", shape: " << shape << "\n";
        overrides.insert(&I);
        // Drop + update so this gets recognized as a change
        mVecinfo.setVectorShape(I, VectorShape::undef());
        update(&I, shape);
      }
    }
  }

  // Start iteration from arguments
  for (auto& arg : F.args()) {
    if (!mVecinfo.getVectorShape(arg).isDefined()) {
      assert(mRegion && "will only default function args if in region mode");
      // set argument shapes to uniform if not known better
      mVecinfo.setVectorShape(arg, VectorShape::uni());
    }

    addDependentValuesToWL(&arg);
  }

  // Propagation of vectorshapes starts at:
  // - Allocas
  // - Constants
  // - Calls (no connection to them if they have no parameters)
  for (const BasicBlock& BB : F) {
    mVecinfo.setVectorShape(BB, VectorShape::uni());

    for (const Instruction& I : BB) {
      if (isa<AllocaInst>(&I)) {
        update(&I, VectorShape::uni(mVecinfo.getMapping().vectorWidth));
      } else if (const CallInst* call = dyn_cast<CallInst>(&I)) {
        // Initialize WL with 0 parameter calls
        // Only makes sense if a value is returned
        if (call->getCalledFunction()->getReturnType()->isVoidTy()) continue;
        if (call->getNumArgOperands() != 0) continue;

        mWorklist.push(&I);
        IF_DEBUG_VA errs() << "Inserted call in initialization: " << I.getName() << "\n";
      } else if (isa<PHINode>(I) && any_of(I.operands(), isa<Constant, Use>)) {
        // Phis that depend on constants are added to the WL
        mWorklist.push(&I);
        IF_DEBUG_VA errs() << "Inserted PHI in initialization: " << I.getName() << "\n";
      }
    }
  }
}

void VectorizationAnalysis::update(const Value* const V, VectorShape AT) {
  bool changed = updateShape(V, AT);
  if (changed && isa<BranchInst>(V))
    analyzeDivergence(cast<BranchInst>(V));
}

bool VectorizationAnalysis::updateShape(const Value* const V, VectorShape AT) {
  VectorShape Old = getShape(V);
  VectorShape New = VectorShape::join(Old, AT);

  if (Old == New) return false;// nothing changed
  if (overrides.count(V) && Old.isDefined()) return false;//prevented by override

  IF_DEBUG_VA errs() << "Marking " << New << ": " << *V << "\n";
  mVecinfo.setVectorShape(*V, New);

  // Add dependent elements to worklist
  addDependentValuesToWL(V);

  return true;
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

    // Doesn't matter if already affected previously
    if (getShape(BB).isVarying()) continue;

    IF_DEBUG_VA errs() << "Branch <" << *branch << "> affects " << BB->getName() << ".\n";

    // Loop headers are not marked divergent, but can be loop divergent
    if (mLoopInfo.isLoopHeader(BB)) {
      const Loop* BBLoop = mLoopInfo.getLoopFor(BB);

      // Already divergent
      if (mVecinfo.isDivergentLoop(BBLoop)) continue;

      // Loop divergence is caused by varying loop exits
      if (!BBLoop->isLoopExiting(endsVarying)) continue;

      mVecinfo.setDivergentLoop(BBLoop);
      updateLCSSAPhisVarying(BBLoop);

      IF_DEBUG_VA {
        errs() << "\nThe loop with header: " << BB->getName() << " is divergent, "
               << "because of the non-uniform branch in: " << endsVarying->getName() << "\n";
      }

      continue;
    }

    // If the dependence exits the loop, we need to blackbox the loop
    // If any loop exit is varying, the block is divergent since the loop might
    // leak information before every thread is done
    if (endsVaryingLoop && !endsVaryingLoop->contains(BB)) {
      // If all exits are uniform, we regard as uniform
      if (allExitsUniform(endsVaryingLoop)) continue;
    }

    mVecinfo.setVectorShape(*BB, VectorShape::varying());

    IF_DEBUG_VA {
      errs() << "\n"
             << "The block:\n"
             << "    " << BB->getName() << "\n"
             << "is divergent because of the non-uniform branch in:\n"
             << "    " << endsVarying->getName() << "\n\n";
    }

    // add phis to worklist
    for (auto & inst : *BB) {
      if (!isa<PHINode>(inst)) break;

      mWorklist.push(&inst);
      IF_DEBUG_VA errs() << "Inserted PHI: " << inst.getName() << "\n";
    }
  }
}

void VectorizationAnalysis::addDependentValuesToWL(const Value* V) {
  // Push users of this value
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

    mWorklist.push(inst);
    IF_DEBUG_VA errs() << "Inserted user of updated " << V->getName() << ":" << *user << "\n";
  }

  const Instruction* I = dyn_cast<Instruction>(V);
  if (!I || getShape(I).isUniform()) return;

  // Push allocas used by this non-uniform value
  for (const Value* op : I->operands()) {
    // Skip GEPs
    while (auto* gep = dyn_cast<GetElementPtrInst>(op)) op = gep->getPointerOperand();

    if (!isa<AllocaInst>(op))      continue; // Only allocas
    if (!getShape(op).isUniform()) continue; // Already processed

    mWorklist.push(cast<Instruction>(op));
  }
}

void VectorizationAnalysis::updateLCSSAPhisVarying(const Loop* divLoop) {
  SmallVector<BasicBlock*, 3> exitBlocks;
  divLoop->getExitBlocks(exitBlocks);

  for (auto* exitBlock : exitBlocks) {
    if (!isInRegion(*exitBlock)) continue;

    for (auto& inst : *exitBlock) {
      if (!isa<PHINode>(inst)) break;
      update(&inst, VectorShape::varying());
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

VectorShape VectorizationAnalysis::joinOperands(const Instruction& I) {
  VectorShape Join = VectorShape::undef();
  for (const auto& op : I.operands()) Join = VectorShape::join(Join, getShape(op));
  return Join;
}

bool VectorizationAnalysis::pushMissingOperands(const Instruction* I) {
  auto pushIfMissing = [this](bool prevpushed, Value* op)
  {
    bool push = isa<Instruction>(op) && !getShape(op).isDefined();
    if (push) {
      IF_DEBUG_VA { errs() << "\tmissing op shape " << *op << "!\n"; }
      mWorklist.push(cast<Instruction>(op));
    }

    return prevpushed || push;
  };

  return std::accumulate(I->op_begin(), I->op_end(), false, pushIfMissing);
}

VectorShape VectorizationAnalysis::computePHIShape(const PHINode & phi) {
   // check if this PHINode actually joins different values
   const Value* first = phi.getIncomingValue(0);
   bool mixingPhi = std::any_of(phi.op_begin() + 1, phi.op_end(),
                                [&](const Value* op) { return op != first; });

   // the PHI node is not actually varying iff all input operands are the same
   // If the block is divergent the phi is varying
   if (mixingPhi && getShape(phi.getParent()).isVarying()) {
     // TODO infer greatest common alignment
     return VectorShape::varying();
   } else {
     return joinOperands(phi);
   }
}

void VectorizationAnalysis::compute(Function& F) {
  IF_DEBUG_VA { errs() << "\n\n-- VA::compute() log -- \n"; }
  /* Worklist algorithm to compute the least fixed-point */
  while (!mWorklist.empty()) {
    const Instruction* I = mWorklist.front();
    mWorklist.pop();

    IF_DEBUG_VA { errs() << "# next: " << *I << "\n"; }

    VectorShape New;
    // allow incomplete inputs for PHI nodes
    if (isa<PHINode>(I)) {
      New = computePHIShape(cast<PHINode>(*I));
    } else if (pushMissingOperands(I)) {
      continue;
    } else {
      New = computeShapeForInst(I);
    }

    if (I->getType()->isPointerTy()) {
      // adjust result type to match alignment
      uint minAlignment = getBaseAlignment(*I, layout);
      New.setAlignment(std::max<uint>(minAlignment, New.getAlignmentFirst()));
    } else if (I->getType()->isFloatingPointTy()) {
      // Only allow strided results for floating point instructions if
      // according fast math flags are set
      FastMathFlags flags = I->getFastMathFlags();
      if (!flags.unsafeAlgebra() && !New.isUniform()) {
        New = VectorShape::varying();
      }
    }

    update(I, New);
  }
}

VectorShape VectorizationAnalysis::computeShapeForInst(const Instruction* I) {
  if (I->isBinaryOp()) return computeShapeForBinaryInst(cast<BinaryOperator>(I));
  if (I->isCast()) return computeShapeForCastInst(cast<CastInst>(I));

  switch (I->getOpcode()) {
    case Instruction::Alloca:
    {
      const int alignment = mVecinfo.getMapping().vectorWidth;
      auto* AllocatedType = I->getType()->getPointerElementType();
      const bool Vectorizable = false;

      if (Vectorizable) {
        int typeStoreSize = (int)(layout.getTypeStoreSize(AllocatedType));
        return VectorShape::strided(typeStoreSize, alignment);
      }

      return VectorShape::varying();
    }
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

      // Get a shape for op1 - op2 and see if it compares uniform to a full zero-vector
      VectorShape diffShape = getShape(op1) - getShape(op2);
      if (diffShape.isVarying())
        return diffShape;

      CmpInst::Predicate predicate = cast<CmpInst>(I)->getPredicate();
      switch (predicate) {
        case CmpInst::Predicate::ICMP_SGT:
        case CmpInst::Predicate::ICMP_UGT:
        case CmpInst::Predicate::ICMP_SLE:
        case CmpInst::Predicate::ICMP_ULE:
          diffShape = -diffShape; // Negate and handle like LESS/GREATER_EQUAL
        case CmpInst::Predicate::ICMP_SLT:
        case CmpInst::Predicate::ICMP_ULT:
        case CmpInst::Predicate::ICMP_SGE:
        case CmpInst::Predicate::ICMP_UGE:
        // These coincide because a >= b is equivalent to !(a < b)
        {
          const int vectorWidth = (int)mVecinfo.getMapping().vectorWidth;
          const int stride = diffShape.getStride();
          const int alignment = diffShape.getAlignmentFirst();

          if (stride >= 0 && alignment >= stride * vectorWidth)
            return VectorShape::uni();

          break;
        }

        case CmpInst::Predicate::ICMP_EQ:
        case CmpInst::Predicate::ICMP_NE:
        {
          if (diffShape.getStride() == 0)
            return VectorShape::uni();

          break;
        }

        default:
          break;
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
        if (StructType* Struct = dyn_cast<StructType>(subT)) {
          if (!isa<ConstantInt>(index)) return VectorShape::varying();

          subT = Struct->getTypeAtIndex(index);

          auto structlayout = layout.getStructLayout(Struct);
          unsigned idxconst = (unsigned) cast<ConstantInt>(index)->getSExtValue();
          unsigned elemoffset = (unsigned) structlayout->getElementOffset(idxconst);

          // Behaves like addition, pointer + offset and offset is uniform, hence the stride stays
          unsigned newalignment = gcd(result.getAlignmentFirst(), elemoffset);
          result.setAlignment(newalignment);
        } else {
          // NOTE: If indexShape is varying, this still reasons about alignment
          subT = isa<PointerType>(subT) ? subT->getPointerElementType() : subT->getSequentialElementType();

          const int typeSizeInBytes = (int)layout.getTypeStoreSize(subT);
          result = result + typeSizeInBytes * getShape(index);
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

        // If the expected shape is more precise than the computed shape, return varying
        if (expected < actual)
          return VectorShape::varying();
      }

      return mapping->resultShape;
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

VectorShape VectorizationAnalysis::computeGenericArithmeticTransfer(const Instruction & I) {
  assert(I.getNumOperands() > 0 && "can not compute arithmetic transfer for instructions w/o operands");
  // generic transfer function
  for (uint i = 0; i < I.getNumOperands(); ++i) {
    if (!getShape(I.getOperand(i)).isUniform()) return VectorShape::varying();
  }
  return VectorShape::uni();
}

VectorShape VectorizationAnalysis::computeShapeForBinaryInst(const BinaryOperator* I) {
  Value* op1 = I->getOperand(0);
  Value* op2 = I->getOperand(1);

  // Assume constants are on the RHS
  if (!isa<Constant>(op2) && I->isCommutative()) std::swap(op1, op2);

  const VectorShape& shape1 = getShape(op1);
  const VectorShape& shape2 = getShape(op2);

  const int stride1 = shape1.getStride();

  const unsigned alignment1 = shape1.getAlignmentFirst();
  const unsigned alignment2 = shape2.getAlignmentFirst();

  const unsigned generalalignment1 = shape1.getAlignmentGeneral();
  const unsigned generalalignment2 = shape2.getAlignmentGeneral();

  switch (I->getOpcode()) {
    case Instruction::Add:
    case Instruction::FAdd:
      return shape1 + shape2;

    case Instruction::Sub:
    case Instruction::FSub:
      return shape1 - shape2;

    // Integer multiplication with a constant simply multiplies the shape offset
    // if existent with the constant value
    // Alignment constants are multiplied
    case Instruction::Mul:
    {
      if (shape1.isVarying() || shape2.isVarying())
        return VectorShape::varying(generalalignment1 * generalalignment2);

      if (shape1.isUniform() && shape2.isUniform())
        return VectorShape::uni(alignment1 * alignment2);

      // If the constant is known, compute the new shape directly
      if (const ConstantInt* constantOp = dyn_cast<ConstantInt>(op2)) {
        const int c = (int) constantOp->getSExtValue();
        return c * shape1;
      }

      return VectorShape::varying(generalalignment1 * generalalignment2);
    }

    case Instruction::Or: {
    // In some cases Or-with-constant can be interpreted as an Add-with-constant
      // this holds e.g. for: <4, 6, 8, 10> | 1 = <5, 7, 9, 11>
      if (!isa<ConstantInt>(op2)) break;

      uint orConst = cast<ConstantInt>(op2)->getZExtValue();
      VectorShape otherShape = getShape(op1);

      if (orConst == 0) {
        // no-op
        return otherShape;
      }

      uint laneAlignment = otherShape.getAlignmentGeneral();
      if (laneAlignment <= 1) break;

      // all bits above constTopBit are zero
      uint constTopBit = static_cast<uint>(highest_bit(orConst));

    // there is an overlap between the bits of the constant and a possible lane value
      if (constTopBit >= laneAlignment) {
        break;
      }

    // from this point we know that the Or behaves like an Add
      if (otherShape.hasStridedShape()) {
        // the Or operates like an Add with an uniform value
        auto resAlignment = gcd<uint>(orConst, otherShape.getAlignmentFirst());
        return VectorShape::strided(otherShape.getStride(), resAlignment);

      } else {
        return VectorShape::varying(gcd<uint>(laneAlignment, orConst));
      }

      break;
    }

    case Instruction::Shl: {
      // interpret shift by constant as multiplication
      if (auto* shiftCI = dyn_cast<ConstantInt>(op2)) {
        int shiftAmount = (int) shiftCI->getSExtValue();
        if (shiftAmount > 0) {
          int factor = 1 << shiftAmount;
          return factor * shape1;
        }
      }

      break;
    }

    case Instruction::SDiv:
    case Instruction::UDiv:
    {
      if (shape1.isUniform() && shape2.isUniform()) return VectorShape::uni(alignment1 / alignment2);

      if (const ConstantInt* constantOp = dyn_cast<ConstantInt>(op2)) {
        const int c = (int) constantOp->getSExtValue();
        if (stride1 % c == 0) return VectorShape::strided(stride1 / c, alignment1 / std::abs(c));
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

  const int aligned = !rv::returnsVoidPtr(*castI) ? castOpShape.getAlignmentFirst() : 1;

  if (castOpShape.isVarying()) return castOpShape;

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
      Type* SrcElemType = castI->getSrcTy()->getPointerElementType();

      unsigned typeSize = (unsigned) layout.getTypeStoreSize(SrcElemType);

      return VectorShape::strided(typeSize * castOpStride, aligned);
    }

      // Truncation reinterprets the stride modulo the target type width
      // i16 to i1: stride(odd) -> consecutive, stride(even) -> uniform
    case Instruction::Trunc:
    {
      Type* destTy = castI->getDestTy();

      return truncateToTypeSize(castOpShape, (unsigned) layout.getTypeStoreSize(destTy));
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
  if (const Constant* C = dyn_cast<Constant>(V))
    return VectorShape::fromConstant(C);

  if (mVecinfo.hasKnownShape(*V))
    return mVecinfo.getVectorShape(*V);

#if 0
  if (isa<GlobalValue>(V)) return VectorShape::uni(0);
  if (isa<BasicBlock>(V)) return VectorShape::uni(0);

  assert (isa<Constant>(V) && "Value is not available");
#endif
  return VectorShape::uni(0);
}

FunctionPass* createVectorizationAnalysisPass() {
  return new VAWrapperPass();
}


}
