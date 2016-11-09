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

#if 1
#define IF_DEBUG_PDA IF_DEBUG
#else
#define IF_DEBUG_PDA if (false)
#endif

namespace rv {

using ValueMap = std::map<const Value*, VectorShape>;
using FuncInfo = native::VectorMappingMap;

#define BYTE_SIZE 8

char PDAWrapperPass::ID = 0;

bool
PDAWrapperPass::runOnFunction(Function& F)
{
    VectorizationInfo& Vecinfo = getAnalysis<VectorizationInfoProxyPass>().getInfo();

    const CDG& cdg = *getAnalysis<llvm::CDGWrapper>().getDFG();
    const DFG& dfg = *getAnalysis<llvm::DFGWrapper>().getDFG();
    const FuncInfo& FuncInfo = getAnalysis<RVInfoProxyPass>().getInfo().getVectorFuncMap();
    const LoopInfo& LoopInfo = getAnalysis<LoopInfoWrapperPass>().getLoopInfo();

    PDA pda(Vecinfo,
            cdg,
            dfg,
            FuncInfo,
            LoopInfo);

    pda.analyze(F);

    return false;
}

/* void
PDAWrapperPass::print(llvm::raw_ostream& O, const Module*) const
{
    O << "  " << mValue2Shape.size() << " elements\n";
    for (auto it : mPDA)
        O << NAME(it.first) << "  --->  " << it.second << "\n";
} */

PDA::PDA(VectorizationInfo& VecInfo,
         const CDG& cdg,
         const DFG& dfg,
         const FuncInfo& Funcinfo,
         const LoopInfo& LoopInfo)

        : mCDG(cdg),
          mDFG(dfg),
          mVecinfo(VecInfo),
          mFuncinfo(Funcinfo),
          mLoopInfo(LoopInfo),
          mRegion(mVecinfo.getRegion())
{ }

void
PDA::analyze(Function& F)
{
    assert (!F.isDeclaration());

    mWorklist.clear();

    init(F);
    compute(F);
    fillVectorizationInfo(F);

    markDivergentLoopLatchesMandatory();
    // checkEquivalentToOldAnalysis(F);
}

void
PDA::setRegion(rv::Region* region)
{
    mRegion = region;
}

bool
PDA::isInRegion(const BasicBlock* BB)
{
    return mRegion ? mRegion->contains(BB) : true;
}


bool
PDA::isInRegion(const Instruction & inst) {
  return !mRegion || isInRegion(inst.getParent());
}

void
PDA::fillVectorizationInfo(Function& F)
{
    for (const BasicBlock& BB : F)
    {
        mVecinfo.setVectorShape(BB, mValue2Shape[&BB]);
        for (const Instruction& I : BB)
            if (mValue2Shape.count(&I))
                mVecinfo.setVectorShape(I, mValue2Shape[&I]);
        if (!mVecinfo.hasKnownShape(*BB.getTerminator()))
            mVecinfo.setVectorShape(*BB.getTerminator(), VectorShape::uni());
    }
}

unsigned
PDA::getAlignment(const Constant* c) const
{
    assert (c);
    assert (!isa<BasicBlock>(c));
    assert (!isa<Function>(c));

    // An undef value is never aligned.
    if (isa<UndefValue>(c)) return 1;

    if (const ConstantInt* cint = dyn_cast<ConstantInt>(c))
    {
        return std::abs(cint->getSExtValue());
    }

    // Other than that, only integer vector constants can be aligned.
    if (!c->getType()->isVectorTy()) return 1;

    // A zero-vector is aligned.
    if (isa<ConstantAggregateZero>(c)) return 0;

    if (const ConstantDataVector* cdv = dyn_cast<ConstantDataVector>(c))
    {
        if (!cdv->getElementType()->isIntegerTy()) return 1;

        const int intValue = (int) cast<ConstantInt>(cdv->getAggregateElement(0U))->getSExtValue();

        return std::abs(intValue);
    }

    assert (isa<ConstantVector>(c));
    const ConstantVector* cv = cast<ConstantVector>(c);

    if (!cv->getType()->getElementType()->isIntegerTy()) return 1;

    assert (isa<ConstantInt>(cv->getOperand(0)));
    const ConstantInt* celem = cast<ConstantInt>(cv->getOperand(0));
    const int intValue = (int) celem->getSExtValue();

    // The vector is aligned if its first element is aligned
    return std::abs(intValue);
}

void
PDA::init(Function& F)
{
    // Update initialized instructions
    for (auto& arg : F.args())
    {
        if (mVecinfo.hasKnownShape(arg)) {
            VectorShape argShape = mVecinfo.getVectorShape(arg);

            /* Adjust pointer arguments to continous stride */
            if (arg.getType()->isPointerTy() && argShape.isVarying())
                argShape = VectorShape::cont();

            update(&arg, argShape);
            mVecinfo.dropVectorShape(arg);
        } else {
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
            } else {
              mVecinfo.setVectorShape(I, VectorShape::uni());
            }
        }
    }

    // Propagation of vectorshapes starts at:
    // - Allocas
    // - Constants
    // - Calls (theres no connection to them in the iteration if they have no parameters)
    for (const BasicBlock& BB : F)
    {

        // Mark block all blocks as uniform
        // if (isInRegion(&BB)) {
            mValue2Shape[&BB] = VectorShape::uni();
        // } else {
        //   // skip over blocks outside of the region
        //   continue;
        // }

        for (const Instruction& I : BB)
        {
            if (isa<AllocaInst>(&I))
            {
                update(&I, VectorShape::uni(mVecinfo.getMapping().vectorWidth));
            }
            /* Need to initialize WL with calls, they may not be reached o.w. */
            else if (const CallInst* call = dyn_cast<CallInst>(&I))
            {
                // Only makes sense if a value is returned
                if (call->getCalledFunction()->getReturnType()->isVoidTy()) continue;

                mWorklist.insert(&I);
                IF_DEBUG_PDA errs() << "Inserted call in initialization: " << I.getName() << "\n";
            }
            /* Phis that depend on constants are added to the WL */
            else if (isa<PHINode>(I) && any_of(I.operands(), isa<Constant, Use>))
            {
                mWorklist.insert(&I);
                IF_DEBUG_PDA outs() << "Inserted PHI in initialization: " << I.getName() << "\n";
            }

#if 0
            /*
             * Every operand is constant, add to WL
             * If only part of the operands are constant, this will be reached
             * during iteration at some point
             */
            // FIXME: just run constant propagation beforehand?
            if (I.getNumOperands() > 0 && all_of(I.operands(), isa<Constant, Use>))
            {
                mWorklist.insert(&I);
                IF_DEBUG_PDA outs() << "Inserted all-constant instruction in initialization: " << I.getName() <<
                       "\n";
            }
#endif
        }
    }
}

void
PDA::update(const Value* const V, VectorShape AT)
{
    const VectorShape& New = joinWithOld(V, AT);

    if (mValue2Shape.count(V) && mValue2Shape[V] == New)
        return;// nothing changed

    IF_DEBUG_PDA {
      outs() << "marking " << V->getName().str() << " " << New << "\n";
    }

    if (overrides.count(V) && mValue2Shape.count(V)) {
      return;
    }
    mValue2Shape[V] = New;

    /* Add dependent elements to worklist */
    addRelevantUsersToWL(V);

    // Nothing else needs to be done for uniform values
    if (New.isUniform())
        return;

    /* If an alloca was used it needs to be updated */
    if (isa<Instruction>(V))
        updateAllocaOperands(cast<Instruction>(V));



    /* Begin with divergence analysis */

    // Divergence is caused directly by branches only
    if (!isa<BranchInst>(V))
        return;

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
    for (const CDNode* cd_succ : cd_node->succs())
    {
        const DFNode* df_node = mDFG[cd_succ->getBB()];
        for (const DFNode* df_pred : df_node->preds())
        {
            // Get the block BB that is affected by the varying branch
            const BasicBlock* const BB = df_pred->getBB();

            IF_DEBUG errs() << "Branch " << *branch << " affects " << *BB << "\n";

            assert(isInRegion(BB)); // Otherwise the region is ill-formed

            // Loop headers are not marked divergent, but can be loop divergent
            if (mLoopInfo.isLoopHeader(BB))
            {
                const Loop* BBLoop = mLoopInfo.getLoopFor(BB);
                if (!BBLoop->contains(endsVaryingLoop))
                    continue;

                const bool BranchExitsBBLoop = BBLoop->isLoopExiting(endsVarying);

                // If the branch exits the loop, the loop is divergent
                // (the header diverges in its loop)

                const Loop * divLevel = BranchExitsBBLoop ? BBLoop : endsVaryingLoop;
                IF_DEBUG errs() << "Setting divlevel for " << *BB << " for branch " << *branch << " to " << *divLevel << "\n";

                mVecinfo.setDivergenceLevel(*BB, divLevel);

                if (mVecinfo.isDivergentLoop(BBLoop))
                {
                    for (auto it = BB->begin(); BB->getFirstNonPHI() != &*it; ++it)
                        updateOutsideLoopUsesVarying(cast<PHINode>(&*it), BBLoop);
                }

                continue;
            }

            // If the dependence exits the loop, we need to blackbox the loop
            // If any loop exit is varying, the block is divergent since the loop might
            // leak information before every thread is done
            if (endsVaryingLoop && !endsVaryingLoop->contains(BB))
            {
                // In case of only one exit block we can optimize considering it gets
                // linearized
                if (endsVaryingLoop->getUniqueExitBlock())
                {
                    mVecinfo.setDivergenceLevel(*BB, endsVaryingLoop);
                    continue;
                }

                // For multiple exits, the loop shall be blackboxed
                VectorShape CombinedExitShape = combineExitShapes(endsVaryingLoop);
                mValue2Shape[BB] = joinWithOld(BB, CombinedExitShape);
            }
            else
                mValue2Shape[BB] = VectorShape::varying();

            mVecinfo.setDivergenceLevel(*BB, endsVaryingLoop);

            if (mValue2Shape[BB].isVarying())
            {
                IF_DEBUG_PDA {
                        outs() << "Block " + BB->getName() +
                                  " is divergent since the branch in " +
                                  endsVarying->getName() + " is varying.\n\n";
                }

                // MANDATORY case 2: divergent block
                mVecinfo.markMandatory(BB);
            }

            // add phis to worklist
            for (auto it = BB->begin(); BB->getFirstNonPHI() != &*it; ++it)
            {
                mWorklist.insert(&*it);
                IF_DEBUG_PDA outs() << "Inserted PHI: " << (&*it)->getName() << "\n";
            }
        }
    }
}

void
PDA::updateAllocaOperands(const Instruction* I)
{
    const int alignment = mVecinfo.getMapping().vectorWidth;

    for (const Value* op : I->operands())
    {
        if (!isa<AllocaInst>(op)) continue;

        // Already processed
        if (!getShape(op).isUniform()) continue;

        eraseUserInfoRecursively(op);

        const Type* PtrElemType = op->getType()->getPointerElementType();
        const bool Vectorizable = rv::isVectorizableNonDerivedType(*PtrElemType);

        update(op, Vectorizable ?
                   VectorShape::cont(alignment) :
                   VectorShape::varying(alignment));
    }
}

void
PDA::eraseUserInfoRecursively(const Value* V)
{
    auto found = mValue2Shape.find(V);

    if (found == mValue2Shape.end()) return;

    mValue2Shape.erase(found);

    for (const Value* use : V->users())
        eraseUserInfoRecursively(use);
}

void
PDA::addRelevantUsersToWL(const Value* V)
{
    for (auto user : V->users())
    {
        // We are only analyzing the region
        if (!isInRegion(*cast<Instruction>(user))) continue;

        // Ignore calls without return value
        if (const CallInst* callI = dyn_cast<CallInst>(user))
            if (callI->getCalledFunction()->getReturnType()->isVoidTy())
                continue;

        mWorklist.insert(cast<Instruction>(user));
        IF_DEBUG_PDA outs() << "Inserted " << user->getName() << " as relevant user of " << V->getName() <<
               "\n";
    }
}

void
PDA::updateOutsideLoopUsesVarying(const PHINode* PHI, const Loop* PHILoop)
{
    for (auto use : PHI->users())
    {
        const Instruction* useI = cast<Instruction>(use);
        if (!isInRegion(*useI)) continue;

        // find outside uses and update varying
        if (!PHILoop->contains(mLoopInfo.getLoopFor(useI->getParent())))
            update(useI, VectorShape::varying());
    }
}

VectorShape
PDA::joinWithOld(const Value* const V, VectorShape AT)
{
    auto found = mValue2Shape.find(V);

    // Has a value to join with
    if (found != mValue2Shape.end())
        return VectorShape::join(found->second, AT);

    // Uninitialized/Bottom value
    return AT;
}

VectorShape
PDA::combineExitShapes(const Loop* loop)
{
    SmallVector<BasicBlock*, 4> exitingBlocks;
    loop->getExitingBlocks(exitingBlocks);

    VectorShape CombinedExitShape = VectorShape::uni();
    for (BasicBlock* exitingBB : exitingBlocks)
    {
        const TerminatorInst* terminator = exitingBB->getTerminator();
        CombinedExitShape = joinWithOld(terminator, CombinedExitShape);
    }

    return CombinedExitShape;
}

void
PDA::markDependentLoopExitsMandatory(const BasicBlock* endsVarying)
{
    Loop* loop = mLoopInfo.getLoopFor(endsVarying);

    // No exits that can be mandatory
    if (!loop)
        return;

    // MANDATORY case 4: can be nicely put with just the cdg,
    //                   if an exit is control dependent on
    //                   this varying branch, then there is
    //                   one path that stays in the loop and
    //                   eventually reaches the latch, and one
    //                   that reaches the exit, as its control dependent

    SmallVector<BasicBlock*, 8> LoopExits;
    loop->getExitBlocks(LoopExits);

    const CDNode* cd_node = mCDG[endsVarying];
    for (const CDNode* cd_succ : cd_node->succs())
        for (const BasicBlock* exit : LoopExits)
            if (exit == cd_succ->getBB())
                mVecinfo.markMandatory(exit);
}

void
PDA::markSuccessorsMandatory(const BasicBlock* endsVarying)
{
    // MANDATORY case 1: successors of varying branches are mandatory
    for (const BasicBlock* succBB : successors(endsVarying))
        mVecinfo.markMandatory(succBB);
}

bool
PDA::allOperandsHaveShape(const Instruction* I)
{
    auto hasKnownShape = [this](Value* op)
    {
        return !isa<Instruction>(op) || mValue2Shape.count(op);
    };

    return all_of(I->operands(), hasKnownShape);
}

void
PDA::compute(Function& F)
{
    /* Worklist algorithm to compute the least fixed-point */
    while (!mWorklist.empty())
    {
        const Instruction* I = *mWorklist.begin();
        mWorklist.erase(I);

        // Skip until all operands have a shape
        if (!isa<PHINode>(I) && !allOperandsHaveShape(I))
            continue;

        const VectorShape& New = computeShapeForInst(I);
        update(I, New);
    }
}

VectorShape
PDA::computeShapeForInst(const Instruction* I)
{
    if (I->isBinaryOp())
        return computeShapeForBinaryInst(cast<BinaryOperator>(I));

    if (I->isCast())
        return computeShapeForCastInst(cast<CastInst>(I));

    switch (I->getOpcode())
    {
        case Instruction::GetElementPtr:
        {
            const GetElementPtrInst* gep = cast<GetElementPtrInst>(I);
            const DataLayout layout(I->getModule());

            const Value* pointer = gep->getPointerOperand();

            VectorShape result = getShape(pointer);

            Type* subT = gep->getPointerOperandType();

            for (const Value* index : make_range(gep->idx_begin(), gep->idx_end()))
            {
                const VectorShape indexShape = getShape(index);
                const int indexStride = indexShape.getStride();
                const int indexAlignment = indexShape.getAlignment();

                if (indexShape.isVarying())
                    return VectorShape::varying();

                if (isa<StructType>(subT))
                {
                    if (!isa<ConstantInt>(index))
                        return VectorShape::varying();

                    subT = cast<StructType>(subT)->getTypeAtIndex(index);

                    if (result.isVarying())
                        result = VectorShape::varying(1); // unaligned FIXME
                    else
                        result = VectorShape(result.getStride() + indexStride, 1);
                }
                else
                {
                    subT = cast<SequentialType>(subT)->getPointerElementType();

                    unsigned typeSize = (unsigned)layout.getTypeSizeInBits(subT) / BYTE_SIZE;

                    if (result.isVarying())
                        result = VectorShape::varying(result.getAlignment());
                    else
                        result = VectorShape(result.getStride() + typeSize * indexStride,
                                             result.getAlignment());
                }
            }

            return result;
        }

        case Instruction::Call:
        {
            const Function* callee = cast<CallInst>(I)->getCalledFunction();
            assert (!callee->getReturnType()->isVoidTy());

            /* Find the shape that is mapped to this function */
            auto found = mFuncinfo.find(callee);

            /* No mapping -> assume most unprecise, varying */
            if (found == mFuncinfo.end())
                return VectorShape::varying();

            const VectorMapping* mapping = found->second;
            const VectorShapeVec Arginfo = mapping->argShapes;

            unsigned int i = 0;
            for (auto& op : callee->operands())
            {
                const VectorShape expected = Arginfo[i++];
                const VectorShape actual   = getShape(op);

                // Anything goes
                if (expected.isVarying())
                    continue;

                // Too unprecise
                if (actual.isVarying())
                    return VectorShape::varying();

                // The strides must be the same
                // and alignment has to be strict enough
                if (expected.getStride() != actual.getStride() ||
                    actual.getAlignment() % expected.getAlignment() == 0)
                    return VectorShape::varying();
            }

            return mapping->resultShape;
        }

        case Instruction::PHI:
        {
            // If the block is divergent the phi is varying
            if (getShape(I->getParent()).isVarying()) {
                return VectorShape::varying();
            }

            // Collect defined values
            VectorShapeVec DefinedValues;
            for (auto& op : I->operands())
                if (isa<Constant>(op) || mValue2Shape.count(op))
                    DefinedValues.push_back(getShape(op));

            // Join them (non-empty, at least one is defined)
            assert (!DefinedValues.empty());
            VectorShape Joined = DefinedValues[0];
            for (VectorShape shape : DefinedValues)
                Joined = VectorShape::join(Joined, shape);

            return Joined;
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

            const VectorShape condShape = getShape(condition);
            const VectorShape sel1Shape = getShape(selection1);
            const VectorShape sel2Shape = getShape(selection2);

            if (!condShape.isUniform())
                return VectorShape::varying();

            return VectorShape::join(sel1Shape, sel2Shape);
        }

        // Join all operands
        default:
        {
            VectorShape ShapeComputed = getShape(*I->op_begin());
            for (auto& op : I->operands())
                ShapeComputed = VectorShape::join(ShapeComputed, getShape(op));

            return ShapeComputed;
        }
    }
}

VectorShape
PDA::computeShapeForBinaryInst(const BinaryOperator* I)
{
    assert (I->isBinaryOp());

    const Value* op1 = I->getOperand(0);
    const Value* op2 = I->getOperand(1);

    const VectorShape shape1 = getShape(op1);
    const VectorShape shape2 = getShape(op2);

    const int stride1 = shape1.getStride();
    const int stride2 = shape2.getStride();

    const int alignment1 = shape1.getAlignment();
    const int alignment2 = shape2.getAlignment();

    switch (I->getOpcode())
    {
        // Addition can be optimized in case of strided shape (adding strides)
        case Instruction::Add:
        case Instruction::FAdd:
        {
            const bool fadd = I->getOpcode() == Instruction::FAdd;

            const int resAlignment = VectorShape::gcd(alignment1, alignment2);

            if (shape1.isVarying() || shape2.isVarying())
                return VectorShape::varying(resAlignment);

            VectorShape res = VectorShape(stride1 + stride2, resAlignment);

            // Only allow strided results for floating point addition if
            // according fast math flags are set
            if (fadd)
            {
                FastMathFlags flags = I->getFastMathFlags();
                if (!flags.unsafeAlgebra())
                {
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

            const int resAlignment = VectorShape::gcd(alignment1, alignment2);

            if (shape1.isVarying() || shape2.isVarying())
                return VectorShape::varying(resAlignment);

            VectorShape res = VectorShape(stride1 - stride2, resAlignment);

            // Only allow strided results for floating point substraction if
            // according fast math flags are set
            if (fsub)
            {
                FastMathFlags flags = I->getFastMathFlags();
                if (!flags.unsafeAlgebra())
                {
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
                return VectorShape::varying();

            if (shape1.isUniform() && shape2.isUniform())
                return VectorShape::uni(alignment1 * alignment2);

            // If the constant is known, compute the new shape directly
            if (const ConstantInt* constantOp = dyn_cast<ConstantInt>(op1))
            {
                const int c = (int) constantOp->getSExtValue();
                return VectorShape(c * stride2, c * alignment2);
            }

            // Symmetric case
            if (const ConstantInt* constantOp = dyn_cast<ConstantInt>(op2))
            {
                const int c = (int) constantOp->getSExtValue();
                return VectorShape(c * stride1, c * alignment1);
            }

            return VectorShape::varying();
        }

        //case Instruction::FMul:

        case Instruction::SDiv:
        case Instruction::UDiv:
        {
            if (shape1.isUniform() && shape2.isUniform())
            {
                return VectorShape::uni(alignment1 / alignment2);
            }

            if (const ConstantInt* constantOp = dyn_cast<ConstantInt>(op2))
            {
                const int c = (int) constantOp->getSExtValue();

                if (stride1 % c == 0)
                {
                    return VectorShape(stride1 / c, alignment1 / c);
                }
            }

            return VectorShape::varying();
        }

        //case Instruction::FDiv:

        // If both compared values have the same stride the comparison is uniform
        // This is new and not recognized by the old analysis
        case Instruction::ICmp:
        {
            if (shape1.isVarying() || shape2.isVarying())
                return VectorShape::varying();

            // Same shape and not varying
            if (shape1.getStride() == shape2.getStride())
                return VectorShape::uni();

            CmpInst::Predicate predicate = cast<CmpInst>(I)->getPredicate();
            const int vectorWidth = mVecinfo.getMapping().vectorWidth;
            const int strideGap = stride1 - stride2;

            if (predicate == CmpInst::Predicate::ICMP_SLT ||
                predicate == CmpInst::Predicate::ICMP_ULT)
            {
                // There are 2 possibilities:
                // 1. The first vector1 entry is not smaller than the first vector2 entry
                //    If strideGap >= 0, the gap does not decrease and so all other entries
                //    are pairwise not smaller
                // 2. The first vector1 entry is smaller...
                //    If the alignment a is the same, we can at least add a until we are possibly
                //    equal. But since strideGap * vectorWidth <= a, the accumulated difference
                //    does not exceed a and all entries are pairwise smaller
                if (strideGap >= 0 &&
                    alignment1 == alignment2 &&
                    strideGap * vectorWidth <= alignment1)
                {
                    return VectorShape::uni();
                }
            }
            else if (predicate == CmpInst::Predicate::ICMP_SGT ||
                     predicate == CmpInst::Predicate::ICMP_UGT)
            {
                // Analogous reasoning to the one above
                if (strideGap <= 0 &&
                    alignment1 == alignment2 &&
                    -strideGap * vectorWidth <= alignment1)
                {
                    return VectorShape::uni();
                }
            }

            return VectorShape::varying();
        }

        default:
        {
            if (shape1.isUniform() && shape2.isUniform())
                return VectorShape::uni();

            return VectorShape::varying();
        }
    }
}

VectorShape
PDA::computeShapeForCastInst(const CastInst* castI)
{
    const Value* castOp = castI->getOperand(0);
    const VectorShape castOpShape = getShape(castOp);
    const int castOpStride = castOpShape.getStride();
    const int castOpAlignment = castOpShape.getAlignment();
    const DataLayout layout(castI->getModule());

    const int aligned = !rv::returnsVoidPtr(*castI) ? castOpShape.getAlignment() : 1;

    switch (castI->getOpcode())
    {
        case Instruction::IntToPtr:
        {
            PointerType* DestType = cast<PointerType>(castI->getDestTy());
            Type* DestPointsTo = DestType->getPointerElementType();

            // FIXME: void pointers are char pointers (i8*), but what
            // difference is there between a real i8* and a void pointer?
            if (DestPointsTo->isIntegerTy(8))
                return VectorShape::varying();

            unsigned typeSize = (unsigned) layout.getTypeSizeInBits(DestPointsTo);

            if (castOpStride % typeSize != 0)
                return VectorShape::varying();

            return VectorShape(castOpStride / typeSize, 1);
        }

        case Instruction::PtrToInt:
        {
            PointerType* SrcType = cast<PointerType>(castI->getSrcTy());
            Type* SrcPointsTo = SrcType->getPointerElementType();

            unsigned typeSize = (unsigned) layout.getTypeSizeInBits(SrcPointsTo);

            return VectorShape(typeSize * castOpStride, 1);
        }

        // Truncation reinterprets the stride modulo the target type width
        // i16 to i1: stride(odd) -> consecutive, stride(even) -> uniform
        case Instruction::Trunc:
        {
            Type* destTy = castI->getDestTy();

            return VectorShape::truncateToTypeSize(castOpShape,
                                                   (unsigned)layout.getTypeSizeInBits(destTy));
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

            unsigned long srcElementSize = layout.getTypeSizeInBits(srcPtr->getElementType());
            unsigned long destElementSize = layout.getTypeSizeInBits(destPtr->getElementType());

            return VectorShape(srcElementSize * castOpStride / destElementSize, 1);
        }

        default:
            return VectorShape::join(VectorShape::uni(aligned), castOpShape);
    }
}

VectorShape
PDA::getShape(const Value* const V) const
{
    if (isa<Constant>(V))
    {
        return VectorShape::uni(getAlignment(cast<Constant>(V))); // TODO alignment
    }

    auto found = mValue2Shape.find(V), end = mValue2Shape.end();
    assert (found != end && "Value has not been computed yet");
    return found->second;
}

inline std::string
NAME(const llvm::Value* V)
{
    if (V->hasName() && V->getName().size() > 0)
        return V->getName().str();
    std::string S;
    llvm::raw_string_ostream O(S);
    V->print(O);
    return O.str();
}

void
PDA::markDivergentLoopLatchesMandatory()
{
    for (const Loop* loop : mLoopInfo)
        markLoopLatchesRecursively(loop);
}

void
PDA::markLoopLatchesRecursively(const Loop* loop)
{
    // MANDATORY case 3: latches of divergent loops are mandatory
    if (mVecinfo.isDivergentLoop(loop))
        mVecinfo.markMandatory(loop->getLoopLatch());

    for (const Loop* sLoop : loop->getSubLoops())
        markLoopLatchesRecursively(sLoop);
}

typename ValueMap::iterator PDA::begin() { return mValue2Shape.begin(); }
typename ValueMap::iterator PDA::end() { return mValue2Shape.end(); }

typename ValueMap::const_iterator PDA::begin() const { return mValue2Shape.begin(); }
typename ValueMap::const_iterator PDA::end() const { return mValue2Shape.end(); }

}
