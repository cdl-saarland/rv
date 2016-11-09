//===- vectorizationAnalysis.cpp ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @authors karrenberg, kloessner, simon
//

#include "rv/analysis/vectorizationAnalysis.h"

#include <stdexcept>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Constants.h> // ConstantInt
#include <llvm/IR/CFG.h> // GraphTraits
#include <llvm/IR/InstIterator.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Analysis/PostDominators.h>
#include <llvm/IR/Verifier.h> // verifyFunction()
#include <llvm/IR/Dominators.h>
#include <llvm/Support/raw_os_ostream.h>

#include "rv/utils/valueInfoMap.h"
#include "rv/utils/functionInfoMap.h"
#include "rv/rvInfoProxyPass.h"
#include "rv/VectorizationInfoProxyPass.h"

#include "utils/rvTools.h"
#include "utils/metadata.h"
#include "SketchGraph.h"
#include "PathFinder.h"

#include "rvConfig.h"

using namespace llvm;
using namespace rv;


char VectorizationAnalysisWrapper::ID = 0;
INITIALIZE_PASS_BEGIN(VectorizationAnalysisWrapper, "vectorizationAnalysis", "Vectorization Analysis", false, true)
INITIALIZE_PASS_DEPENDENCY(PostDominatorTree)
INITIALIZE_PASS_DEPENDENCY(DominatorTreeWrapperPass)
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass)
//INITIALIZE_PASS_DEPENDENCY(LoopSimplify)
INITIALIZE_PASS_END(VectorizationAnalysisWrapper, "vectorizationAnalysis", "Vectorization Analysis", false, true)

// Public interface to the VectorizationAnalysis pass
FunctionPass*
llvm::createVectorizationAnalysisPass()
{
    return new VectorizationAnalysisWrapper();
}


static bool
IsMaskIntrinsic(CallInst * call) {
	auto * callee = call->getCalledFunction();
	if (!callee) return false;
	if (callee->getName() == "rv_any") return true;
	if (callee->getName() == "rv_all") return true;
	return false;
}

static bool
IsMaskIntrinsicBranch(BranchInst * br) {
	if (br->isUnconditional()) return false;
	auto * cond = br->getCondition();
	auto * call = dyn_cast<CallInst>(cond);
	if (!call) return false;
	return IsMaskIntrinsic(call);
}

VectorizationAnalysis::VectorizationAnalysis(Function*                   scalarFn,
                                             const Function*             simdFn,
                                             const unsigned              vectorizationFactor,
                                             const int                   maskPosition,
                                             const rv::ValueInfoMap*     valueInfoMap,
                                             const rv::FunctionInfoMap*  functionInfoMap,
                                             const bool                  disableMemAccessAnalysis,
                                             const bool                  disableControlFlowDivAnalysis,
                                             const bool                  disableAllAnalyses,
                                             const bool                  verbose,
                                             RVInfo*                     info,
                                             VectorizationInfo&          vecInfo,
                                             const LoopInfo&             loopInfo,
                                             const PostDominatorTree&    postDomTree,
                                             const DominatorTree&        domTree)
:       mInfo(info),
        mVecInfo(vecInfo),
        mScalarFunction(scalarFn),
        mSimdFunction(simdFn),
        mMaskPosition(maskPosition),
        mValueInfoMap(valueInfoMap),
        mFunctionInfoMap(functionInfoMap),
        mDisableMemAccessAnalysis(disableMemAccessAnalysis),
        mDisableControlFlowDivAnalysis(disableControlFlowDivAnalysis),
        mDisableAllAnalyses(disableAllAnalyses),
        mVerbose(verbose),
        mVectorizationFactor(vectorizationFactor),
        mLoopInfo(loopInfo),
        mPostDomTree(postDomTree),
        mDomTree(domTree)
{
    // RVInterface should set disableMemAccessAnalysis and disableControlFlowDivAnalysis
    // to 'false' if disableAllAnalyses is set.
    assert ((!mDisableAllAnalyses ||
             (mDisableMemAccessAnalysis && mDisableControlFlowDivAnalysis)) &&
            "expecting all analyses to internally be disabled if 'disableAllAnalyses' is set!");
    assert (scalarFn && !verifyFunction(*scalarFn) &&
            "constructor has to be called with valid scalar function!");
    assert (simdFn && "constructor has to be called with valid simd function!");
    assert (vectorizationFactor % 4 == 0 && "vectorizationFactor must be multiple of 4!");
    assert (maskPosition == -1 ||
            (maskPosition >= 0 && maskPosition < (int)simdFn->getFunctionType()->getNumParams()));
}

VectorizationAnalysis::~VectorizationAnalysis()
{
}

VectorizationAnalysisWrapper::
VectorizationAnalysisWrapper() : FunctionPass(ID) {
    // We can not use "AnalysisPassOnce" here because the test infrastructure
    // calls this for every test case.
    initializeVectorizationAnalysisWrapperPass(*PassRegistry::getPassRegistry());
}

void
VectorizationAnalysisWrapper::releaseMemory()
{
}

void
VectorizationAnalysisWrapper::getAnalysisUsage(AnalysisUsage &AU) const
{
	AU.addRequired<RVInfoProxyPass>();
    AU.addRequired<PostDominatorTree>();
    AU.addRequired<DominatorTreeWrapperPass>();
    AU.addRequired<LoopInfoWrapperPass>();
    AU.addRequired<VectorizationInfoProxyPass>();

    // Fore some reason, the analysis does not have to be required -
    // it works if it is just added to the passmanager...
    //AU.addRequired<VectorizationPrereqVerifier>();

    // 'Unable to schedule' because this is a LoopPass.
    // FIXME: I currently have no idea how we can do this.
    //AU.addRequiredID(LoopSimplifyID);

    AU.setPreservesAll();
}

bool
VectorizationAnalysisWrapper::doInitialization(Module& M)
{
    // The return value presumably signals whether the module was changed or not.
    // There is no documentation on this in LLVM.
    return false;
}

bool
VectorizationAnalysisWrapper::doFinalization(Module& M)
{
    // The return value presumably signals whether the module was changed or not.
    // There is no documentation on this in LLVM.
    return false;
}

bool
VectorizationAnalysisWrapper::runOnFunction(Function& F)
{
    RVInfo* Info                        = &getAnalysis<RVInfoProxyPass>().getInfo();
    const LoopInfo& LoopInfo             = getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
    const PostDominatorTree& PostDomTree = getAnalysis<PostDominatorTree>();
    const DominatorTree& DomTree         = getAnalysis<DominatorTreeWrapperPass>().getDomTree();
    VectorizationInfo& vectorizationInfo = getAnalysis<VectorizationInfoProxyPass>().getInfo();

    VectorizationAnalysis Analysis(&F,
                                   Info->mSimdFunction,
                                   Info->mVectorizationFactor,
                                   Info->mMaskPosition,
                                   &Info->mValueInfoMap,
                                   &Info->mFunctionInfoMap,
                                   Info->mDisableMemAccessAnalysis,
                                   Info->mDisableControlFlowDivAnalysis,
                                   Info->mDisableAllAnalyses,
                                   Info->mVerbose,
                                   Info,
                                   vectorizationInfo,
                                   LoopInfo,
                                   PostDomTree,
                                   DomTree);

    Analysis.run(F);

    // Function was not changed.
    return false;
}

void
VectorizationAnalysisWrapper::print(raw_ostream& O, const Module* M) const
{
}

void
VectorizationAnalysis::run(Function& F)
{
    assert (mScalarFunction && "scalar function must be set before runOnFunction() is called!");
    assert (mSimdFunction && "scalar function must be set before runOnFunction() is called!");

    // If an error occurred in a previous pass, abort.
    assert (&F == mScalarFunction);

    DEBUG_VA(
            outs() << "\n";
            mValueInfoMap->print(outs());
            outs() << "\n";
    );

    try
    {
        analyzeMaskInfo(F); // FIXME still need METADATA_MASK
    }
    catch (std::logic_error& error)
    {
        errs() << "\nException occurred during vectorization analysis: "
        << error.what() << "\n";
    }
    catch (...)
    {
        errs() << "\nINTERNAL ERROR: Unexpected exception occurred during "
        << "vectorization analysis!\n";
    }
}

void
VectorizationAnalysis::setMarksFromOutside(Value*                   value,
                                           const rv::ValueInfoMap& valueInfoMap)
{
    const rv::ValueInfo& info = valueInfoMap.get(*value);

    //
    // OP_UNIFORM / OP_VARYING / OP_SEQUENTIAL / OP_SEQUENTIAL_GUARDED.
    //
    if (info.mIsOpUniform)
    {
        assert (info.mIsResultUniform && "OP_UNIFORM value has to be RESULT_SAME!");
        assert (info.mIsIndexSame && "OP_UNIFORM value has to be INDEX_SAME!");
        assert (isa<Instruction>(value) && "Only instructions can be OP_UNIFORM!");
        markValueAs(value, rv::RV_METADATA_OP_UNIFORM);
    }
    else if (info.mIsOpVarying)
    {
        assert (!info.mIsResultUniform && "Only OP_UNIFORM values can be RESULT_SAME!");
        assert (!info.mIsIndexSame && "Only OP_UNIFORM values can be INDEX_SAME!");
        assert (isa<Instruction>(value) && "Only instructions can be OP_VARYING!");
        recursivelyMarkVarying(cast<Instruction>(value), nullptr);
        markValueAs(value, rv::RV_METADATA_OP_VARYING);
    }
    else if (info.mIsOpSequential)
    {
        assert (isa<Instruction>(value) && "Only instructions can be OP_SEQUENTIAL!");
        recursivelyMarkVarying(cast<Instruction>(value), nullptr);
        markValueAs(value, rv::RV_METADATA_OP_SEQUENTIAL);
    }
    else if (info.mIsOpSequentialGuarded)
    {
        assert (isa<Instruction>(value) && "Only instructions can be OP_SEQUENTIAL_GUARDED!");
        recursivelyMarkVarying(cast<Instruction>(value), nullptr);
        markValueAs(value, rv::RV_METADATA_OP_SEQUENTIAL_GUARDED);
    }

    //
    // RES_UNIFORM / RES_VECTOR.
    //
    if (info.mIsResultUniform)
    {
        markValueAs(value, rv::RV_METADATA_RES_UNIFORM);
    }
    else if (info.mIsResultVector)
    {
        markValueAs(value, rv::RV_METADATA_RES_VECTOR);
    }
    else if (info.mIsResultScalars)
    {
        assert ((!isa<Instruction>(value) ||
                 (info.mIsOpSequential || info.mIsOpSequentialGuarded)) &&
                "RESULT_SCALARS instruction has to be OP_SEQUENTIAL or OP_SEQUENTIAL_GUARDED!");
        markValueAs(value, rv::RV_METADATA_RES_SCALARS);
    }

    // Since arguments may not have OP_* marks, we have to "manually"
    // make sure that all uses are marked as OP_VARYING if the argument
    // is not RES_UNIFORM.
    if (Argument* arg = dyn_cast<Argument>(value))
    {
        if (info.mIsResultVector || info.mIsResultScalars)
        {
            // Recurse into uses (DFS).
            for (Argument::use_iterator U = arg->use_begin(),
                UE = arg->use_end(); U != UE; ++U)
            {
                Instruction* useInst = cast<Instruction>(U->getUser());
                recursivelyMarkVarying(useInst, nullptr);
            }
        }
    }

    //
    // INDEX_SAME / INDEX_CONSECUTIVE / INDEX_RANDOM.
    //
    if (info.mIsIndexSame)
    {
        assert (!isa<Instruction>(value) ||
                rv::hasMetadata(value, rv::RV_METADATA_OP_UNIFORM));
        assert (rv::hasMetadata(value, rv::RV_METADATA_RES_UNIFORM));
        markValueAs(value, rv::RV_METADATA_INDEX_SAME);
    }
    else if (info.mIsIndexConsecutive)
    {
        assert (!isa<Instruction>(value) ||
                !rv::hasMetadata(value, rv::RV_METADATA_OP_UNIFORM));
        assert (!rv::hasMetadata(value, rv::RV_METADATA_RES_UNIFORM));
        markValueAs(value, rv::RV_METADATA_INDEX_CONSECUTIVE);
    }
    else
    {
        assert (!isa<Instruction>(value) ||
                !rv::hasMetadata(value, rv::RV_METADATA_OP_UNIFORM));
        assert (!rv::hasMetadata(value, rv::RV_METADATA_RES_UNIFORM));
        markValueAs(value, rv::RV_METADATA_INDEX_RANDOM);
    }

    //
    // ALIGNED_TRUE / ALIGNED_FALSE
    //
    markValueAs(value, info.mIsAligned ?
        rv::RV_METADATA_ALIGNED_TRUE :
        rv::RV_METADATA_ALIGNED_FALSE);

    return;
}

void
VectorizationAnalysis::setUserDefinedMarks(Function*                   scalarFn,
                                           const rv::ValueInfoMap&    valueInfoMap,
                                           const rv::FunctionInfoMap& functionInfoMap)
{
    // Mark arguments that are defined as OP_VARYING by the user via
    // addSIMDSemantics() as well as their uses.
    for (Argument & A : scalarFn->getArgumentList()) {
        if (!valueInfoMap.hasMapping(A)) continue;
        setMarksFromOutside(&A, valueInfoMap);
    }

    // Mark instructions that are defined as OP_VARYING by the user via
    // addSMDSemantics() or addFunctionMapping() as well as their uses.
    for (llvm::inst_iterator I=inst_begin(scalarFn), IE=inst_end(scalarFn); I!=IE; ++I)
    {
        Instruction* inst = &*I;

        // 1) Check if this instruction was marked from outside.
        if (valueInfoMap.hasMapping(*inst))
        {
            setMarksFromOutside(inst, valueInfoMap);
            continue;
        }

        // 2) Check if this is a call to a function that has a mapping.
        // If the user specified a mapping for this call (or the called
        // function) to have no side effects, we may leave it scalar if
        // the arguments allow it (so we initialize it with OP_UNIFORM).
        if (!isa<CallInst>(inst)) continue;
        CallInst* call = cast<CallInst>(inst);

        Function* calledFn = call->getCalledFunction();
        if (!calledFn || !functionInfoMap.hasMapping(*calledFn)) continue;

        // If the function does not have side effects and it is not already
        // marked as OP_VARYING due to some data dependency, we mark it
        // OP_UNIFORM/RES_UNIFORM (for now, may change during fixpoint
        // iteration).
        if (!functionInfoMap.mayHaveSideEffects(*calledFn) &&
            !rv::hasMetadata(call, rv::RV_METADATA_OP_VARYING))
        {
            rv::setMetadata(call, rv::RV_METADATA_OP_UNIFORM);
            rv::setMetadata(call, rv::RV_METADATA_RES_UNIFORM);
        }
        else
        {
            recursivelyMarkVarying(call, nullptr);
        }
    }
}


void
VectorizationAnalysis::analyzeNothing(Function* scalarFn, const bool uniformReturn)
{
    assert (scalarFn);
    // TODO: It should be enough to not mark anything,
    //       this should result in the most conservative
    //       result, which is to expect all instructions
    //       to be OP_VARYING, all blocks to be MANDATORY,
    //       and all loops to be LOOP_DIVERGENT (#20).
    //return;

    for (Argument & A : scalarFn->getArgumentList()) {
        if (mValueInfoMap->hasMapping(A)) {
            continue;
        }

        if (A.getType()->isPointerTy())
        {
            rv::setMetadata(&A, rv::RV_METADATA_RES_SCALARS);
        }
        else
        {
            rv::setMetadata(&A, rv::RV_METADATA_RES_VECTOR);
        }

        rv::setMetadata(&A, rv::RV_METADATA_INDEX_RANDOM);
        rv::setMetadata(&A, rv::RV_METADATA_ALIGNED_FALSE);
    }

    for (auto &BB : *scalarFn)
    {
        // Mark all instructions as OP_VARYING/RES_VECTOR
        // except for unconditional branches and void-returns.
        for (auto &I : BB)
        {
            // Ignore our own metadata calls.
            if (rv::isMetadataCall(&I)) continue;

            if (BranchInst* br = dyn_cast<BranchInst>(&I))
            {
                if (br->isUnconditional())
                {
                    markValueAs(&I, rv::RV_METADATA_OP_UNIFORM);
                    continue;
                } else if (IsMaskIntrinsicBranch(br)) {
                	markValueAs(&I, rv::RV_METADATA_OP_UNIFORM);
					continue;
                }
            }
            else if (ReturnInst* ret = dyn_cast<ReturnInst>(&I))
            {
                if (!ret->getReturnValue() || uniformReturn)
                {
                    markValueAs(&I, rv::RV_METADATA_OP_UNIFORM); // TODO: Why not also RES_UNIFORM?
                    continue;
                }
            }

            if (mValueInfoMap->hasMapping(I))
            {
                continue;
            }

            markValueAs(&I, rv::RV_METADATA_OP_VARYING);
            markValueAs(&I, rv::RV_METADATA_RES_VECTOR);
            markValueAs(&I, rv::RV_METADATA_INDEX_RANDOM);
            markValueAs(&I, rv::RV_METADATA_ALIGNED_FALSE);
        }

        // Mark all loops as LOOP_DIVERGENT.
        if (mLoopInfo.isLoopHeader(&BB))
        {
            markValueAs(&BB, rv::RV_METADATA_LOOP_DIVERGENT_TRUE);
        }
    }

    // Mark DIVERGENT blocks.
    // We can't just mark each non-header block with more than one incoming edge
    // as DIVERGENT since we also need the divergence-causing blocks.
    markDivergentBlocks(scalarFn);

    // Mark MANDATORY blocks.
    // Similiarly, we can't just mark blocks MANDATORY since we also need the
    // rewire information.
    markMandatoryBlocks(scalarFn);

    // Mark special DIVERGENT loops.
    markNestedDivergentTopLevelLoops();
}

// Mark instructions as OP_UNIFORM or OP_VARYING.
// Mark results of instructions as RES_UNIFORM or RES_VECTOR.
// Mark blocks as MANDATORY or OPTIONAL.
// Mark loops as LOOP_DIVERGENT or LOOP_NON_DIVERGENT.
// Mark of phis depends on mark of parent block (fixpoint iteration required).
// Mark of result of instruction depends on mark of parent loop if
//   it is live across loop boundaries (LALB) (fixpoint iteration required).
void
VectorizationAnalysis::analyzeUniformInfo(Function*                   scalarFn,
                                          const bool                  uniformReturn,
                                          const SmallVector<bool, 4>& uniformArgs)
{
    assert (scalarFn);

    // Start at arguments and user-defined functions,
    // mark all non-UNIFORM instructions as OP_VARYING.
    // Which ones actually are SEQUENTIAL/SEQUENTIAL_GUARDED
    // is analyzed & refined later.

    // Mark instructions that depend on varying arguments as well as their uses.
    // (Only required once, thus not in fixpoint iteration).
    auto itScalarArg = scalarFn->arg_begin();
    for (uint i = 0; itScalarArg != scalarFn->arg_end(); ++itScalarArg, ++i) {
    	Argument & A = *itScalarArg;
        if (mValueInfoMap->hasMapping(A))
        {
            continue;
        }

        if (uniformArgs[i])
        {
            rv::setMetadata(&A, rv::RV_METADATA_RES_UNIFORM);
            assert (rv::hasMetadata(&A, rv::RV_METADATA_RES_UNIFORM));
            continue;
        }

        rv::setMetadata(&A, rv::RV_METADATA_RES_VECTOR);
        assert (rv::hasMetadata(&A, rv::RV_METADATA_RES_VECTOR));

        DEBUG_VA( outs() << "\nmarking uses of argument: " << A << " as OP_VARYING...\n"; );

        // Recurse into uses (DFS).
        for (User * user : A.users()) {
            Instruction* useInst = cast<Instruction>(user);
            recursivelyMarkVarying(useInst, nullptr);
        }
    }

    // TODO: What about non-UNIFORM pointer parameters of
    //       user-defined functions? Is that an issue?
    //       Recurse into operands (as done in rv1)?

    // Mark unconditional branches and void-returns as UNIFORM. (and mask predicate branches)
    for (llvm::inst_iterator I=inst_begin(scalarFn), IE=inst_end(scalarFn); I!=IE; ++I)
    {
        Instruction* inst = &*I;
        if (!isa<BranchInst>(inst) && !isa<ReturnInst>(inst)) continue;

        if (BranchInst* br = dyn_cast<BranchInst>(inst))
        {
            if (br->isUnconditional()) markValueAs(br, rv::RV_METADATA_OP_UNIFORM);
            if (IsMaskIntrinsicBranch(br)) markValueAs(br, rv::RV_METADATA_OP_UNIFORM);
        }
        else
        {
            ReturnInst* ret = cast<ReturnInst>(inst);
            if (!ret->getReturnValue())
            {
                markValueAs(ret, rv::RV_METADATA_OP_UNIFORM);
            }
#if 1
            else if (!uniformReturn)
            {
                // Make sure that return instructions are properly marked as
                // OP_VARYING / RES_VECTOR if the target function requires it.
                // This is necessary for kernels that have a declaration that requires
                // a vector but the actual returned value is known to be uniform.
                markValueAs(ret, rv::RV_METADATA_OP_VARYING); // TODO: Why not also RES_VECTOR?
            }
#else
            else
            {
                // TODO: Isn't this what we want? Why does this fire an assertion for
                //       TestFactorial?
                markValueAs(ret,
                            uniformReturn ?
                                rv::RV_METADATA_OP_UNIFORM :
                                rv::RV_METADATA_OP_VARYING);
            }
#endif
        }
    }

    // Everything else is OP_UNIFORM for now (possibly updated during
    // following fixpoint iteration).
    for (llvm::inst_iterator I=inst_begin(scalarFn), IE=inst_end(scalarFn); I!=IE; ++I)
    {
        Instruction* inst = &*I;

        // Ignore our own metadata calls.
        if (rv::isMetadataCall(inst))
        {
            continue;
        }

        if (rv::hasMetadata(inst, rv::RV_METADATA_OP_VARYING) ||
            rv::hasMetadata(inst, rv::RV_METADATA_OP_SEQUENTIAL) ||
            rv::hasMetadata(inst, rv::RV_METADATA_OP_SEQUENTIAL_GUARDED))
        {
            continue;
        }

        markValueAs(inst, rv::RV_METADATA_OP_UNIFORM);
    }

    if (mDisableControlFlowDivAnalysis)
    {
        // Mark all blocks as mandatory.
        for (auto &BB : *scalarFn)
        {
            // Mark only branches and returns.
            for (auto &I : BB)
            {
                if (!isa<BranchInst>(&I) && !isa<ReturnInst>(&I)) continue;

                if (BranchInst* br = dyn_cast<BranchInst>(&I))
                {
                    if (br->isUnconditional()) continue;
                }
                else if (ReturnInst* ret = dyn_cast<ReturnInst>(&I))
                {
                    if (!ret->getReturnValue()) continue;
                }

                markValueAs(&I, rv::RV_METADATA_OP_VARYING);
                markValueAs(&I, rv::RV_METADATA_RES_VECTOR);
            }

            // Mark all loops as LOOP_DIVERGENT.
            if (mLoopInfo.isLoopHeader(&BB))
            {
                markValueAs(&BB, rv::RV_METADATA_LOOP_DIVERGENT_TRUE);
            }
        }
    }

    // TODO: Introduce shortcuts to stop iteration, e.g. before updating
    //       of phis if nothing changed anyways.
    bool changed = true;
    while (changed)
    {
        changed = false;

        if (!mDisableControlFlowDivAnalysis)
        {
            // Mark loops as LOOP_DIVERGENT/LOOP_NON_DIVERGENT.
            DEBUG_VA( outs() << "\nMarking divergent loops...\n\n"; );
            changed |= markDivergentLoops(mLoopInfo);

            // Mark blocks as ALWAYS_BY_ALL
            DEBUG_VA( outs() << "\nMarking blocks that are always executed by "
                << "all threads...\n\n"; );
            changed |= markAlwaysByAllBlocks(scalarFn);

            // Mark blocks as ALWAYS_BY_ALL_OR_NONE or ALWAYS_BY_ALL_FALSE.
            // NOTE: This is only inside the fixpoint iteration because the derived
            //       information is required for the alloca update.
            changed |= markABAONBlocks(scalarFn);
        }
        // Mark blocks as DIVERGENT/NON_DIVERGENT.
        // We *have* to mark them, even if control flow analyses are disabled.
        // TODO: We have to update the divergence-causing blocks even if
        //       the mark did not change!
        DEBUG_VA( outs() << "\nMarking divergent blocks...\n\n"; );
        changed |= markDivergentBlocks(scalarFn);

        // Update UNIFORM/VARYING information of phis.
        DEBUG_VA( outs() << "\nUpdating phis with divergence information...\n\n"; );
        changed |= updateUniformPhisWithDivergenceInfo(scalarFn);

        // Update UNIFORM/VARYING information of operations with side effects.
        DEBUG_VA( outs() << "\nUpdating operations with possible side effects...\n\n"; );
        changed |= updateUniformSideEffectOperations(scalarFn);

        // Update UNIFORM/VARYING information of allocas.
        DEBUG_VA( outs() << "\nUpdating alloca uniform/varying information...\n\n"; );
        changed |= updateUniformAllocas(scalarFn);

        // Update UNIFORM/VARYING information of LALB-values.
        DEBUG_VA( outs() << "\nUpdating values that are live across loop boundaries...\n\n"; );
        changed |= updateUniformLALBValues(scalarFn);
    }

    // Mark blocks as MANDATORY/OPTIONAL.
    DEBUG_VA( outs() << "\nMarking mandatory/optional blocks...\n\n"; );
    markMandatoryBlocks(scalarFn);

    // Mark all instructions without mark as UNIFORM.
    for (llvm::inst_iterator I=inst_begin(scalarFn), IE=inst_end(scalarFn); I!=IE; ++I)
    {
        Instruction* inst = &*I;
        if (rv::hasMetadata(inst)) continue;
        markValueAs(inst, rv::RV_METADATA_OP_UNIFORM);
    }

    // Mark all OP_UNIFORM without RES_* mark as RES_UNIFORM.
    for (llvm::inst_iterator I=inst_begin(scalarFn), IE=inst_end(scalarFn); I!=IE; ++I)
    {
        Instruction* inst = &*I;
        if (!rv::hasMetadata(inst, rv::RV_METADATA_OP_UNIFORM)) continue;
        if (rv::hasMetadata(inst, rv::RV_METADATA_RES_VECTOR) ||
            rv::hasMetadata(inst, rv::RV_METADATA_RES_SCALARS)) continue;
        markValueAs(inst, rv::RV_METADATA_RES_UNIFORM);
    }

    // Mark loops as TOP_LEVEL_DIVERGENT and/or DIVERGENT_INNERMOST.
    markNestedDivergentTopLevelLoops();
}

static bool
IsDivergentEdge(const BasicBlock * src, const BasicBlock * dest) {
	const BranchInst * inBranch = cast<const BranchInst>(src->getTerminator());
	return !rv::hasMetadata(inBranch, rv::RV_METADATA_OP_UNIFORM) ||
	       ! rv::hasMetadata(src, rv::RV_METADATA_ALWAYS_BY_ALL_OR_NONE);

}

// Recursively marks the instruction and all its uses as OP_VARYING / RES_VECTOR
// except for phis and selects which require some special handling.
// Returns true if some mark was set, false otherwise.
bool
VectorizationAnalysis::recursivelyMarkVarying(Instruction* inst,
                                              BasicBlock*  block)
{
    assert (inst);

    if (rv::hasMetadata(inst, rv::RV_METADATA_OP_VARYING))
    {
        DEBUG_VA( outs() << "    previously marked as OP_VARYING - ignored!\n"; );
        return false;
    }

    if (rv::hasMetadata(inst, rv::RV_METADATA_OP_SEQUENTIAL) ||
        rv::hasMetadata(inst, rv::RV_METADATA_OP_SEQUENTIAL_GUARDED))
    {
        DEBUG_VA( outs() << "    previously marked as OP_SEQUENTIAL(_GUARDED) - ignored!\n"; );
        return false;
    }

    bool changed = false;

    // We have to treat some instructions differently:
    // - a phi can only be marked OP_VARYING by the divergence analysis.
    // - a select is only OP_VARYING if its condition is non-uniform.
    switch (inst->getOpcode())
    {
        case Instruction::PHI:
        {
            // If block is set, this function was called from
            // the divergence analysis and requests the phi to be marked
            // OP_VARYING.
            // Otherwise, mark phi as RES_VECTOR only.
            if (block) { // FIXME hack: this is only valid, if there is only a single value incoming from divergent edges
#if 0
				auto * phi = dyn_cast<PHINode>(inst);

			// Check whether is only one value incoming from divergent edges
				// Remark: this must be a constant because of temporal divergence in loops
				// In the acyclic case, it suffices to check for the same value
				bool divergentPHI = false;
				Constant * divergentEdgeConst = nullptr;
				for (unsigned i = 0; i < phi->getNumIncomingValues(); ++i) {
					BasicBlock * inBlock = phi->getIncomingBlock(i);

					if (IsDivergentEdge(inBlock, block)) {
						if (Constant * thisEdgeConst = dyn_cast<ConstantExpr>(phi->getIncomingValue(i))) {
							if (!divergentEdgeConst) {
								divergentEdgeConst = thisEdgeConst;
							} else if (divergentEdgeConst != thisEdgeConst) {
								divergentPHI = true;
								break;
							}
						}
					}
				}

				// FIXME safe, to not promote vectors here?
				if (! divergentPHI) {
					break;
				}
#endif
			// Default case. One divergent edge-> the entire PHI is divergent
				assert (rv::isExitOfDivergentLoop(*block, mLoopInfo, mVecInfo) ||
						!rv::hasMetadata(block, rv::RV_METADATA_DIVERGENT_FALSE));

				changed |= markValueAs(inst, rv::RV_METADATA_OP_VARYING);
            }
            changed |= markValueAs(inst, rv::RV_METADATA_RES_VECTOR);
            break;
        }

        case Instruction::Select:
        {
            // If the condition is RES_UNIFORM, the select can remain uniform and
            // just choose one of the vectors.
            SelectInst* select = cast<SelectInst>(inst);
            Value* condition = select->getCondition();
            if ((isa<Argument>(condition) || isa<Instruction>(condition)) &&
                !rv::hasMetadata(select->getCondition(), rv::RV_METADATA_RES_UNIFORM))
            {
                changed |= markValueAs(select, rv::RV_METADATA_OP_VARYING);
            }
            changed |= markValueAs(select, rv::RV_METADATA_RES_VECTOR);
            break;
        }

        // Mark instructions that do not have a result as OP_VARYING only.
        case Instruction::Store:
        case Instruction::Ret:
        case Instruction::Br:
        case Instruction::Switch:
        {
            changed |= markValueAs(inst, rv::RV_METADATA_OP_VARYING);
            break;
        }
        case Instruction::Call:
        {
            CallInst* call = cast<CallInst>(inst);

        	if (IsMaskIntrinsic(call)) {  // do not propagate varying info through mask intrinsics
        		return false;
#if 0
        		changed |= markValueAs(call, rv::RV_METADATA_RES_UNIFORM);
        		// changed |= markValueAs(call, rv::RV_METADATA_INDEX_SAME);
        		changed |= markValueAs(call, rv::RV_METADATA_OP_UNIFORM);
        		if (!changed) return false;
#endif
        	}

            if (!call->getType()->isVoidTy())
            {
                changed |= markValueAs(call, rv::RV_METADATA_RES_VECTOR);
            }
            changed |= markValueAs(call, rv::RV_METADATA_OP_VARYING);
            break;
        }

        default:
        {
            assert (!inst->getType()->isVoidTy());
            // Mark as OP_VARYING / RES_VECTOR.
            changed |= markValueAs(inst, rv::RV_METADATA_OP_VARYING);
            changed |= markValueAs(inst, rv::RV_METADATA_RES_VECTOR);
            break;
        }
    }

    if (!changed) return false;

    // Recurse into uses (DFS).
    for (Instruction::use_iterator U = inst->use_begin(),
            UE = inst->use_end(); U != UE; ++U)
    {
        Instruction* useInst = cast<Instruction>(U->getUser());
        recursivelyMarkVarying(useInst, nullptr);
    }

    return true;
}


bool
VectorizationAnalysis::markAlwaysByAllBlocks(Function* scalarFn)
{
    assert (scalarFn);

    if (mMaskPosition != -1)
    {
        DEBUG_VA( outs() << "  Function has mask argument, no blocks can be "
            << "ALWAYS_BY_ALL!\n"; );
        return false; // Nothing changed.
    }

    BasicBlock* potentialABA = &scalarFn->getEntryBlock();

    // Mark the start block.
    bool changed = markValueAs(potentialABA, rv::RV_METADATA_ALWAYS_BY_ALL_TRUE);

    while (potentialABA)
    {
        DomTreeNode* dtn = mPostDomTree.getNode(potentialABA);

        // If there is no post dominator, stop.
        if (!dtn || !dtn->getIDom()) break;

        // Set the post dominator as the current block.
        potentialABA = dtn->getIDom()->getBlock();

        if (!potentialABA) break;

        // If the block is not part of a divergent loop, mark it ABA.
        // NOTE: We have to check *all* loops to which this block belongs.
        Loop* loop              = mLoopInfo.getLoopFor(potentialABA);
        bool  isInDivergentLoop = false;

        while (loop)
        {
            if (!loop->contains(potentialABA))
            {
                break;
            }
            if (rv::hasMetadata(loop, rv::RV_METADATA_LOOP_DIVERGENT_TRUE))
            {
                isInDivergentLoop = true;
                break;
            }
            loop = loop->getParentLoop();
        }

        changed |= markValueAs(potentialABA,
                               isInDivergentLoop ?
                                   rv::RV_METADATA_ALWAYS_BY_ALL_FALSE :
                                   rv::RV_METADATA_ALWAYS_BY_ALL_TRUE);
    }

    return changed;
}

bool
VectorizationAnalysis::markDivergentBlocks(Function* scalarFn)
{
    assert (scalarFn);
    bool changed = false;

    for (auto & BB : *scalarFn) {
        const bool divergent = isDivergent(&BB);
        changed |= markValueAs(&BB,
                               divergent ?
                                   rv::RV_METADATA_DIVERGENT_TRUE :
                                   rv::RV_METADATA_DIVERGENT_FALSE);
    }

    return changed;
}

namespace {

void
printPaths(const rv::PathVecType& paths, raw_ostream& o)
{
    for (const auto &path : paths)
    {
        o << "   * ";
        for (const auto &pathBB : *path)
        {
            o << pathBB->getName() << " ";
        }
        o << "\n";
    }

}

}

Loop *
VectorizationAnalysis::getCommonLoop(const BasicBlock * A, const BasicBlock * B) const {
	const LoopInfo & LI = mLoopInfo;
	Loop * aLoop = LI.getLoopFor(A);
	if (!aLoop) return nullptr;
	Loop * bLoop = LI.getLoopFor(B);
	if (!bLoop) return nullptr;

	while (aLoop != bLoop) {
		uint aDepth = !aLoop ? 0 : aLoop->getLoopDepth();
		uint bDepth = !bLoop ? 0 : bLoop->getLoopDepth();
		if (aDepth >= bDepth) {
			aLoop = aLoop->getParentLoop();
		} else if (aDepth < bDepth) {
			bLoop = bLoop->getParentLoop();
		}
	}

	return aLoop;
}

static inline
const DomTreeNode*
GetIDom(const DominatorTree & domTree, const BasicBlock & block) {
	const DomTreeNode * domNode = domTree.getNode(const_cast<BasicBlock*>(&block));
	return domNode->getIDom();
}

#if 1
#define VA_DBG(X) X
#else
#define VA_DBG(X)
#endif

bool
VectorizationAnalysis::checkForDisjointLoopPaths(const BasicBlock*  exitBlock,
				                                 const Loop*        loop,
				                                 BlockSet*          divCauseBlocks) const {

	NodeSet exitingNodes;
	BasicBlock * entryBlock = &mScalarFunction->getEntryBlock();
	SketchGraph * graph = nullptr;

	const DomTreeNode * idom = GetIDom(mDomTree, *exitBlock);
	BasicBlock * idomBlock = idom ? idom->getBlock() : nullptr;


// Restrict the graph search to @loop and exiting edges from @loop to @exitBlock
	BasicBlock * loopHeader = loop->getHeader();
	const DomTreeNode * loopDom = mDomTree.getNode(loopHeader);

	graph = SketchGraph::createFromDominatorNode(mDomTree, *loopHeader, exitingNodes);

// get the node handle for the exiting node
	const BasicBlock * exitingBlock = exitBlock->getUniquePredecessor();
	assert(exitingBlock);
	auto exitingNode = graph->getNode(*exitingBlock);

// add the exiting node explicitly
	SketchNode * exitNode = graph->push(exitBlock);
	exitingNode->insert(exitNode);

// only consider reachable edges
	const BasicBlock * latchBlock = loop->getLoopLatch();
#if 0
	errs() << "Loop scope "; loop->print(errs());
	llvm::errs() << "Graph "; graph->dump();
	llvm::errs() << "Exit block: "<< exitBlock->getName() << "\n";
	llvm::errs() << "Latch block: "<< latchBlock->getName() << "\n";
#endif
	SketchNode * latchNode = graph->getNode(*latchBlock);
	if (!latchNode) {
		return false; // FIXME
	}

	assert(latchNode && "latchNode killed during clean-up");
	// except for the actual path enumeration, this is copied over from isMandatoryExit(..)
	NodeSet accepting;
	accepting.insert(exitNode);
	accepting.insert(latchNode);
	PathFinder finder(*graph, accepting);

	const auto & loopBlocks = loop->getBlocks();

	bool isMandatoryExit = false;
    for (const auto * vBlock : loopBlocks) {
		if (! rv::HasVaryingBranch(*vBlock, mVecInfo))
			continue;

	   // this only applies to nodes, that vBlock does not post-dominate
	   if ( mPostDomTree.dominates(exitBlock, vBlock) )
		   continue;

	   if ( mPostDomTree.dominates(latchBlock, vBlock) )
		   continue;

    // new path search algorithm starting from here
		SketchNode * parentNode = graph->getNode(*vBlock);
		assert(parentNode && "branch not modelled");

		if (finder.findPath(parentNode)) { // validity is not checked for first node
			DEBUG_VA( outs() << "  Block '" << exitBlock->getName() << "' is MANDATORY (4)!\n"; );
			DEBUG_VA( outs() << "    due to exit '" << vBlock->getName() << "'.\n"; );
			// Add both the current block as well as the latch as rewire targets for the branch parent.
			if (divCauseBlocks) {
				divCauseBlocks->insert(const_cast<BasicBlock*>(exitBlock));
			}
			isMandatoryExit =  true;
			break;
		}
	}

    delete graph;
	return isMandatoryExit;
}

bool
VectorizationAnalysis::checkForDisjointPaths(const BasicBlock*  block,
					                         bool               doNotBranchToOuterLoops) const {
	NodeSet exitingNodes;
	BasicBlock * entryBlock = &mScalarFunction->getEntryBlock();
	SketchGraph * graph = nullptr;

	const DomTreeNode * idom = GetIDom(mDomTree, *block);
	BasicBlock * idomBlock = idom ? idom->getBlock() : nullptr;

	// Restrict the search to nodes in the idom region that reach @block
	if (doNotBranchToOuterLoops) {
		const Loop * loop = mLoopInfo.getLoopFor(block);
		BasicBlock * topBlock = loop ? loop->getHeader() : entryBlock;
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
		keepSet.insert(graph->getNode(*block));
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

    bool ret = false;

	for (auto & pair  : *compactGraph) {
		SketchNode * vCompactNode = pair.first;
		const llvm::BasicBlock * vBlock = pair.second;

		// optimization only look below the idom (if any)
		if (! idomBlock || (vBlock == idomBlock) || mDomTree.dominates(idomBlock, vBlock)) { //  TODO Only nodes below the idom are interesting
			if (rv::HasVaryingBranch(*vBlock, mVecInfo)) {
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
					ret = true;
                    break;
				}
			}
		}
	}

	delete graph;
	if (compactGraph != graph) delete compactGraph;
	return ret;
}

// A block is divergent if there exist two *disjoint* paths from a block v
// that ends with a varying branch to the current block b.
bool
VectorizationAnalysis::isDivergent(BasicBlock* block) const
{
    assert (block);
    DEBUG_VA( outs() << "\nisDivergent(" << block->getName() << ")\n"; );

    if (rv::hasMetadata(block, rv::RV_METADATA_DIVERGENT_TRUE))
    {
        DEBUG_VA( outs() << "  Block '" << block->getName() << "' is DIVERGENT (0)!\n"; );
        return true;
    }

    // A block with less than two incoming edges can not be divergent.
    if (block->getUniquePredecessor() ||
        &block->getParent()->getEntryBlock() == block ||
        pred_begin(block) == pred_end(block))
    {
        DEBUG_VA( outs() << "  has less than two incoming edges - NON_DIVERGENT!\n"; );
        return false;
    }

    return checkForDisjointPaths(block, true);
}

bool
VectorizationAnalysis::markMandatoryBlocks(Function* scalarFn)
{
    assert (scalarFn);

    bool changed = false;
    for (auto & BB : *scalarFn) {
        changed |= markValueAs(&BB,
                               isMandatory(&BB, mLoopInfo) ?
                                   rv::RV_METADATA_MANDATORY :
                                   rv::RV_METADATA_OPTIONAL);
    }

    return changed;
}

#if 0
void
VectorizationAnalysis::findUniformBlocks() {
	return;
	outs() << "== Analyzing mask intrinsics == \n";
	for (BasicBlock & block : *mScalarFunction) {
		auto itPredBlock = pred_begin(&block);
		if (itPredBlock == pred_end(&block)) continue;
		BasicBlock * onlyPred = *itPredBlock;
		itPredBlock++;
		if (itPredBlock != pred_end(&block)) continue;

		outs () << "- single pred block " << block.getName() << "\n";

		auto * predBranch = dyn_cast<BranchInst>(onlyPred->getTerminator());
		if (!predBranch) continue;
		if (!predBranch->isConditional()) continue;

		auto * predBranchCond = predBranch->getCondition();

		auto * condCall = dyn_cast<CallInst>(predBranchCond);
		if (!condCall) continue;

		if (!IsMaskIntrinsic(condCall)) continue;

		outs() << "Guarded by mask intrinsic : " << block.getName() << "\n";
		uniformBlocks.insert(&block);
	}
	outs() << "== [EOA] == \n";
}
#endif



// If the block only depends on non-divergent control-flow,
// it is "always by all or none".
// Those are blocks that have only ABA or ABAON predecessors
// with uniform branches.
bool
VectorizationAnalysis::markABAONBlocks(Function* scalarFn)
{
    assert (scalarFn);

    if (mMaskPosition != -1)
    {
        DEBUG_VA( outs() << "  Function has mask argument, no blocks can be "
            << "ALWAYS_BY_ALL_OR_NONE!\n"; );
        return false; // Nothing changed.
    }

    bool changed = false;

    // There might be dependencies between ABAON blocks, so we have to make
    // sure we do not mark blocks as ABA_FALSE too early.
    // Blocks that post dominate ABAON blocks have to be ABAON as well,
    // unless they are in divergent loops (just like ABA analysis).
    // Do a top-down DFS and only mark blocks of which we have seen all predecessors.
    SmallVector<BasicBlock*, 8> stack;
    SmallPtrSet<BasicBlock*, 16> visitedBlocks;
    stack.push_back(&scalarFn->getEntryBlock());
    bool isFollowedPostDom = false;

    while (!stack.empty())
    {
        BasicBlock* block = stack.pop_back_val();

        // If we have already seen this block, stop recursion.
        if (visitedBlocks.count(block)) continue;
#if 1
        // Mark user-specified uniform blocks as ABAON
        if (uniformBlocks.count(block)) {
			changed |= markValueAs(block, rv::RV_METADATA_ALWAYS_BY_ALL_OR_NONE);
			continue;
        }
#endif
        // If we have not yet seen all predecessors, stop recursion for this path,
        // unless we are in a loop header and the predecessor is the latch, or we
        // are following a post dominator.
        // In case of a loop header, mark it depending on the loop's DIVERGENT mark,
        // and continue as if we had seen all predecessors.
        // In case of a post dominator, mark it ABAON and continue as if we had seen
        // all predecessors.
        Loop* loop = mLoopInfo.getLoopFor(block);
        const bool isLoopHeader = loop && loop->getHeader() == block;
        bool allSeen = true;
        for (pred_iterator P=pred_begin(block), PE=pred_end(block); P!=PE; ++P)
        {
            BasicBlock* predBB = *P;
            if (visitedBlocks.count(predBB)) continue;
            allSeen = false;
            break;
        }
        if (!allSeen && !isLoopHeader && !isFollowedPostDom) continue;

        // Otherwise, mark current block as seen, and reset postdom flag.
        visitedBlocks.insert(block);
        isFollowedPostDom = false;

        // Push all successors onto the stack for DFS traversal.
        TerminatorInst* terminator = block->getTerminator();
        for (unsigned i=0, e=terminator->getNumSuccessors(); i<e; ++i)
        {
            BasicBlock* succBB = terminator->getSuccessor(i);
            stack.push_back(succBB);
        }

        // If the block is marked already, skip it.
        if (rv::hasMetadata(block, rv::RV_METADATA_ALWAYS_BY_ALL_TRUE) ||
            rv::hasMetadata(block, rv::RV_METADATA_ALWAYS_BY_ALL_FALSE))
        {
            continue;
        }

        // Otherwise, mark the block.

        // If the block has a predecessor with OP_VARYING branch, it is ABA_FALSE.
        bool marked = false;
        bool hasABAFalsePred = false;
        for (pred_iterator P=pred_begin(block), PE=pred_end(block); P!=PE; ++P)
        {
            BasicBlock* predBB = *P;
            if (!rv::hasMetadata(predBB->getTerminator(), rv::RV_METADATA_OP_UNIFORM))
            {
                changed |= markValueAs(block, rv::RV_METADATA_ALWAYS_BY_ALL_FALSE);
                marked = true;
                break;
            }

            if (!rv::hasMetadata(predBB, rv::RV_METADATA_ALWAYS_BY_ALL_FALSE)) continue;
            hasABAFalsePred = true;
        }

        if (marked) continue;

        // If we are in a divergent loop, the block is ABA_FALSE.
        if (loop && !rv::hasMetadata(loop, rv::RV_METADATA_LOOP_DIVERGENT_FALSE))
        {
            changed |= markValueAs(block, rv::RV_METADATA_ALWAYS_BY_ALL_FALSE);
            continue;
        }

        // Otherwise, it is ABAON
        // - if it post dominates an ABAON block and is dominated by it, or
        // - if it has no ABA_FALSE predecessor,
        // and ABA_FALSE otherwise.
        DEBUG_VA( if (hasABAFalsePred) outs() << "  block '" << block->getName() << "' has ABA_FALSE predecessor.\n"; );
        if (!hasABAFalsePred)
        {
            changed |= markValueAs(block, rv::RV_METADATA_ALWAYS_BY_ALL_OR_NONE);
        }
        else
        {
            bool isABAONSESEExit = false;
            DomTreeNode* dtn = mDomTree.getNode(block);
            if (dtn && dtn->getIDom() &&
                rv::hasMetadata(dtn->getIDom()->getBlock(), rv::RV_METADATA_ALWAYS_BY_ALL_OR_NONE))
            {
                BasicBlock* postDominatedBB = dtn->getIDom()->getBlock();
                dtn = mPostDomTree.getNode(postDominatedBB);
                if (dtn && dtn->getIDom())
                {
                    BasicBlock* dominatedBB = dtn->getIDom()->getBlock();
                    isABAONSESEExit = dominatedBB == block;
                    DEBUG_VA( if (isABAONSESEExit) outs() << "  block '" << block->getName() << "' is exit of ABAON SESE region.\n"; );
                }
            }
            changed |= markValueAs(block,
                                   (isABAONSESEExit || !hasABAFalsePred) ?
                                       rv::RV_METADATA_ALWAYS_BY_ALL_OR_NONE :
                                       rv::RV_METADATA_ALWAYS_BY_ALL_FALSE);
        }

        if (hasABAFalsePred) continue;

        // We just marked the block ABAON, so if there is a block that post-dominates the current
        // block and is dominated by the current block, we now mark it ABAON as well unless it is
        // in a divergent loop.
        DomTreeNode* dtn = mPostDomTree.getNode(block);
        if (!dtn || !dtn->getIDom()) continue;

        BasicBlock* postDominatedBB = dtn->getIDom()->getBlock();

        Loop* pdLoop = mLoopInfo.getLoopFor(postDominatedBB);
        if (pdLoop && !rv::hasMetadata(pdLoop, rv::RV_METADATA_LOOP_DIVERGENT_FALSE)) continue;

        if (rv::hasMetadata(postDominatedBB, rv::RV_METADATA_ALWAYS_BY_ALL_TRUE) ||
            rv::hasMetadata(postDominatedBB, rv::RV_METADATA_ALWAYS_BY_ALL_FALSE))
        {
            continue;
        }

        dtn = mDomTree.getNode(postDominatedBB);
        if (!dtn || !dtn->getIDom()) continue;

        if (dtn->getIDom()->getBlock() != block) continue;

        DEBUG_VA( outs() << "  marking post dominator '" << postDominatedBB->getName() << "' as ABAON...\n"; );
        changed |= markValueAs(postDominatedBB, rv::RV_METADATA_ALWAYS_BY_ALL_OR_NONE);
        stack.push_back(postDominatedBB);
        isFollowedPostDom = true;
    }

    return changed;
}

namespace {

bool
isABAE(const BasicBlock*        block,
       const BasicBlock*        entryBB,
       const PostDominatorTree& postDomTree,
       const LoopInfo&          loopInfo)
{
    assert (block && entryBB);
    assert (block->getParent() == entryBB->getParent());

    if (!postDomTree.dominates(block, entryBB)) return false;

    // If the block is not part of a divergent loop, mark it ABA.
    // NOTE: We have to check *all* loops to which this block belongs.
    Loop* loop              = loopInfo.getLoopFor(block);
    bool  isInDivergentLoop = false;

    while (loop)
    {
        if (!loop->contains(block))
        {
            break;
        }
        if (rv::hasMetadata(loop, rv::RV_METADATA_LOOP_DIVERGENT_TRUE))
        {
            isInDivergentLoop = true;
            break;
        }
        loop = loop->getParentLoop();
    }

    return !isInDivergentLoop;
}

}

// A block b is marked as MANDATORY if
// 1. b is a direct successor of another block v that ends with a varying branch, or
// 2. b is DIVERGENT, or
// 3. b is a latch of a DIVERGENT loop, or
// 4. b is an exit block of a loop l with loop latch e. v is another block of
//    the same loop that ends with a varying branch. There exist two *disjoint*
//    paths p1 and p2 starting at the true and false edge of v, respectively,
//    that do not include the back edge of l. p1 goes from v to b while p2 goes
//    from v to e.
// NOTE: Case 4 is not fully exploited in CFG linearization.
bool
VectorizationAnalysis::isMandatory(const BasicBlock* block,
                                   const LoopInfo&   loopInfo) const
{
    assert (block);
    DEBUG_VA( outs() << "\nisMandatory(" << block->getName() << ")\n"; );

    bool mandatory = false;
#if 0 // We recompute everything anyway...
    if (rv::hasMetadata(block, rv::RV_METADATA_MANDATORY))
    {
        DEBUG_VA( outs() << "  Block '" << block->getName() << "' is MANDATORY (0)!\n"; );
        mandatory = true;
    }
#endif

    //-----------------------------------------------------------------------//
    // 1. b is a direct successor of v.
    //-----------------------------------------------------------------------//
    for (const BasicBlock* v : predecessors(block))
    {
        // The branch is varying if the terminator is not OP_UNIFORM.
        // This is a little more safe than just checking for OP_VARYING
        // although at this point there should be no OP_SEQUENTIAL and
        // no unmarked terminators.
        if (!rv::hasMetadata(v->getTerminator(),
                              rv::RV_METADATA_OP_UNIFORM))
        {
            DEBUG_VA( outs() << "  Block '" << block->getName() << "' is MANDATORY (1)!\n"; );
            DEBUG_VA( outs() << "    due to predecessor '" << v->getName() << "'.\n"; );
            mandatory = true;
        }
    }

    //-----------------------------------------------------------------------//
    // 2. b is a divergent block.
    //-----------------------------------------------------------------------//
    if (rv::hasMetadata(block, rv::RV_METADATA_DIVERGENT_TRUE))
    {
        DEBUG_VA( outs() << "  Block '" << block->getName() << "' is MANDATORY (2)!\n"; );
        DEBUG_VA( outs() << "\n"; );
        mandatory = true;
    }

    //-----------------------------------------------------------------------//
    // 3. b is a latch of a DIVERGENT loop
    //-----------------------------------------------------------------------//
    Loop* loop = loopInfo.getLoopFor(block);
    if (loop)
    {
        if (block == loop->getLoopLatch() &&
            rv::hasMetadata(loop, rv::RV_METADATA_LOOP_DIVERGENT_TRUE))
        {
            DEBUG_VA( outs() << "  Block '" << block->getName() << "' is MANDATORY (3)!\n"; );
            // Rewire targets are all blocks that cause exits to be MANDATORY.
            // Since it is easier to add this information when such an exit is found, we do it
            // there.
            mandatory = true;
        }
    }

    //-----------------------------------------------------------------------//
    // 4. b is an exit block of a loop l with loop latch e. v is another block of
    //    the same loop that ends with a varying branch. There exist two *disjoint*
    //    paths p1 and p2 starting at the true and false edge of v, respectively,
    //    that do not include the back edge of l. p1 goes from v to b while p2 goes
    //    from v to e.
    //-----------------------------------------------------------------------//
    // Check if this block is an exit block of a loop. This can never be the case
    // if it has more than one or no predecessor (thanks to loop simplification).
    const BasicBlock* exitBlockPredBB = block->getUniquePredecessor();
    if (exitBlockPredBB)
    {
        // Obviously, if the predecessor is not in a different loop than the current
        // block, this is not a loop exit.
        Loop* predLoop = loopInfo.getLoopFor(exitBlockPredBB);
        if (predLoop && predLoop != loop)
        {
            // We have to test the criterion for all nested loops that this block is an exit of.
            while (predLoop)
            {
                if (!predLoop->isLoopExiting(exitBlockPredBB)) break;
                if (isMandatoryExit(block, predLoop)) mandatory = true;
                predLoop = predLoop->getParentLoop();
            }
        }
    }

    return mandatory;
}

bool
VectorizationAnalysis::isMandatoryExit(const BasicBlock* block,
                                       const Loop*       loop) const
{
    if (mDisableControlFlowDivAnalysis)
    {
        if (rv::hasMetadata(loop, rv::RV_METADATA_LOOP_DIVERGENT_TRUE))
        {
            DEBUG_VA( outs() << "  Loop '" << block->getName()
                << "' is DIVERGENT, exit block is MANDATORY "
                << "(mDisableDivergenceAnalysis set)!\n"; );
                return true;
        }
        return false;
    }

    const BasicBlock* exitBlockPredBB = block->getUniquePredecessor();

    // If the predecessor block is the loop latch, all paths from varying branches
    // inside the loop by definition can not have disjoint paths to the latch and
    // the block, so the block can not be MANDATORY due to this criterion.
    const BasicBlock* latch = loop->getLoopLatch();
    DEBUG_VA ( if (exitBlockPredBB == latch) outs() << "    exiting block is the latch itself "
            << "- no candidate for divergence criterion (4).\n"; );
    if (exitBlockPredBB == latch) return false;

    // Check all edges in the set of the predecessor for one
    // 1. whose parent is inside the loop, and
    // 2. whose other edge reaches the latch.
    // Test case for criterion 2: TestDivergenceAnalysisLoop2.

    bool mandatory = checkForDisjointLoopPaths(block, loop, nullptr);
    DEBUG_VA( if (!mandatory) outs() << "  reachable edges do not fulfill loop exit criterion "
        << "- no divergence (4)!\n"; );

    return mandatory;
}

// Mark phis in divergent blocks as OP_VARYING / RES_VECTOR.
// Mark LCSSA phis of divergent loops as OP_UNIFORM / RES_VECTOR.
// Returns true if some mark was set, false otherwise.
bool
VectorizationAnalysis::updateUniformPhisWithDivergenceInfo(Function* scalarFn)
{
    bool changed = false;

    for (auto &BB : *scalarFn)
    {
        BasicBlock* block = &BB;

        // Ignore non-divergent and non-optional divergent-loop exit blocks.
        const bool isExitOfDivergentLoop = rv::isExitOfDivergentLoop(*block, mLoopInfo, mVecInfo);
        if (!rv::hasMetadata(block, rv::RV_METADATA_DIVERGENT_TRUE) &&
            !isExitOfDivergentLoop)
        {
            continue;
        }

        for (auto &I : BB)
        {
            Instruction* inst = &I;

            // We are only interested in phis.
            if (inst == block->getFirstInsertionPt()) break;
            assert (isa<PHINode>(inst));
            assert (!isExitOfDivergentLoop || cast<PHINode>(inst)->getNumIncomingValues() == 1);

            if (mValueInfoMap->hasMapping(*inst)) continue;

            // Ignore all phis that are already marked as VARYING/SEQUENTIAL.
            if (rv::hasMetadata(inst, rv::RV_METADATA_OP_VARYING) ||
                rv::hasMetadata(inst, rv::RV_METADATA_OP_SEQUENTIAL) ||
                rv::hasMetadata(inst, rv::RV_METADATA_OP_SEQUENTIAL_GUARDED))
            {
                continue;
            }

            DEBUG_VA( outs() << "  updating phi: " << *inst << "\n"; );

            // Mark phis in divergent blocks as OP_VARYING/RES_VECTOR.
            // Mark LCSSA phis of divergent loops as RES_VECTOR only.
            changed |= recursivelyMarkVarying(inst, isExitOfDivergentLoop ? nullptr : block);
        }
    }

    return changed;
}

// Mark operations with side-effects depending on the properties of their parent block.
// Returns true if some mark was set, false otherwise.
// This is crap: Operations with side effects must NEVER be uniform in the first place!
//               They actually should be considered as inputs if they return a value!!
bool
VectorizationAnalysis::updateUniformSideEffectOperations(Function* scalarFn)
{
    bool changed = false;

    for (auto &BB : *scalarFn)
    {
        const bool hasFullyUniformMask =
            rv::hasMetadata(&BB, rv::RV_METADATA_ALWAYS_BY_ALL_TRUE) ||
            rv::hasMetadata(&BB, rv::RV_METADATA_ALWAYS_BY_ALL_OR_NONE);

        for (auto &I : BB)
        {
            Instruction* inst = &I;

            if (mValueInfoMap->hasMapping(*inst)) continue;

            // We are only interested in operations with side effects.
            if (!rv::mayHaveSideEffects(*inst, mFunctionInfoMap)) continue;

            // Loads can be left uniform.
            if (isa<LoadInst>(inst)) continue;

            // Stores in blocks with fully uniform mask can be left uniform.
            if (hasFullyUniformMask && isa<StoreInst>(inst)) continue;

            // Ignore our own metadata calls.
            if (rv::isMetadataCall(inst)) continue;

            // Ignore all operations that are already marked as VARYING/SEQUENTIAL.
            if (rv::hasMetadata(inst, rv::RV_METADATA_OP_VARYING) ||
                rv::hasMetadata(inst, rv::RV_METADATA_OP_SEQUENTIAL) ||
                rv::hasMetadata(inst, rv::RV_METADATA_OP_SEQUENTIAL_GUARDED))
            {
                continue;
            }

            // Instruction which may have side effects is in a block that can't
            // be proven to be executed by all or no instances. Thus, it can't
            // be uniform (we mark it as OP_SEQUENTIAL/_GUARDED during split
            // analysis later).
            DEBUG_VA( outs() << "  updating operation with possible side effects: "
                << *inst << "\n"; );

            // Mark the instruction as OP_VARYING / RES_VECTOR.
            changed |= recursivelyMarkVarying(inst, &BB);
        }
    }

    return changed;
}

// Update UNIFORM/VARYING information of alloca operations.
// The marks of alloca's, in contrast to other instructions, depend upon how
// they are used. If an alloca has VARYING uses, the alloca has to be VARYING
// as well. Also, if a store or call use of an alloca is inside divergent
// control flow (i.e. its parent block is not ABA or ABAON), the alloca has to
// be VARYING.
bool
VectorizationAnalysis::updateUniformAllocas(Function* scalarFn)
{
    bool changed = false;

    for (auto &BB : *scalarFn)
    {
        BasicBlock* block = &BB;

        for (auto &I : BB)
        {
            Instruction* inst = &I;

            // We are only interested in allocas.
            if (!isa<AllocaInst>(inst)) continue;

            // Ignore all allocas that are already marked as VARYING/SEQUENTIAL.
            if (rv::hasMetadata(inst, rv::RV_METADATA_OP_VARYING) ||
                rv::hasMetadata(inst, rv::RV_METADATA_OP_SEQUENTIAL) ||
                rv::hasMetadata(inst, rv::RV_METADATA_OP_SEQUENTIAL_GUARDED))
            {
                continue;
            }

            if (mValueInfoMap->hasMapping(*inst)) continue;

            // If the alloca has a VARYING use, it is VARYING as well.
            // If the alloca has a store/call use in a block that is neither ABA
            // nor ABAON, it is VARYING.
            for (Instruction::use_iterator U=inst->use_begin(),
                 UE=inst->use_end(); U!=UE; ++U)
            {
                Instruction* useI = cast<Instruction>(U->getUser());

                if (rv::hasMetadata(useI, rv::RV_METADATA_OP_UNIFORM))
                {
                    if (!isa<StoreInst>(useI) && !isa<CallInst>(useI)) continue;

                    BasicBlock* useBB = useI->getParent();
                    if (rv::hasMetadata(useBB, rv::RV_METADATA_ALWAYS_BY_ALL_TRUE) ||
                        rv::hasMetadata(useBB, rv::RV_METADATA_ALWAYS_BY_ALL_OR_NONE))
                    {
                        continue;
                    }
                }

                DEBUG_VA( outs() << "  updating alloca: " << *inst << "\n"; );

                // Mark the alloca as OP_VARYING / RES_VECTOR.
                changed |= recursivelyMarkVarying(inst, block);

                break;
            }
        }
    }

    return changed;
}

// Mark phis in headers of divergent loops as RES_VARYING.
// Returns true if some mark was set, false otherwise.
// TODO: Don't we introduce result phis exactly for this reason?!
bool
VectorizationAnalysis::updateUniformLALBValues(Function* scalarFn)
{
    bool changed = false;

    for (auto &L : mLoopInfo)
    {
        changed |= updateUniformLALBValues(L);
    }

    return changed;
}

bool
VectorizationAnalysis::updateUniformLALBValues(Loop* loop)
{
    assert (loop);

    bool changed = false;

    for (auto &SL : *loop)
    {
        changed |= updateUniformLALBValues(SL);
    }

    if (rv::hasMetadata(loop, rv::RV_METADATA_LOOP_DIVERGENT_FALSE)) return changed;

    for (Loop::block_iterator BB=loop->block_begin(); BB!=loop->block_end(); ++BB)
    {
        BasicBlock* curBB = *BB;

        for (Instruction & inst : *curBB) {
        	auto * I = &inst;
            // Ignore instructions that are already marked as VARYING/SEQUENTIAL.
            if (rv::hasMetadata(I, rv::RV_METADATA_OP_VARYING) ||
                rv::hasMetadata(I, rv::RV_METADATA_OP_SEQUENTIAL) ||
                rv::hasMetadata(I, rv::RV_METADATA_OP_SEQUENTIAL_GUARDED))
            {
                continue;
            }
            // Ignore phis that are already marked as RES_VECTOR.
            if (isa<PHINode>(I) &&
                (rv::hasMetadata(I, rv::RV_METADATA_RES_VECTOR) ||
                 rv::hasMetadata(I, rv::RV_METADATA_RES_SCALARS)))
            {
                continue;
            }

            if (mValueInfoMap->hasMapping(*I)) continue;

            // Find a use of this instruction outside the loop.
            bool outsideUseFound = false;
            Instruction* outsideUse = nullptr;
            for (Instruction::use_iterator U=I->use_begin(),
                    UE=I->use_end(); U!=UE && !outsideUseFound; ++U)
            {
                Instruction* useI = cast<Instruction>(U->getUser());
                if (!loop->contains(useI->getParent()))
                {
                    outsideUseFound = true;
                }
#if 0
                // This is too conservative (#23).
                if (isa<PHINode>(useI) &&
                    useI->getParent() == loop->getHeader())
                {
                    outsideUseFound = true;
                }
#endif
                if (outsideUseFound)
                {
                    DEBUG_VA( outs() << "  found loop live value: " << *I << "\n"; );
                    DEBUG_VA( outs() << "    with use outside loop-boundary: " << *useI << "\n"; );
                    outsideUse = useI;
                }
            }
            if (!outsideUseFound) continue;

            // Find a phi in the header of this loop that this instruction depends on.
            // If we find such a phi, we have to expect this value to change in every
            // iteration, so it must be varying.
            PHINode* phi = findLoopPhiForInstruction(I, loop);
            if (!phi)
            {
                DEBUG_VA( outs() << "  does not depend upon loop iterations!\n"; );
                continue;
            }

            if (isa<PHINode>(outsideUse))
            {
                // Mark the phi as RES_VECTOR only.
                // TODO: Are there cases where we have to mark a phi as OP_VARYING here?
                changed |= recursivelyMarkVarying(outsideUse, nullptr);
            }
            else
            {
                // Mark the instruction as OP_VARYING / RES_VECTOR.
                changed |= recursivelyMarkVarying(outsideUse, curBB);
            }
        }
    }

    return changed;
}

PHINode*
VectorizationAnalysis::findLoopPhiForInstruction(Instruction* inst, Loop* loop)
{
    BasicBlock* parentBB = inst->getParent();
    if (!loop->contains(parentBB)) return nullptr;

    BasicBlock* headerBB = loop->getHeader();

    // If we are in the header, see if 'inst' is one of the
    // header's phi-instructions. If that is the case, we
    // have our connection. Otherwise we go on iterating over
    // operands until we have left the loop.
    if (headerBB == parentBB)
    {
        for (BasicBlock::iterator I=headerBB->begin(); I!=headerBB->end(); ++I)
        {
            if (headerBB->getFirstInsertionPt() == I) break;
            if (inst == I) return cast<PHINode>(I);
        }
    }

    // Recursively search for a phi.
    for (Instruction::op_iterator OP=inst->op_begin(); OP!=inst->op_end(); ++OP)
    {
        if (!isa<Instruction>(OP)) continue;
        Instruction*  opI = cast<Instruction>(OP);
        if (!loop->contains(opI->getParent())) continue;

        if (PHINode* phi = findLoopPhiForInstruction(opI, loop)) return phi;
    }

    return nullptr;
}


bool
VectorizationAnalysis::markDivergentLoops(const LoopInfo& loopInfo)
{
    bool changed = false;
    for (auto &L : loopInfo)
    {
        changed |= markDivergentLoop(L, loopInfo);
    }
    return changed;
}

bool
VectorizationAnalysis::markDivergentLoop(Loop* loop, const LoopInfo& loopInfo)
{
    assert (loop);

    bool changed = false;

    if (mDisableControlFlowDivAnalysis)
    {
        changed = markValueAs(loop->getHeader(), rv::RV_METADATA_LOOP_DIVERGENT_TRUE);

        // We have to mark all exit branch operations as OP_VARYING.
        SmallVector<BasicBlock*, 2> exitingBlocks;
        loop->getExitingBlocks(exitingBlocks);

        for (auto &BB : exitingBlocks)
        {
            TerminatorInst* terminator = BB->getTerminator();
            markValueAs(terminator, rv::RV_METADATA_OP_VARYING);
        }

        for (auto &SL : *loop)
        {
            changed |= markDivergentLoop(SL, loopInfo);
        }
    }
    else
    {
        changed = markValueAs(loop->getHeader(),
                              isLoopDivergent(loop, loopInfo) ?
                                  rv::RV_METADATA_LOOP_DIVERGENT_TRUE :
                                  rv::RV_METADATA_LOOP_DIVERGENT_FALSE);

        for (auto &SL : *loop)
        {
            changed |= markDivergentLoop(SL, loopInfo);
        }
    }

    return changed;
}

// A loop is divergent if there is a block in the loop that ends with a
// varying branch and is not strictly post-dominated by another block that
// belongs to the same loop.
bool
VectorizationAnalysis::isLoopDivergent(Loop* loop, const LoopInfo& loopInfo)
{
    for (auto &it : loop->getBlocks())
    {
        BasicBlock* block = it;

        const TerminatorInst* terminator = block->getTerminator();
        if (rv::hasMetadata(terminator, rv::RV_METADATA_OP_UNIFORM)) continue;

        assert (mPostDomTree.getNode(block));
        assert (mPostDomTree.getNode(block)->getIDom());
        const BasicBlock* postDom = mPostDomTree.getNode(block)->getIDom()->getBlock();
        assert (postDom);
        if (loop->contains(postDom)) continue;

        // The block ends with a varying branch and is not strictly post-dominated by
        // the latch -> the loop is divergent.
        return true;
    }

    // All blocks either end with a uniform branch or are strictly post-dominated by
    // the loop latch, so the loop is non-divergent.
    return false;
}

// We have to be sure to create loop exit masks for every nested divergent
// loop. Therefore, we first mark all those loops that are divergent
// and "top-level", meaning they are not nested or nested directly
// inside a non-divergent loop.
// EXAMPLE: divergent loop inside non-divergent loop inside divergent loop.
void
VectorizationAnalysis::markNestedDivergentTopLevelLoops()
{
    for (Loop* loop : mLoopInfo)
    {
        if (rv::hasMetadata(loop, rv::RV_METADATA_LOOP_DIVERGENT_TRUE))
        {
            markValueAs(loop->getHeader(), rv::RV_METADATA_LOOP_TOP_LEVEL_DIVERGENT);
            if (loop->getSubLoops().empty())
            {
                markValueAs(loop->getHeader(), rv::RV_METADATA_LOOP_INNERMOST_DIVERGENT);
            }
        }
        markNestedDivergentTopLevelLoops(loop);
    }
}

// We must not generate mask operations for sub loops before their parents,
// so we only mark the "top-level" divergent loops (= at uniform loops, add
// all divergent sub loops, at divergent loops, do not add any direct sub loop but
// continue recursion into them).
// NOTE: Should only be called through markNestedDivergentTopLevelLoops().
void
VectorizationAnalysis::markNestedDivergentTopLevelLoops(Loop* loop)
{
    const bool loopIsUniform =
        rv::hasMetadata(loop, rv::RV_METADATA_LOOP_DIVERGENT_FALSE);

    bool allSubLoopsUniform = true;
    for (Loop::iterator SL=loop->begin(); SL != loop->end(); ++SL)
    {
        Loop* subLoop = *SL;
        const bool subLoopIsUniform =
            rv::hasMetadata(subLoop, rv::RV_METADATA_LOOP_DIVERGENT_FALSE);

        if (loopIsUniform && !subLoopIsUniform)
        {
            // DIVERGENT inner loop inside NON_DIVERGENT outer loop
            // -> mark inner loop as "top level"
            markValueAs(subLoop->getHeader(),
                        rv::RV_METADATA_LOOP_TOP_LEVEL_DIVERGENT);
            allSubLoopsUniform = false;
        }
        markNestedDivergentTopLevelLoops(subLoop);
    }

    if (!loopIsUniform && allSubLoopsUniform)
    {
        // Only UNIFORM inner loops inside DIVERGENT outer loop.
        // -> Mark outer loop as "innermost".
        // TODO: This is conservative, but can we gain something from
        //       correctly handling mixed non-divergent/divergent inner loops
        //       (same nesting level)?
        markValueAs(loop->getHeader(), rv::RV_METADATA_LOOP_INNERMOST_DIVERGENT);
    }
}

////////////////////////////////////////////////////////////////////////////
//                       INDEX & ALIASING ANALYSIS                        //
////////////////////////////////////////////////////////////////////////////

// IMPORTANT: Index and alignment info always refer to the result of the
//            operation! This means that if we want to know whether we can
//            use a vector load instead of a gather, we have to look at the
//            indices of the GEP instruction if there is one.

// Marking of same/consecutive/random and aligned/unaligned works differently
// than uniform/varying: We cannot simply mark everything and then remove
// marks but rather have to only mark what we can prove.
// Therefore, we start by marking uniform values as SAME and user-defined
// values according to the input.
// Only then we start recursively marking the rest of the function.
void
VectorizationAnalysis::analyzeConsecutiveAlignedInfo(Function* scalarFn)
{
    SmallPtrSet<Value*, 64> markedValues;

    // Add all instructions that were marked from outside.
    for (llvm::inst_iterator I=inst_begin(scalarFn), IE=inst_end(scalarFn); I!=IE; ++I)
    {
        Instruction* inst = &*I;
        if (mValueInfoMap->hasMapping(*inst)) markedValues.insert(inst);
    }

    if (mDisableMemAccessAnalysis)
    {
        for (llvm::inst_iterator I=inst_begin(scalarFn), IE=inst_end(scalarFn); I!=IE; ++I)
        {
            Instruction* inst = &*I;
            if (inst->getType()->isVoidTy()) continue;
            if (mValueInfoMap->hasMapping(*inst)) continue;
            if (rv::hasMetadata(inst, rv::RV_METADATA_OP_UNIFORM)) // TODO: Shouldn't this test for RES_UNIFORM?
            {
                rv::setMetadata(inst, rv::RV_METADATA_INDEX_SAME);
            }
            else
            {
                rv::setMetadata(inst, rv::RV_METADATA_INDEX_RANDOM);
            }
            rv::setMetadata(inst, rv::RV_METADATA_ALIGNED_FALSE);
        }
        return;
    }

    // Mark all arguments.
    // Mark arguments that were marked from outside accordingly.
    // Mark uniform arguments that are pointers as INDEX_SAME / ALIGN_TRUE.
    // Mark varying pointer arguments as INDEX_CONSECUTIVE / ALIGN_TRUE.
    // These are requirements for the caller of the vectorized function.
    DEBUG_VA( outs() << "\nmarking index/alignment info of arguments...\n"; );
    for (auto & arg : scalarFn->getArgumentList()) {
    	Argument * A = &arg;
        if (mValueInfoMap->hasMapping(*A))
        {
            assert (rv::hasMetadata(A, rv::RV_METADATA_INDEX_SAME) ||
                    rv::hasMetadata(A, rv::RV_METADATA_INDEX_CONSECUTIVE) ||
                    rv::hasMetadata(A, rv::RV_METADATA_INDEX_RANDOM));
            assert (rv::hasMetadata(A, rv::RV_METADATA_ALIGNED_TRUE) ||
                    rv::hasMetadata(A, rv::RV_METADATA_ALIGNED_FALSE));
            markedValues.insert(A);
            DEBUG_VA( outs() << "argument already marked from outside: " << *A << "\n"; );
            continue;
        }

        if (rv::hasMetadata(A, rv::RV_METADATA_RES_UNIFORM))
        {
            rv::setMetadata(A, rv::RV_METADATA_INDEX_SAME);
            rv::setMetadata(A,
                             A->getType()->isPointerTy() ?
                                 rv::RV_METADATA_ALIGNED_TRUE :
                                 rv::RV_METADATA_ALIGNED_FALSE);
            DEBUG_VA(
                outs() << "marked UNIFORM argument: " << *A << " as "
                    << "INDEX_SAME / "
                    << (A->getType()->isPointerTy() ?
                        "ALIGNED_TRUE" : "ALIGNED_FALSE") << "!\n";
            );
        }
        else
        {
            rv::setMetadata(A,
                             A->getType()->isPointerTy() ?
                                 rv::RV_METADATA_INDEX_CONSECUTIVE :
                                 rv::RV_METADATA_INDEX_RANDOM);
            rv::setMetadata(A,
                             A->getType()->isPointerTy() ?
                                 rv::RV_METADATA_ALIGNED_TRUE :
                                 rv::RV_METADATA_ALIGNED_FALSE);

            DEBUG_VA(
                outs() << "marked VARYING argument: " << *A << " as "
                    << (A->getType()->isPointerTy() ?
                    "INDEX_CONSECUTIVE / ALIGNED_TRUE!\n" :
                    "INDEX_RANDOM / ALIGNED_FALSE!\n");
            );
        }
        markedValues.insert(A);
    }

    //
    // Collect all "outputs" of the function and conditional branches
    // as starting points for a post-reversed DFS.
    //

    SmallPtrSet<Instruction*, 64> workSet;

    // If the function returns something, all returns are outputs :).
    // Otherwise, ignore.
    if (!scalarFn->getFunctionType()->getReturnType()->isVoidTy())
    {
        SmallPtrSet<BasicBlock*, 2> returnBlocks;
        rv::findReturnBlocks(*scalarFn, returnBlocks);
        for (auto BB : returnBlocks)
        {
            workSet.insert(BB->getTerminator());
        }
    }

    // All calls and stores have to count as outputs.
    // All conditional branches also have to be added to workSet because
    // they depend on other values that have to be marked.
    // NOTE: Adding calls is not enough: they might have been added by the
    //       user and thus are marked already. We have to add their operands.
    for (auto & BB : *scalarFn) {
        for (auto & inst : BB) {
        	auto * I = &inst;
            if (!isa<CallInst>(I) &&
                !isa<StoreInst>(I) &&
                !isa<BranchInst>(I))
            {
                continue;
            }

            // Ignore our own metadata calls.
            if (rv::isMetadataCall(I)) continue;

            if (BranchInst* br = dyn_cast <BranchInst>(I))
            {
                if (br->isUnconditional())
                {
                    // ignore, but mark as INDEX_SAME / ALIGN_FALSE
                    //rv::setMetadata(br, rv::RV_METADATA_INDEX_SAME);
                    //rv::setMetadata(br, rv::RV_METADATA_ALIGNED_FALSE);
                    //markedValues.insert(br);
                    continue;
                }
            }

            if (markedValues.count(I))
            {
                // Value is already marked.
                // Add operands to be sure that we mark everything.
                for (Instruction::op_iterator O=I->op_begin(), OE=I->op_end(); O!=OE; ++O)
                {
                    if (!isa<Instruction>(O)) continue;
                    Instruction* opI = cast<Instruction>(O);
                    if (markedValues.count(opI)) continue;
                    workSet.insert(opI);
                }
                continue;
            }

            // Ignore instructions already marked via API calls.
            // Note that checking this here still allows to add the operands.
            if (mValueInfoMap->hasMapping(*I)) continue;

            workSet.insert(I);
        }
    }


    //
    // Now mark all other instructions according to their dependencies.
    //

    DEBUG_VA(
        outs() << "\nworkSet:\n";
        for (auto it : workSet)
        {
            outs() << " * " << *it << "\n";
        }
    );

    DEBUG_VA( outs() << "\nmarking instructions...\n"; );
    for (auto it : workSet)
    {
        Instruction* inst = it;
        const char* ii = nullptr;
        const char* ai = nullptr;
        markIndexAlignValueAndOps(inst, markedValues, &ii, &ai);

        // TODO: Move these checks to a verification function that tests
        //       *all* instructions.
        assert ((inst->getType()->isVoidTy() ||
                !rv::hasMetadata(inst, rv::RV_METADATA_INDEX_SAME) ||
                rv::hasMetadata(inst, rv::RV_METADATA_RES_UNIFORM)) &&
                "if value is INDEX_SAME it must be RES_UNIFORM!");
        assert ((inst->getType()->isVoidTy() ||
                !rv::hasMetadata(inst, rv::RV_METADATA_RES_UNIFORM) ||
                rv::hasMetadata(inst, rv::RV_METADATA_INDEX_SAME)) &&
                "if value is RES_UNIFORM it must be INDEX_SAME!");

        markedValues.insert(inst);
    }
}

void
VectorizationAnalysis::markIndexAlignValueAndOps(Value*                   value,
                                                 SmallPtrSet<Value*, 64>& markedValues,
                                                 const char**             indexInfo,
                                                 const char**             alignInfo)
{
    assert (value);
    assert (isa<Instruction>(value) ||
            isa<Argument>(value) ||
            isa<Constant>(value));

    DEBUG_VA( outs() << "markIndexAlignValueAndOps(1): " << *value << "\n"; );

    // Handle values that were already marked.
    if (markedValues.count(value))
    {
        getIndexAlignedInfo(value, indexInfo, alignInfo);
        if (strcmp(*alignInfo, "") == 0) { // no alignInfo specified yet
        	rv::setMetadata(value, rv::RV_METADATA_ALIGNED_FALSE);
        	*alignInfo = rv::RV_METADATA_ALIGNED_FALSE;
        }
        DEBUG_VA( outs() << "  already marked as " << *indexInfo << " / " << *alignInfo
                << " - ignored!\n"; );
        return;
    }

    assert (!isa<Argument>(value) &&
            "all arguments have to be marked already!");

    if (Constant* c = dyn_cast<Constant>(value))
    {
        *indexInfo = deriveIndexInfo(c);
        *alignInfo = deriveAlignedInformation(c);
        return;
    }

    // If this is a loop PHI, make sure to recurse into the predecessor
    // outside of the loop first to break cycles. The phi is then marked
    // according to this predecessor.
    if (isa<PHINode>(value) &&
        mLoopInfo.isLoopHeader(cast<PHINode>(value)->getParent()))
    {
        PHINode* phi = cast<PHINode>(value);
        assert (phi->getNumIncomingValues() == 2);
        Loop* loop = mLoopInfo.getLoopFor(phi->getParent());

        // Mark predecessor of incoming value from outside loop
        BasicBlock* preheaderBB = loop->getLoopPreheader();
        Value* preheaderVal = phi->getIncomingValueForBlock(preheaderBB);

        const char* phIndex = nullptr;
        const char* phAlign = nullptr;
        markIndexAlignValueAndOps(preheaderVal, markedValues, &phIndex, &phAlign);
        DEBUG_VA( outs() << "markIndexAlignValueAndOps(2): " << *phi << "\n"; );
        assert (strcmp(phIndex, "") != 0);
        assert (strcmp(phAlign, "") != 0);

        // Mark the phi according to this predecessor, unless the phi is
        // known to be VARYING and the predecessor is INDEX_SAME - this
        // violates an important assumption, so we do not set a wrong mark
        // here to prevent this in the first place.
        // NOTE: Unfortunately, marking the phi as INDEX_RANDOM  might
        //       introduce some imprecision in cases where the other
        //       incoming value is INDEX_CONSECUTIVE.
        rv::setMetadata(phi, phIndex);
        rv::setMetadata(phi, phAlign);
        DEBUG_VA( outs() << "  marked loop phi: " << *phi << "\n"; );

        markedValues.insert(phi);

        // Set indexInfo/alignInfo for fixpoint iteration.
        *indexInfo = phIndex;
        *alignInfo = phAlign;

        // Now recurse into other operand (back edge).
        bool changed = true;
        while (changed)
        {
            SmallPtrSet<Value*, 64> markedLoopValues(markedValues);

            const int preheaderIdx = phi->getBasicBlockIndex(preheaderBB);
            const int backedgeIdx = preheaderIdx == 0 ? 1 : 0;
            Value* backedgeVal = phi->getIncomingValue(backedgeIdx);

            const char* beIndex = nullptr;
            const char* beAlign = nullptr;
            markIndexAlignValueAndOps(backedgeVal, markedLoopValues, &beIndex, &beAlign);
            DEBUG_VA( outs() << "markIndexAlignValueAndOps(3): " << *phi << "\n"; );
            assert (strcmp(beIndex, "") != 0);
            assert (strcmp(beAlign, "") != 0);

            // If necessary, update marks of phi (if backedge-marks differ from
            // preheader-marks).
            std::vector<const char*> iiVec;
            std::vector<const char*> aiVec;
            iiVec.push_back(beIndex);
            iiVec.push_back(phIndex);
            aiVec.push_back(beAlign);
            aiVec.push_back(phAlign);

            const char* ii = deriveIndexInfo(phi, iiVec);
            const char* ai = deriveAlignmentInfo(phi, aiVec);
            rv::setMetadata(phi, ii);
            rv::setMetadata(phi, ai);
            DEBUG_VA( outs() << "  updated loop phi: " << *phi << "\n"; );

            // If we did update the phi, we have to recurse again
            changed = (*indexInfo && *alignInfo) ?
                (strcmp(ii, *indexInfo) || strcmp(ai, *alignInfo)) :
                false;

            *indexInfo = ii;
            *alignInfo = ai;

            if (!changed ||
                (ii == rv::RV_METADATA_INDEX_RANDOM && ai == rv::RV_METADATA_ALIGNED_FALSE))
            {
                markedValues.insert(markedLoopValues.begin(), markedLoopValues.end());
            }
        }

        return;
    }

    // Arguments have to be marked already, same for constants now.
    assert (isa<Instruction>(value));
    Instruction* valI = cast<Instruction>(value);

    // Collect info of operands.
    std::vector<const char*> aiVec;
    std::vector<const char*> iiVec;

    for (Instruction::op_iterator O=valI->op_begin(), OE=valI->op_end(); O!=OE; ++O)
    {
        Value* opVal = *O;
        if (isa<BasicBlock>(opVal)) continue; // handle phis correctly
        if (isa<Function>(opVal)) continue; // handle calls correctly

        const char* opIndex = nullptr;
        const char* opAlign = nullptr;
        markIndexAlignValueAndOps(opVal, markedValues, &opIndex, &opAlign);
        assert (strcmp(opIndex, "") != 0);
        assert (strcmp(opAlign, "") != 0);

        iiVec.push_back(opIndex);
        aiVec.push_back(opAlign);
    }
    DEBUG_VA( outs() << "markIndexAlignValueAndOps(4): " << *valI << "\n"; );

    // If this is an operation without return value, ignore it.
    // Note that we can only stop after marking all operands.
    if (value->getType()->isVoidTy())
    {
        DEBUG_VA( outs() << "  has void type - ignored!\n"; );
        return;
    }

    // If this is a call or a non-uniform load, we don't know anything about
    // its return value if it was not marked by user.
    // If the return type of a non-uniform instruction can not be vectorized,
    // we don't know anything about it unless it was marked by user already.
    // Non-uniform non-vectorizable instructions can not be INDEX_CONSECUTIVE.
    const bool isCall = isa<CallInst>(valI);
    const bool isLoad = isa<LoadInst>(valI);
    const bool isCast = isa<CastInst>(valI);
    const bool isGEP  = isa<GetElementPtrInst>(valI);
    const bool isUniform = rv::hasMetadata(valI, rv::RV_METADATA_OP_UNIFORM);
    const bool isVectorizable = rv::isVectorizableInst(*valI);

    const bool noInfo = !isUniform &&
        (isCall || isLoad || (!isCast && !isGEP && !isVectorizable));

    if (noInfo)
    {
        assert (rv::hasMetadata(valI, rv::RV_METADATA_OP_VARYING) ||
                rv::hasMetadata(valI, rv::RV_METADATA_OP_SEQUENTIAL) ||
                rv::hasMetadata(valI, rv::RV_METADATA_OP_SEQUENTIAL_GUARDED));
        assert (!rv::hasMetadata(valI, rv::RV_METADATA_RES_UNIFORM));

        rv::setMetadata(valI, rv::RV_METADATA_INDEX_RANDOM);
        rv::setMetadata(valI, rv::RV_METADATA_ALIGNED_FALSE);
        DEBUG_VA( outs() << "  marked instruction as INDEX_RANDOM / ALIGN_FALSE!\n"; );

        *indexInfo = rv::RV_METADATA_INDEX_RANDOM;
        *alignInfo = rv::RV_METADATA_ALIGNED_FALSE;

        DEBUG_VA( outs() << "  marked value: " << *valI << "\n"; );
        markedValues.insert(valI);

        return;
    }

    // Derive alignment and index info depending on instruction and marks of
    // operands.
    const char* ii = deriveIndexInfo(valI, iiVec);
    const char* ai = deriveAlignmentInfo(valI, aiVec);
    rv::setMetadata(valI, ii);
    rv::setMetadata(valI, ai);

    DEBUG_VA( outs() << "  marked value: " << *valI << "\n"; );

    markedValues.insert(valI);

    assert (strcmp(ii, "") != 0);
    assert (strcmp(ai, "") != 0);

    *indexInfo = ii;
    *alignInfo = ai;

    return;
}

void
VectorizationAnalysis::getIndexAlignedInfo(Value*       value,
                                           const char** indexInfo,
                                           const char** alignInfo) const
{
    assert (value);
    assert (isa<Instruction>(value) ||
            isa<Argument>(value));

    *indexInfo = rv::hasMetadata(value, rv::RV_METADATA_INDEX_SAME) ?
        rv::RV_METADATA_INDEX_SAME :
        rv::hasMetadata(value, rv::RV_METADATA_INDEX_CONSECUTIVE) ?
            rv::RV_METADATA_INDEX_CONSECUTIVE :
            rv::hasMetadata(value, rv::RV_METADATA_INDEX_RANDOM) ?
                rv::RV_METADATA_INDEX_RANDOM :
                rv::hasMetadata(value, rv::RV_METADATA_INDEX_STRIDED) ?
                    rv::RV_METADATA_INDEX_STRIDED :
                    rv::hasMetadata(value, rv::RV_METADATA_INDEX_SHUFFLE) ?
                        rv::RV_METADATA_INDEX_SHUFFLE :
                        "";

    *alignInfo = rv::hasMetadata(value, rv::RV_METADATA_ALIGNED_TRUE) ?
        rv::RV_METADATA_ALIGNED_TRUE :
        rv::hasMetadata(value, rv::RV_METADATA_ALIGNED_FALSE) ?
            rv::RV_METADATA_ALIGNED_FALSE :
            "";

    return;
}

// Derive index info depending on the instruction and operands marks
const char*
VectorizationAnalysis::deriveIndexInfo(Instruction* inst,
                                       const std::vector<const char*>& iiVec) const
{
    assert (inst);

    const char* indexInfo = nullptr;
    const bool isSub = inst->getOpcode() == Instruction::Sub  || inst->getOpcode() == Instruction::FSub;

    switch (inst->getOpcode())
    {
        case Instruction::Add:
        case Instruction::FAdd:
        case Instruction::Sub:
        case Instruction::FSub:
        {
            assert (iiVec.size() == 2);
            // Adding/subtracting two "same" indices yields a "same" index.
            if (strcmp(iiVec[0], rv::RV_METADATA_INDEX_SAME) == 0 &&
                strcmp(iiVec[1], rv::RV_METADATA_INDEX_SAME) == 0)
            {
                indexInfo = rv::RV_METADATA_INDEX_SAME;
                break;
            }
            if (strcmp(iiVec[0], rv::RV_METADATA_INDEX_CONSECUTIVE) == 0 &&
                strcmp(iiVec[1], rv::RV_METADATA_INDEX_CONSECUTIVE) == 0)
            {
                if (inst->getOpcode() == Instruction::Sub ||
                    inst->getOpcode() == Instruction::FSub)
                {
                    // Subtracting two consecutive indices yields a "same" index.
                    indexInfo = rv::RV_METADATA_INDEX_SAME;
                    break;
                }
                else
                {
                    // Adding two consecutive indices yields a strided index.
                    // TODO: do something useful with this info :)
                    //indexInfo = INDEX_STRIDED;
                    indexInfo = rv::RV_METADATA_INDEX_RANDOM;
                    break;
                }
            }

             // Adding/subtracting a consecutive and a "same" index yields a consecutive index.
             if ((strcmp(iiVec[0], rv::RV_METADATA_INDEX_CONSECUTIVE) == 0 &&
                  strcmp(iiVec[1], rv::RV_METADATA_INDEX_SAME) == 0) ||
                (!isSub && strcmp(iiVec[1], rv::RV_METADATA_INDEX_CONSECUTIVE) == 0 &&
                           strcmp(iiVec[0], rv::RV_METADATA_INDEX_SAME) == 0))
            {
                indexInfo = rv::RV_METADATA_INDEX_CONSECUTIVE;
                break;
            }
            indexInfo = rv::RV_METADATA_INDEX_RANDOM;
            break;
        }

        case Instruction::GetElementPtr:
        {
            // If any operand is "random", return "random".
            // If all operands are "same", return "same".
            // If one index is "consecutive" while all others are "same", return "consecutive".
            // Otherwise, return INDEX_RANDOM.
            GetElementPtrInst* gep = cast<GetElementPtrInst>(inst);


            DEBUG_VA( errs() << "\nAnalyzing " << *gep << "\n"; );
            // the basePtr is SOA
            Value * ptrOperand = gep->getPointerOperand();

            bool flatBasePtr = rv::IsFlatPointer(*ptrOperand->getType());
            DEBUG_VA( errs() <<   "Base flat : " << flatBasePtr << "\n"; );

            bool flatTargetPtr = rv::IsFlatPointer(*gep->getType());
            DEBUG_VA( errs() << "Target flat : " << flatTargetPtr << "    " << *gep->getType() << "\n"; );

            bool isSOA = !flatBasePtr && strcmp(iiVec[0], rv::RV_METADATA_INDEX_CONSECUTIVE) == 0;
            DEBUG_VA( errs() << "      isSOA : " << isSOA << "\n"; );

            bool lastConsecutive = false;


			bool     random      = false;
			bool     allSame     = true;
			unsigned consecFound = 0;
			for (unsigned i=0, e=gep->getNumOperands(); i!=e; ++i)
			{
				DEBUG_VA( errs() << "ii[" << i << "] " <<iiVec[i] << "\n"; );

				lastConsecutive = false;
				if (strcmp(iiVec[i], rv::RV_METADATA_INDEX_CONSECUTIVE) == 0)
				{
					++consecFound;
					allSame = false;
					lastConsecutive = true;
					continue;
				}

				if (strcmp(iiVec[i], rv::RV_METADATA_INDEX_SAME) != 0)
				{
					random = true;
					break;
				}
			}

			if (! random) {
				if (allSame) {
					DEBUG_VA( errs() << "-> Uniform GEP\n"; );
					indexInfo = rv::RV_METADATA_INDEX_SAME; break;
				} else if (consecFound == 1 && isSOA) {
					DEBUG_VA( errs() << "-> Consecutive SOA GEP\n"; );
					indexInfo = rv::RV_METADATA_INDEX_CONSECUTIVE; break;
				} // SOA base pointer with uniform accesses remains SOA
				else if (consecFound == 1 && lastConsecutive && flatTargetPtr) {
					DEBUG_VA( errs() << "-> Consecutive flat GEP\n"; );
					indexInfo = rv::RV_METADATA_INDEX_CONSECUTIVE; break;
				} // UNIFORM base pointer, last index is consecutive on a flat pointer type
			}

			// default behavior
			DEBUG_VA( errs() << "-> Varying GEP\n"; );
			rv::setMetadata(inst, rv::RV_METADATA_RES_SCALARS); // FIXME RES_VECTOR if pointers fit into vector components
			indexInfo = rv::RV_METADATA_INDEX_RANDOM;
			break;
        }

        case Instruction::Mul:
        case Instruction::FMul:
        {
            assert (iiVec.size() == 2);
            // Multiplying two "same" indices yields a "same" index.
            if (strcmp(iiVec[0], rv::RV_METADATA_INDEX_SAME) == 0 &&
                strcmp(iiVec[1], rv::RV_METADATA_INDEX_SAME) == 0)
            {
                indexInfo = rv::RV_METADATA_INDEX_SAME;
                break;
            }
            // Multiplying a "same" index with a consecutive one yields a strided index.
            if ((strcmp(iiVec[0], rv::RV_METADATA_INDEX_CONSECUTIVE) == 0 &&
                 strcmp(iiVec[1], rv::RV_METADATA_INDEX_SAME) == 0) ||
                (strcmp(iiVec[1], rv::RV_METADATA_INDEX_CONSECUTIVE) == 0 &&
                 strcmp(iiVec[0], rv::RV_METADATA_INDEX_SAME) == 0))
            {
                // TODO: do something useful with this info :)
                //indexInfo = INDEX_STRIDED;
                indexInfo = rv::RV_METADATA_INDEX_RANDOM;
                break;
            }
            indexInfo = rv::RV_METADATA_INDEX_RANDOM;
            break;
        }

        // These instructions retain "same" if operands are all "same",
        // nothing else.
        case Instruction::UDiv:
        case Instruction::SDiv:
        case Instruction::URem:
        case Instruction::SRem:
        case Instruction::And:
        case Instruction::Or:
        case Instruction::Xor:
        case Instruction::Shl:
        case Instruction::LShr:
        case Instruction::AShr:
        case Instruction::Call:
        case Instruction::ICmp:
        case Instruction::FCmp:
        {
            for (const auto &it : iiVec)
            {
                if (strcmp(it, rv::RV_METADATA_INDEX_SAME) != 0)
                {
                    indexInfo = rv::RV_METADATA_INDEX_RANDOM;
                    break;
                }
            }
            indexInfo = rv::RV_METADATA_INDEX_SAME;
            break;
        }

        // These instructions retain any information if all operands have
        // the same mark except for casts to void pointer type.
        // TODO: This is not true! If casting consecutive/aligned i32* to i16*,
        //       the access becomes strided! Likewise, if casting i32* to i64*,
        //       the access becomes possibly unaligned.
        case Instruction::Trunc:
        case Instruction::SExt:
        case Instruction::FPTrunc:
        case Instruction::FPExt:
        case Instruction::ZExt:
        case Instruction::FPToUI:
        case Instruction::FPToSI:
        case Instruction::UIToFP:
        case Instruction::SIToFP:
        case Instruction::IntToPtr:
        case Instruction::PtrToInt:
        case Instruction::BitCast:
        {
            if (rv::returnsVoidPtr(*inst))
            {
                indexInfo = rv::RV_METADATA_INDEX_RANDOM;
                break;
            }

            bool allSame = true;
            bool allConsecutive = true;
            bool allStrided = true;
            for (unsigned i=0, e=iiVec.size(); i!=e; ++i)
            {
                allSame &= strcmp(iiVec[i], rv::RV_METADATA_INDEX_SAME) == 0;
                allConsecutive &= strcmp(iiVec[i], rv::RV_METADATA_INDEX_CONSECUTIVE) == 0;
                allStrided &= strcmp(iiVec[i], rv::RV_METADATA_INDEX_STRIDED) == 0;
            }
            assert (!(allSame && allConsecutive));
            assert (!(allSame && allStrided));
            assert (!(allStrided && allConsecutive));

            if (allSame) indexInfo = rv::RV_METADATA_INDEX_SAME;
            // else if (allConsecutive) indexInfo = rv::RV_METADATA_INDEX_CONSECUTIVE;
            else if (allStrided) indexInfo = rv::RV_METADATA_INDEX_STRIDED;
            else indexInfo = rv::RV_METADATA_INDEX_RANDOM;
            errs() << *inst << " : " << allSame << " "  << allConsecutive << " " << allStrided << "\n";
            // abort();
            break;
        }

        case Instruction::Store:
        {
            for (const auto &it : iiVec)
            {
                if (strcmp(it, rv::RV_METADATA_INDEX_SAME) != 0)
                {
                    indexInfo = rv::RV_METADATA_INDEX_RANDOM;
                    break;
                }
            }
            indexInfo = rv::RV_METADATA_INDEX_SAME;
            break;
        }

        case Instruction::Load:
        {
            LoadInst* load = cast<LoadInst>(inst);

            // Special case:
            // If we load from a pointer-to-pointer that is marked
            // INDEX_CONSECUTIVE, the result is still INDEX_CONSECUTIVE.
            // TODO: Unsure if this is safe :)
            Value* pointer = load->getPointerOperand();
            if (pointer->getType()->getContainedType(0)->isPointerTy())
            {
                if (isa<Instruction>(pointer) || isa<Argument>(pointer))
                {
                    const char* ii, *ai;
                    getIndexAlignedInfo(pointer, &ii, &ai);
                    if (strcmp(ii, rv::RV_METADATA_INDEX_CONSECUTIVE) == 0)
                    {
                        indexInfo = rv::RV_METADATA_INDEX_CONSECUTIVE;
                        break;
                    }
                }
            }

            const unsigned ptrIdx = load->getPointerOperandIndex();
            if (strcmp(iiVec[ptrIdx], rv::RV_METADATA_INDEX_SAME) == 0)
            {
                indexInfo = rv::RV_METADATA_INDEX_SAME;
                break;
            }
            indexInfo = rv::RV_METADATA_INDEX_RANDOM;
            break;
        }

        case Instruction::PHI:
        {
            PHINode* phi = cast<PHINode>(inst);
            bool allSame = true;
            bool allConsecutive = true;
            bool allStrided = true;
            for (unsigned i=0, e=phi->getNumIncomingValues(); i!=e; ++i)
            {
                // We ignored basic blocks while collecting indexInfo,
                // so now we can directly index iiVec per incoming value.
                allSame &= strcmp(iiVec[i], rv::RV_METADATA_INDEX_SAME) == 0;
                allConsecutive &= strcmp(iiVec[i], rv::RV_METADATA_INDEX_CONSECUTIVE) == 0;
                allStrided &= strcmp(iiVec[i], rv::RV_METADATA_INDEX_STRIDED) == 0;
            }
            assert (!(allSame && allConsecutive));
            assert (!(allSame && allStrided));
            assert (!(allStrided && allConsecutive));

            // If the phi is OP_VARYING (e.g. due to divergent control flow),
            // it will be transformed into a select which - even if both operands are
            // SAME - can not be proven to be SAME as well.
            if (rv::hasMetadata(phi, rv::RV_METADATA_OP_VARYING))
            {
                assert (rv::isExitOfDivergentLoop(*phi->getParent(), mLoopInfo, mVecInfo) ||
                        !rv::hasMetadata(phi->getParent(), rv::RV_METADATA_DIVERGENT_FALSE));
                indexInfo = rv::RV_METADATA_INDEX_RANDOM;
                break;
            }

            if (allSame) indexInfo = rv::RV_METADATA_INDEX_SAME;
            else if (allConsecutive) indexInfo = rv::RV_METADATA_INDEX_CONSECUTIVE;
            else if (allStrided) indexInfo = rv::RV_METADATA_INDEX_STRIDED;
            else indexInfo = rv::RV_METADATA_INDEX_RANDOM;
            break;
        }

        case Instruction::Select:
        {
            assert (iiVec.size() == 3);

            // If the condition is not UNIFORM, we cannot say anything
            // about the result.
            if (strcmp(iiVec[0], rv::RV_METADATA_INDEX_SAME) != 0)
            {
                assert ((isa<Constant>(cast<SelectInst>(inst)->getCondition()) ||
                         !rv::hasMetadata(cast<SelectInst>(inst)->getCondition(), rv::RV_METADATA_RES_UNIFORM)) &&
                        "condition must not be RES_UNIFORM!");
                indexInfo = rv::RV_METADATA_INDEX_RANDOM;
                break;
            }

            assert ((isa<Constant>(cast<SelectInst>(inst)->getCondition()) ||
                     rv::hasMetadata(cast<SelectInst>(inst)->getCondition(), rv::RV_METADATA_RES_UNIFORM)) &&
                    "condition must be RES_UNIFORM!");

            // If the condition is INDEX_SAME, we know that the result
            // has a certain property if both incoming values have it.
            bool allSame = strcmp(iiVec[1], rv::RV_METADATA_INDEX_SAME) == 0 &&
            strcmp(iiVec[2], rv::RV_METADATA_INDEX_SAME) == 0;
            bool allConsecutive = strcmp(iiVec[1], rv::RV_METADATA_INDEX_CONSECUTIVE) == 0 &&
            strcmp(iiVec[2], rv::RV_METADATA_INDEX_CONSECUTIVE) == 0;
            bool allStrided = strcmp(iiVec[1], rv::RV_METADATA_INDEX_STRIDED) == 0 &&
            strcmp(iiVec[2], rv::RV_METADATA_INDEX_STRIDED) == 0;

            if (allSame) indexInfo = rv::RV_METADATA_INDEX_SAME;
            else if (allConsecutive) indexInfo = rv::RV_METADATA_INDEX_CONSECUTIVE;
            else if (allStrided) indexInfo = rv::RV_METADATA_INDEX_STRIDED;
            else indexInfo = rv::RV_METADATA_INDEX_RANDOM;
            break;
        }

        case Instruction::Alloca:
        {
            assert (iiVec.size() == 1);

            if (rv::hasMetadata(inst, rv::RV_METADATA_OP_UNIFORM))
            {
                indexInfo = rv::RV_METADATA_INDEX_SAME;
            }
            else if (rv::isVectorizableNonDerivedType(*inst->getType()->getPointerElementType()))
            {
                // Can use vector load -> CONSECUTIVE.
                indexInfo = rv::RV_METADATA_INDEX_CONSECUTIVE;
            }
            else
            {
                indexInfo = rv::RV_METADATA_INDEX_RANDOM;
            }
            break;
        }

        default:
        {
            // Retain any information if all operands have the same mark.
            if (iiVec.size() == 0)
            {
                indexInfo = rv::RV_METADATA_INDEX_SAME;
                break;
            }
            bool allSame = true;
            bool allConsecutive = true;
            bool allStrided = true;
            for (const auto &it : iiVec)
            {
                allSame &= strcmp(it, rv::RV_METADATA_INDEX_SAME) == 0;
                allConsecutive &= strcmp(it, rv::RV_METADATA_INDEX_CONSECUTIVE) == 0;
                allStrided &= strcmp(it, rv::RV_METADATA_INDEX_STRIDED) == 0;
            }
            assert (!(allSame && allConsecutive));
            assert (!(allSame && allStrided));
            assert (!(allStrided && allConsecutive));

            if (allSame) indexInfo = rv::RV_METADATA_INDEX_SAME;
            else if (allConsecutive) indexInfo = rv::RV_METADATA_INDEX_CONSECUTIVE;
            else if (allStrided) indexInfo = rv::RV_METADATA_INDEX_STRIDED;
            else indexInfo = rv::RV_METADATA_INDEX_RANDOM; // weakest information
            break;
        }
    }

    assert(indexInfo && "was not set!");

    // A non-RES_UNIFORM instruction can never be INDEX_SAME, but it can be
    // INDEX_CONSECUTIVE.
    if (strcmp(indexInfo, rv::RV_METADATA_INDEX_SAME) == 0 &&
        !rv::hasMetadata(inst, rv::RV_METADATA_RES_UNIFORM))
    {
        return rv::RV_METADATA_INDEX_RANDOM;
    }

    return indexInfo;
}

// Derive alignment info depending on the instruction and operands marks
// TODO: div/rem/others?
const char*
VectorizationAnalysis::deriveAlignmentInfo(Instruction* inst,
                                           const std::vector<const char*>& aiVec) const
{
    assert (inst);

    switch (inst->getOpcode())
    {
        // These instructions retain alignment if all operands are aligned
        case Instruction::Add:
        case Instruction::Sub:
        case Instruction::FAdd:
        case Instruction::FSub:
        {
            assert (aiVec.size() == 2);
            if (strcmp(aiVec[0], rv::RV_METADATA_ALIGNED_TRUE) != 0 ||
                strcmp(aiVec[1], rv::RV_METADATA_ALIGNED_TRUE) != 0)
            {
                return rv::RV_METADATA_ALIGNED_FALSE;
            }
            return rv::RV_METADATA_ALIGNED_TRUE;
        }

        // These instructions retain alignment if one operand is aligned
        case Instruction::Mul:
        case Instruction::FMul:
        {
            assert (aiVec.size() == 2);
            if (strcmp(aiVec[0], rv::RV_METADATA_ALIGNED_TRUE) == 0 ||
                strcmp(aiVec[1], rv::RV_METADATA_ALIGNED_TRUE) == 0)
            {
                return rv::RV_METADATA_ALIGNED_TRUE;
            }
            return rv::RV_METADATA_ALIGNED_FALSE;
        }

        // These instructions retain any information if all operands have
        // the same mark except for casts to void pointer type.
        case Instruction::Trunc:
        case Instruction::SExt:
        case Instruction::FPTrunc:
        case Instruction::FPExt:
        case Instruction::ZExt:
        case Instruction::FPToUI:
        case Instruction::FPToSI:
        case Instruction::UIToFP:
        case Instruction::SIToFP:
        case Instruction::IntToPtr:
        case Instruction::PtrToInt:
        case Instruction::BitCast:
        {
            if (rv::returnsVoidPtr(*inst)) return rv::RV_METADATA_INDEX_RANDOM;

            assert (aiVec.size() == 1);
            if (strcmp(aiVec[0], rv::RV_METADATA_ALIGNED_TRUE) == 0)
            {
                return rv::RV_METADATA_ALIGNED_TRUE;
            }
            return rv::RV_METADATA_ALIGNED_FALSE;
        }

        // GEP retains alignment if all indices are aligned
        // TODO: really? :P
        case Instruction::GetElementPtr:
        {
            GetElementPtrInst* gep = cast<GetElementPtrInst>(inst);
            const unsigned numIndices = gep->getNumIndices();
            const unsigned idxBegin = gep->getNumOperands() - numIndices;
            for (unsigned i=idxBegin, e=gep->getNumOperands(); i!=e; ++i)
            {
                if (strcmp(aiVec[i], rv::RV_METADATA_ALIGNED_TRUE) != 0)
                {
                    return rv::RV_METADATA_ALIGNED_FALSE;
                }
            }
            return rv::RV_METADATA_ALIGNED_TRUE;
        }

        // Phi is aligned if all incoming values are aligned
        case Instruction::PHI:
        {
            PHINode* phi = cast<PHINode>(inst);
            for (unsigned i=0, e=phi->getNumIncomingValues(); i!=e; ++i)
            {
                // We ignored basic blocks while collecting indexInfo,
                // so now we can directly index iiVec per incoming value.
                if (strcmp(aiVec[i], rv::RV_METADATA_ALIGNED_TRUE) != 0)
                {
                    return rv::RV_METADATA_ALIGNED_FALSE;
                }
            }

            return rv::RV_METADATA_ALIGNED_TRUE;
        }

        case Instruction::Alloca:
        {
            return rv::RV_METADATA_ALIGNED_TRUE;
        }

        // All other instructions (conservatively) produce non-aligned values
        default:
        {
            return rv::RV_METADATA_ALIGNED_FALSE; // weakest information
        }
    }
}

const char*
VectorizationAnalysis::deriveIndexInfo(const Constant* c) const
{
    assert (c);
    assert (!isa<BasicBlock>(c));
    assert (!isa<Function>(c));

    return rv::RV_METADATA_INDEX_SAME;
}

// TODO: implement support for natural numbers stored as floats?
const char*
VectorizationAnalysis::deriveAlignedInformation(const Constant* c) const
{
    assert (c);
    assert (!isa<BasicBlock>(c));
    assert (!isa<Function>(c));

    // An undef value is never aligned.
    if (isa<UndefValue>(c)) return rv::RV_METADATA_ALIGNED_FALSE;

    // Integer that are divisible by the simd width are ALIGN_TRUE.
    if (c->getType()->isIntegerTy())
    {
        const ConstantInt* cint = cast<ConstantInt>(c);
        const uint64_t& intValue = *cint->getValue().getRawData();
        if (intValue % mVectorizationFactor == 0) return rv::RV_METADATA_ALIGNED_TRUE;
        else return rv::RV_METADATA_ALIGNED_FALSE;
    }

    // Other than that, only integer vector constants can be aligned.
    if (!c->getType()->isVectorTy()) return rv::RV_METADATA_ALIGNED_FALSE;

    // A zero-vector is aligned.
    if (isa<ConstantAggregateZero>(c)) return rv::RV_METADATA_ALIGNED_TRUE;

    if (const ConstantDataVector* cdv = dyn_cast<ConstantDataVector>(c))
    {
        if (!cdv->getElementType()->isIntegerTy())
        {
            return rv::RV_METADATA_ALIGNED_FALSE;
        }

        const uint64_t intValue = cdv->getElementAsInteger(0);

        if (intValue % mVectorizationFactor != 0) return rv::RV_METADATA_ALIGNED_FALSE;

        bool isSame   = true;
        bool isConsec = true;
        for (unsigned i=1, e=cdv->getNumElements(); i<e; ++i)
        {
            const uint64_t val = cdv->getElementAsInteger(i);
            if (isSame && intValue == val)
            {
                isConsec = false;
                continue;
            }
            else
            {
                isSame   = false;
            }

            if (isConsec && intValue + i == val)
            {
                isSame   = false;
                continue;
            }
            else
            {
                isConsec = false;
            }

            if (!isSame && !isConsec) break;
        }

        return (isSame || isConsec) ?
            rv::RV_METADATA_ALIGNED_TRUE :
            rv::RV_METADATA_ALIGNED_FALSE;
    }

    assert (isa<ConstantVector>(c));
    const ConstantVector* cv = cast<ConstantVector>(c);

    if (!cv->getType()->getElementType()->isIntegerTy()) return rv::RV_METADATA_ALIGNED_FALSE;

    assert (isa<ConstantInt>(cv->getOperand(0)));
    const ConstantInt* celem = cast<ConstantInt>(cv->getOperand(0));
    const uint64_t& intValue = *celem->getValue().getRawData();

    // The vector is aligned if its first element is aligned and the
    // constant is either SAME or CONSECUTIVE
    if (intValue % mVectorizationFactor != 0) return rv::RV_METADATA_ALIGNED_FALSE;

    // TODO: There might be other cases (e.g. STRIDED) where we want to
    //       return true...
    const char* ii = deriveIndexInfo(c);
    if (strcmp(ii, rv::RV_METADATA_INDEX_SAME) == 0)
    {
        return rv::RV_METADATA_ALIGNED_TRUE;
    }
    else if (strcmp(ii, rv::RV_METADATA_INDEX_CONSECUTIVE) == 0)
    {
        return rv::RV_METADATA_ALIGNED_TRUE;
    }
    return rv::RV_METADATA_ALIGNED_FALSE;
}


bool
VectorizationAnalysis::requiresScalarResults(const Instruction& inst) const
{
    // Exclude instructions that can never be RES_SCALARS.
    if (inst.getType()->isVoidTy()) return false;

    // Exclude instructions that can never be OP_SEQUENTIAL.
    // TODO: This might be wrong for loop header phis:
    if (rv::hasMetadata(&inst, rv::RV_METADATA_OP_UNIFORM)) return false;

    // If the instruction is OP_VARYING / INDEX_CONSECUTIVE,
    // it can remain RES_VECTOR even if the type is not okay since
    // we will not actually create vectors.
    if (rv::hasMetadata(&inst, rv::RV_METADATA_INDEX_CONSECUTIVE)) return false;

    // Check instructions that were marked from outside.
    if (mValueInfoMap->hasMapping(inst)) return false;

#if 0
    // Check all other instructions except for those with
    // special handling (load/store/gep/phi/select/call).
    if (rv::mayHaveSideEffects(inst, mFunctionInfoMap) || requiresSpecialHandling(inst)) return;
#endif

    // If this is an OP_VARYING alloca of a type with nested pointers that has
    // OP_SEQUENTIAL uses, we have to split it up as well to prevent pointer-
    // to-vector vs. vector-of-pointers problems. Essentially, this means that
    // if there is an OP_SEQUENTIAL use, we consider any pointer to be a vector
    // of pointers, which we currently do not support natively.
    if (isa<AllocaInst>(inst) && rv::hasNestedPointer(*inst.getType()))
    {
        for (Instruction::const_use_iterator U=inst.use_begin(), UE=inst.use_end(); U!=UE; ++U)
        {
            const Instruction* useI = cast<Instruction>(U->getUser());
            if (rv::hasMetadata(useI, rv::RV_METADATA_OP_SEQUENTIAL) ||
                rv::hasMetadata(useI, rv::RV_METADATA_OP_SEQUENTIAL_GUARDED))
            {
                return true;
            }
        }
    }

    // For performance reasons, if the alloca is an array (that is not tiny) and
    // has OP_SEQUENTIAL uses, we also mark it SEQUENTIAL to prevent too many
    // copy operations.
    if (isa<AllocaInst>(inst) &&
        (inst.getType()->getPointerElementType()->isArrayTy() &&
         inst.getType()->getPointerElementType()->getArrayNumElements() > 16))
    {
        for (Instruction::const_use_iterator U=inst.use_begin(), UE=inst.use_end(); U!=UE; ++U)
        {
            const Instruction* useI = cast<Instruction>(U->getUser());
            if (rv::hasMetadata(useI, rv::RV_METADATA_OP_SEQUENTIAL) ||
                rv::hasMetadata(useI, rv::RV_METADATA_OP_SEQUENTIAL_GUARDED))
            {
                return true;
            }
        }
    }

    // If the return type can not be vectorized, it has to be RES_SCALARS.
    // NOTE: A pointer return type *can* be okay, since it may be transformed to
    //       a pointer to a vector whereas a vector of pointers would not be okay.
    //       However, this depends on the OP_SEQUENTIAL/_GUARDED property, so we
    //       can not yet decide that.
    // TODO: Using returnsVoidPtr() here effectively disallows <W x i8>*.
    if (!rv::returnsVoidPtr(inst) && rv::isVectorizableType(*inst.getType())) return false;

    return true;
}

// An instruction requires guards only if it may have side effects.
// An instruction requires guards only if its parent block has a non-fully-uniform mask.
// Exceptions:
// - We never guard loads but leave this to blending (may not be okay if race conditions occur).
// - We don't have to guard calls that have a mask parameter (these are not considered
//   as "may have side effects").
// - TODO: Do some instructions from isVectorizableInst() require guards?
bool
VectorizationAnalysis::mayRequireGuards(const Instruction& inst) const
{
    if (!rv::mayHaveSideEffects(inst, mFunctionInfoMap)) return false;

    if (mDisableAllAnalyses) return true;

    // TODO: Unsure whether guarding scalar loads with non-uniform mask
    //       could be faster than executing all W loads.
    if (isa<LoadInst>(inst)) return false;

    const BasicBlock* parentBB = inst.getParent();

    const bool isAlwaysByAllOrNone =
        rv::hasMetadata(parentBB, rv::RV_METADATA_ALWAYS_BY_ALL_TRUE) ||
        rv::hasMetadata(parentBB, rv::RV_METADATA_ALWAYS_BY_ALL_OR_NONE);

    return !isAlwaysByAllOrNone;
}

// Returns false if the GEP requires splitting, true otherwise.
bool
VectorizationAnalysis::hasVaryingIndex(const GetElementPtrInst& gep) const
{
    for (GetElementPtrInst::const_op_iterator IDX=gep.idx_begin(),
            IDXE=gep.idx_end(); IDX!=IDXE; ++IDX)
    {
        assert (isa<Value>(IDX));
        const Value* idxV = cast<Value>(IDX);

        if (isa<Constant>(idxV)) continue;
        if (rv::hasMetadata(idxV, rv::RV_METADATA_RES_UNIFORM)) continue;
        if (rv::hasMetadata(idxV, rv::RV_METADATA_INDEX_CONSECUTIVE)) continue;

        return true;
    }

    return false;
}

bool
VectorizationAnalysis::requiresSpecialHandling(const Instruction& inst) const
{
    return isa<GetElementPtrInst>(inst) ||
        isa<PHINode>(inst) ||
        isa<SelectInst>(inst);
}

bool
VectorizationAnalysis::hasSequentialOperand(const Instruction& inst) const
{
    for (Instruction::const_op_iterator O=inst.op_begin(),
        OE=inst.op_end(); O!=OE; ++O)
    {
        if (operandRequiresSequentialExec(**O)) return true;
    }

    return false;
}

bool
VectorizationAnalysis::operandRequiresSequentialExec(const Value& value) const
{
    assert (!value.getType()->isVoidTy() && "how can an operand be of type void?!");
    if (value.getType()->isLabelTy()) return false;
    if (value.getType()->isFunctionTy()) return false;
    if (isa<PointerType>(value.getType()) &&
        value.getType()->getPointerElementType()->isFunctionTy()) return false;

    if (isa<Instruction>(value) || isa<Argument>(value))
    {
        assert (!(rv::hasMetadata(&value, rv::RV_METADATA_RES_SCALARS) &&
                  rv::hasMetadata(&value, rv::RV_METADATA_INDEX_CONSECUTIVE)) &&
                "value can not be RES_SCALARS and INDEX_CONSECUTIVE!");

        return rv::hasMetadata(&value, rv::RV_METADATA_RES_SCALARS);
    }

    return false;
}

////////////////////////////////////////////////////////////////////////////
//                           MASK INFO ANALYSIS                           //
////////////////////////////////////////////////////////////////////////////

void
VectorizationAnalysis::analyzeMaskInfo(Function& scalarFn)
{
    DEBUG_VA( outs() << "\nMarking mask operations...\n"; );

    for (auto& BB : scalarFn)
    {
        for (auto& I : BB)
        {
            if (BranchInst* br = dyn_cast<BranchInst>(&I))
            {
                if (br->isConditional())
                {
                    if (!isa<Instruction>(br->getCondition())) continue;
                    markAsMask(cast<Instruction>(br->getCondition()));
                }
            }
            else if (SelectInst* sel = dyn_cast<SelectInst>(&I))
            {
                // TODO: HERE! Also mark arguments as masks?
                if (!isa<Instruction>(sel->getCondition())) continue;
                markAsMask(cast<Instruction>(sel->getCondition()));
            }
        }
    }
}

void
VectorizationAnalysis::markAsMask(Instruction* inst)
{
    assert (inst);

    if (rv::hasMetadata(inst, rv::RV_METADATA_MASK)) return;
    if (!inst->getType()->isIntegerTy(1)) return;

    markValueAs(inst, rv::RV_METADATA_MASK);

    // Stop at compare instructions
    if (isa<CmpInst>(inst)) return;

    // If this is no compare instruction, go backwards and mark operands as MASK.
    for (auto& operand : inst->operands())
    {
        if (!isa<Instruction>(operand)) continue;

        Instruction* opInst = cast<Instruction>(operand);
        markAsMask(opInst);
    }
}

////////////////////////////////////////////////////////////////////////////
//                                 MISC                                   //
////////////////////////////////////////////////////////////////////////////

bool
VectorizationAnalysis::markValueAs(Value* value, const char* mark)
{
    assert (value && mark);

    auto * branch = dyn_cast<BranchInst>(value);
    assert(!branch || rv::hasMetadata(value, rv::RV_METADATA_OP_VARYING) == 0);

    DEBUG_VA( outs() << "  marking value: " << value->getName() << " as " << mark << "...\n"; );

    assert ((strcmp(mark, rv::RV_METADATA_ALIGNED_TRUE) != 0 ||
            !rv::hasMetadata(value, rv::RV_METADATA_ALIGNED_FALSE)) &&
            "must not overwrite ALIGNED_FALSE with ALIGNED_TRUE!");
    assert ((strcmp(mark, rv::RV_METADATA_OP_UNIFORM) != 0 ||
            (!rv::hasMetadata(value, rv::RV_METADATA_OP_VARYING) &&
            !rv::hasMetadata(value, rv::RV_METADATA_OP_SEQUENTIAL) &&
            !rv::hasMetadata(value, rv::RV_METADATA_OP_SEQUENTIAL_GUARDED))) &&
            "must not overwrite OP_VARYING/OP_SEQUENTIAL with OP_UNIFORM!");
    assert ((strcmp(mark, rv::RV_METADATA_OP_VARYING) != 0 ||
            (!rv::hasMetadata(value, rv::RV_METADATA_OP_SEQUENTIAL) &&
            !rv::hasMetadata(value, rv::RV_METADATA_OP_SEQUENTIAL_GUARDED))) &&
            "must not overwrite OP_SEQUENTIAL with OP_VARYING!");
    assert ((strcmp(mark, rv::RV_METADATA_OP_SEQUENTIAL) != 0 ||
            !rv::hasMetadata(value, rv::RV_METADATA_OP_SEQUENTIAL_GUARDED)) &&
            "must not overwrite OP_SEQUENTIAL_GUARDED with OP_SEQUENTIAL!");
    assert ((strcmp(mark, rv::RV_METADATA_RES_UNIFORM) != 0 ||
            (!rv::hasMetadata(value, rv::RV_METADATA_RES_VECTOR) &&
            !rv::hasMetadata(value, rv::RV_METADATA_RES_SCALARS))) &&
            "must not overwrite RES_VECTOR/RES_SCALARS with RES_UNIFORM!");
    assert ((strcmp(mark, rv::RV_METADATA_RES_VECTOR) != 0 ||
            !rv::hasMetadata(value, rv::RV_METADATA_RES_SCALARS)) &&
            "must not overwrite RES_SCALARS with RES_VECTOR!");
    assert ((strcmp(mark, rv::RV_METADATA_INDEX_SAME) != 0 ||
            (!rv::hasMetadata(value, rv::RV_METADATA_INDEX_CONSECUTIVE) &&
            !rv::hasMetadata(value, rv::RV_METADATA_INDEX_SHUFFLE) &&
            !rv::hasMetadata(value, rv::RV_METADATA_INDEX_STRIDED) &&
            !rv::hasMetadata(value, rv::RV_METADATA_INDEX_RANDOM))) &&
            "must not overwrite CONSECUTIVE/SHUFFLE/STRIDED/RANDOM with SAME!");
    assert ((strcmp(mark, rv::RV_METADATA_INDEX_CONSECUTIVE) != 0 ||
            (!rv::hasMetadata(value, rv::RV_METADATA_INDEX_SHUFFLE) &&
            !rv::hasMetadata(value, rv::RV_METADATA_INDEX_STRIDED) &&
            !rv::hasMetadata(value, rv::RV_METADATA_INDEX_RANDOM))) &&
            "must not overwrite SHUFFLE/STRIDED/RANDOM with CONSECUTIVE!");
    assert ((strcmp(mark, rv::RV_METADATA_INDEX_SHUFFLE) != 0 ||
            (!rv::hasMetadata(value, rv::RV_METADATA_INDEX_STRIDED) &&
            !rv::hasMetadata(value, rv::RV_METADATA_INDEX_RANDOM))) &&
            "must not overwrite STRIDED/RANDOM with SHUFFLE!");
    assert ((strcmp(mark, rv::RV_METADATA_INDEX_STRIDED) != 0 ||
            (!rv::hasMetadata(value, rv::RV_METADATA_INDEX_SHUFFLE) &&
            !rv::hasMetadata(value, rv::RV_METADATA_INDEX_RANDOM))) &&
            "must not overwrite SHUFFLE/RANDOM with STRIDED!");

    assert ((strcmp(mark, rv::RV_METADATA_DIVERGENT_FALSE) != 0 ||
            !rv::hasMetadata(value, rv::RV_METADATA_DIVERGENT_TRUE)) &&
            "must not overwrite DIVERGENT_TRUE with DIVERGENT_FALSE!");
    assert ((strcmp(mark, rv::RV_METADATA_ALWAYS_BY_ALL_TRUE) != 0 ||
            !rv::hasMetadata(value, rv::RV_METADATA_ALWAYS_BY_ALL_FALSE)) &&
            "must not overwrite ALWAYS_BY_ALL_FALSE with ALWAYS_BY_ALL_TRUE!");
    assert ((strcmp(mark, rv::RV_METADATA_ALWAYS_BY_ALL_OR_NONE) != 0 ||
            !rv::hasMetadata(value, rv::RV_METADATA_ALWAYS_BY_ALL_FALSE)) &&
            "must not overwrite ALWAYS_BY_ALL_FALSE with ALWAYS_BY_ALL_OR_NONE!");
    assert ((strcmp(mark, rv::RV_METADATA_ALWAYS_BY_ALL_TRUE) != 0 ||
            !rv::hasMetadata(value, rv::RV_METADATA_ALWAYS_BY_ALL_OR_NONE)) &&
            "must not overwrite ALWAYS_BY_ALL_OR_NONE with ALWAYS_BY_ALL_TRUE!");
    assert ((strcmp(mark, rv::RV_METADATA_LOOP_DIVERGENT_FALSE) != 0 ||
            !rv::hasMetadata(value, rv::RV_METADATA_LOOP_DIVERGENT_TRUE)) &&
            "must not overwrite LOOP_DIVERGENT_TRUE with LOOP_DIVERGENT_FALSE!");
    assert ((strcmp(mark, rv::RV_METADATA_OPTIONAL) != 0 ||
            !rv::hasMetadata(value, rv::RV_METADATA_MANDATORY)) &&
            "must not overwrite MANDATORY with OPTIONAL!");

    if (rv::hasMetadata(value, mark))
    {
        DEBUG_VA( outs() << "    previously marked as " << mark << " - ignored!\n"; );
        return false;
    }

    rv::setMetadata(value, mark);
    return true;
}

bool
VerifyVectorizationAnalysis(const Function& f)
{
    bool verified = true;

    for (const BasicBlock & block : f) {
    	const auto * BB = &block;
        const bool isMandatory         = rv::hasMetadata(BB, rv::RV_METADATA_MANDATORY);
        const bool isOptional          = rv::hasMetadata(BB, rv::RV_METADATA_OPTIONAL);
        const bool isDivergent         = rv::hasMetadata(BB, rv::RV_METADATA_DIVERGENT_TRUE);
        const bool isNonDivergent      = rv::hasMetadata(BB, rv::RV_METADATA_DIVERGENT_FALSE);
        const bool isAlwaysByAll       = rv::hasMetadata(BB, rv::RV_METADATA_ALWAYS_BY_ALL_TRUE);
        const bool isAlwaysByAllOrNone = rv::hasMetadata(BB, rv::RV_METADATA_ALWAYS_BY_ALL_OR_NONE);
        const bool isNotAlwaysByAll    = rv::hasMetadata(BB, rv::RV_METADATA_ALWAYS_BY_ALL_FALSE);
        const bool isLoopDiv           = rv::hasMetadata(BB, rv::RV_METADATA_LOOP_DIVERGENT_TRUE);
        const bool isLoopNonDiv        = rv::hasMetadata(BB, rv::RV_METADATA_LOOP_DIVERGENT_FALSE);

        // A block must have no or non-ambiguous marks.
        verified &= (unsigned)isMandatory + (unsigned)isOptional <= 1U;
        verified &= (unsigned)isDivergent + (unsigned)isNonDivergent <= 1U;
        verified &= (unsigned)isAlwaysByAll +
                (unsigned)isAlwaysByAllOrNone +
                (unsigned)isNotAlwaysByAll <= 1U;

        // A loop must have no or non-ambiguous marks.
        verified &= (unsigned)isLoopDiv + (unsigned)isLoopNonDiv <= 1U;

        for (const Instruction & inst : block) {
        	const auto * I = &inst;
            // An instruction must have no or non-ambiguous marks per "class"
            const bool isOpUniform    = rv::hasMetadata(I, rv::RV_METADATA_OP_UNIFORM);
            const bool isOpParallel   = rv::hasMetadata(I, rv::RV_METADATA_OP_VARYING);
            const bool isOpSequential = rv::hasMetadata(I, rv::RV_METADATA_OP_SEQUENTIAL);
            const bool isOpSeqGuarded = rv::hasMetadata(I, rv::RV_METADATA_OP_SEQUENTIAL_GUARDED);

            verified &= (unsigned)isOpUniform +
                    (unsigned)isOpParallel +
                    (unsigned)isOpSequential +
                    (unsigned)isOpSeqGuarded <= 1U;

            const bool isResUniform   = rv::hasMetadata(I, rv::RV_METADATA_RES_UNIFORM);
            const bool isResVector    = rv::hasMetadata(I, rv::RV_METADATA_RES_VECTOR);
            const bool isResScalars   = rv::hasMetadata(I, rv::RV_METADATA_RES_SCALARS);

            verified &= (unsigned)isResUniform +
                    (unsigned)isResVector +
                    (unsigned)isResScalars <= 1U;

            const bool isAligned      = rv::hasMetadata(I, rv::RV_METADATA_ALIGNED_TRUE);
            const bool isNotAligned   = rv::hasMetadata(I, rv::RV_METADATA_ALIGNED_FALSE);

            verified &= (unsigned)isAligned + (unsigned)isNotAligned <= 1U;

            const bool isSame         = rv::hasMetadata(I, rv::RV_METADATA_INDEX_SAME);
            const bool isConsecutive  = rv::hasMetadata(I, rv::RV_METADATA_INDEX_CONSECUTIVE);
            const bool isShuffle      = rv::hasMetadata(I, rv::RV_METADATA_INDEX_SHUFFLE);
            const bool isStrided      = rv::hasMetadata(I, rv::RV_METADATA_INDEX_STRIDED);
            const bool isRandom       = rv::hasMetadata(I, rv::RV_METADATA_INDEX_RANDOM);

            verified &= (unsigned)isSame +
                    (unsigned)isConsecutive +
                    (unsigned)isShuffle +
                    (unsigned)isStrided +
                    (unsigned)isRandom <= 1U;
        }

        // There must be at most one function call to the metadataFn
        bool callFound = false;
        for (BasicBlock::const_iterator I=BB->begin(), IE=BB->end(); I!=IE; ++I)
        {
            if (!isa<CallInst>(I)) continue;
            const CallInst* call = cast<CallInst>(I);
            if (call->getCalledFunction() != argMetadataFn) continue;

            verified &= !callFound;
            callFound = true;
        }
    }

    return verified;
}

void VectorizationAnalysis::checkLoop(const Loop* L)
{
    assert(rv::hasMetadata(L, rv::RV_METADATA_LOOP_TOP_LEVEL_DIVERGENT) ==
           mVecInfo.isDivergentLoopTopLevel(L));

    assert(rv::hasMetadata(L, rv::RV_METADATA_LOOP_DIVERGENT_TRUE) ==
           mVecInfo.isDivergentLoop(L));
    assert(rv::hasMetadata(L, rv::RV_METADATA_LOOP_DIVERGENT_FALSE) ==
           !mVecInfo.isDivergentLoop(L));

    for (const Loop* SL : L->getSubLoops())
        checkLoop(SL);
}

void
VectorizationAnalysis::checkEquivalentToNewAnalysis(Function& F)
{
#if 1
    for (const Loop* L : mLoopInfo)
	{
		checkLoop(L);
	}

	/* Tests whether both analyses are equivalent */
	for (auto& BB : F)
	{
		assert(rv::hasMetadata(&BB, rv::RV_METADATA_DIVERGENT_FALSE) ==
			   mVecInfo.getVectorShape(BB).isUniform());
		assert(rv::hasMetadata(&BB, rv::RV_METADATA_DIVERGENT_TRUE) ==
			   mVecInfo.getVectorShape(BB).isVarying());

		for (auto& I : BB)
		{
			if (rv::isMetadataCall(&I))
				continue;

            if (const CallInst* call = dyn_cast<CallInst>(&I))
                if (call->getCalledFunction()->getReturnType()->isVoidTy())
                    continue;

			rv::VectorShape vs = mVecInfo.getVectorShape(I);

			/*if (!isa<TerminatorInst>(&I))
			{
				if (rv::hasMetadata(&I, rv::RV_METADATA_INDEX_SAME))
					assert (vs.isUniform());
				else if (rv::hasMetadata(&I, rv::RV_METADATA_INDEX_CONSECUTIVE))
					assert (vs.isContiguous());
				else if (rv::hasMetadata(&I, rv::RV_METADATA_INDEX_RANDOM))
				{}//assert (vs.isVarying()); // TODO allow more precise results
				else if (rv::hasMetadata(&I, rv::RV_METADATA_RES_VECTOR))
					assert (vs.isVarying());
				else if (rv::hasMetadata(&I, rv::RV_METADATA_RES_SCALARS))
					assert (vs.isVarying());
				else if (rv::hasMetadata(&I, rv::RV_METADATA_OP_UNIFORM))
					assert (vs.isUniform());

				//if (rv::hasMetadata(&I, rv::RV_METADATA_ALIGNED_TRUE))
				//   assert (vs.getAlignment() % mVecinfo.getMapping().vectorWidth == 0);
			}*/

			if (isa<PHINode>(&I))
			{
				if (rv::hasMetadata(&I, rv::RV_METADATA_OP_VARYING))
					assert(vs.isVarying());
				else
					assert(!mVecInfo.isDivergent(*I.getParent()));
			}
		}

		assert (rv::hasMetadata(&BB, rv::RV_METADATA_MANDATORY) ==
				mVecInfo.isMandatory(&BB));
		assert (rv::hasMetadata(&BB, rv::RV_METADATA_OPTIONAL) ==
				!mVecInfo.isMandatory(&BB));
	}
#endif
}
