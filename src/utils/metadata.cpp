//===- metadata.cpp ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @author karrenberg

#include "metadata.h"
#include "rvConfig.h"

#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Analysis/LoopInfo.h>

#include <llvm/IR/IRBuilder.h>

#include "stringUtils.h"

static void
SetInsertAtStart(IRBuilder<> & builder, BasicBlock * block) {
	if (block->empty()) {
		builder.SetInsertPoint(block);
	} else {
		builder.SetInsertPoint(block, block->getFirstInsertionPt());
	}
}

static llvm::ValueAsMetadata * CreateMD(Value * V) {
 	if (! V) return nullptr;
 	else return ValueAsMetadata::get(V);
 }

static Value*
getV(Metadata * MD) {
	auto * valMD = dyn_cast<ValueAsMetadata>(MD);
	if (!valMD) return nullptr;
	return valMD->getValue();
}

namespace rv {

void
setUpMetadata(Module* mod)
{
    assert (mod);

    if (Function* fn = mod->getFunction(RV_METADATA_ARGUMENT_INFO_FUNCTION_NAME))
    {
        assert (fn->isDeclaration() &&
                fn->getFunctionType()->getReturnType()->isVoidTy() &&
                fn->getFunctionType()->getFunctionNumParams() == 0 &&
                fn->doesNotAccessMemory() &&
                fn->doesNotThrow() &&
                "incompatible function 'rvMetadataFn' already exists in module!");

        argMetadataFn = fn;
    }
    else
    {
        FunctionType* fTy = FunctionType::get(Type::getVoidTy(mod->getContext()), false);
        argMetadataFn = Function::Create(fTy,
                                      Function::ExternalLinkage,
                                      RV_METADATA_ARGUMENT_INFO_FUNCTION_NAME,
                                      mod);
        argMetadataFn->setDoesNotAccessMemory();
        argMetadataFn->setDoesNotThrow();
    }

    nullMDN = MDNode::get(mod->getContext(), nullptr);

    assert (isMetadataSetUp());
}

bool
isMetadataSetUp()
{
    return argMetadataFn != nullptr && nullMDN != nullptr;
}

bool
isMetadataCall(const Instruction* inst)
{
    if (!inst) return false;
    if (!isa<CallInst>(inst)) return false;
    const CallInst* call = cast<CallInst>(inst);
    if (!call->getCalledFunction()) return false;
    return call->getCalledFunction()->getName().equals(RV_METADATA_ARGUMENT_INFO_FUNCTION_NAME);
}

bool
hasRVMetadata(const Function& f)
{
    for (auto &BB : f)
    {
        if (hasRVMetadata(&BB)) return true;

        for (auto &I : BB)
        {
            if (hasRVMetadata(&I)) return true;
        }
    }

    for (Function::const_arg_iterator A=f.arg_begin(),
         AE=f.arg_end(); A!=AE; ++A)
    {
        if (hasRVMetadata(&*A)) return true;
    }

    return false;
}

void
setMetadata(Instruction* inst, const char* const metaDataString)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    assert (inst);

    if (strcmp(metaDataString, RV_METADATA_OP_UNIFORM) == 0 ||
        strcmp(metaDataString, RV_METADATA_OP_VARYING) == 0 ||
        strcmp(metaDataString, RV_METADATA_OP_SEQUENTIAL) == 0 ||
        strcmp(metaDataString, RV_METADATA_OP_SEQUENTIAL_GUARDED) == 0)
    {
        removeMetadata(inst, RV_METADATA_OP_UNIFORM);
        removeMetadata(inst, RV_METADATA_OP_VARYING);
        removeMetadata(inst, RV_METADATA_OP_SEQUENTIAL);
        removeMetadata(inst, RV_METADATA_OP_SEQUENTIAL_GUARDED);
    }
    else if (strcmp(metaDataString, RV_METADATA_RES_UNIFORM) == 0 ||
             strcmp(metaDataString, RV_METADATA_RES_VECTOR) == 0 ||
             strcmp(metaDataString, RV_METADATA_RES_SCALARS) == 0)
    {
        removeMetadata(inst, RV_METADATA_RES_UNIFORM);
        removeMetadata(inst, RV_METADATA_RES_VECTOR);
        removeMetadata(inst, RV_METADATA_RES_SCALARS);
    }
    else if (strcmp(metaDataString, RV_METADATA_ALIGNED_TRUE) == 0 ||
             strcmp(metaDataString, RV_METADATA_ALIGNED_FALSE) == 0)
    {
        removeMetadata(inst, RV_METADATA_ALIGNED_TRUE);
        removeMetadata(inst, RV_METADATA_ALIGNED_FALSE);
    }
    else if (strcmp(metaDataString, RV_METADATA_INDEX_SAME) == 0 ||
             strcmp(metaDataString, RV_METADATA_INDEX_CONSECUTIVE) == 0 ||
             strcmp(metaDataString, RV_METADATA_INDEX_SHUFFLE) == 0 ||
             strcmp(metaDataString, RV_METADATA_INDEX_STRIDED) == 0 ||
             strcmp(metaDataString, RV_METADATA_INDEX_RANDOM) == 0)
    {
        removeMetadata(inst, RV_METADATA_INDEX_SAME);
        removeMetadata(inst, RV_METADATA_INDEX_CONSECUTIVE);
        removeMetadata(inst, RV_METADATA_INDEX_SHUFFLE);
        removeMetadata(inst, RV_METADATA_INDEX_STRIDED);
        removeMetadata(inst, RV_METADATA_INDEX_RANDOM);
    }
    else
    {
        assert ((strcmp(metaDataString, RV_METADATA_ARGUMENT_CAST) == 0 ||
                strcmp(metaDataString, RV_METADATA_PKT_PTR_CAST) == 0 ||
                strcmp(metaDataString, RV_METADATA_BLEND_INFO) == 0 ||
                strcmp(metaDataString, RV_METADATA_PACK_UNPACK) == 0 ||
                strcmp(metaDataString, RV_METADATA_VARIANT_START) == 0 ||
                strcmp(metaDataString, RV_METADATA_VARIANT_END) == 0 ||
                strcmp(metaDataString, RV_METADATA_VARIANT_DISABLE_VECT) == 0 ||
                strcmp(metaDataString, RV_METADATA_VARIANT_SEQUENTIALIZE) == 0 ||
                strcmp(metaDataString, RV_METADATA_VARIANT_BOSCC) == 0 ||
                strcmp(metaDataString, RV_METADATA_MASK) == 0) &&
                "invalid metadata for instruction found!");
    }

    inst->setMetadata(metaDataString, nullMDN);
}

bool
hasMetadata(const Instruction* inst)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    assert (inst);
    return inst->hasMetadata();
}

bool
hasRVMetadata(const Instruction* inst)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    assert (inst);
    if (hasMetadata(inst, rv::RV_METADATA_ARGUMENT_CAST)) return true;
    if (hasMetadata(inst, rv::RV_METADATA_PKT_PTR_CAST)) return true;
    if (hasMetadata(inst, rv::RV_METADATA_BLEND_INFO)) return true;
    if (hasMetadata(inst, rv::RV_METADATA_PACK_UNPACK)) return true;
    if (hasMetadata(inst, rv::RV_METADATA_OP_UNIFORM)) return true;
    if (hasMetadata(inst, rv::RV_METADATA_OP_VARYING)) return true;
    if (hasMetadata(inst, rv::RV_METADATA_OP_SEQUENTIAL)) return true;
    if (hasMetadata(inst, rv::RV_METADATA_OP_SEQUENTIAL_GUARDED)) return true;
    if (hasMetadata(inst, rv::RV_METADATA_RES_UNIFORM)) return true;
    if (hasMetadata(inst, rv::RV_METADATA_RES_VECTOR)) return true;
    if (hasMetadata(inst, rv::RV_METADATA_RES_SCALARS)) return true;
    if (hasMetadata(inst, rv::RV_METADATA_ALIGNED_TRUE)) return true;
    if (hasMetadata(inst, rv::RV_METADATA_ALIGNED_FALSE)) return true;
    if (hasMetadata(inst, rv::RV_METADATA_INDEX_SAME)) return true;
    if (hasMetadata(inst, rv::RV_METADATA_INDEX_CONSECUTIVE)) return true;
    if (hasMetadata(inst, rv::RV_METADATA_INDEX_SHUFFLE)) return true;
    if (hasMetadata(inst, rv::RV_METADATA_INDEX_STRIDED)) return true;
    if (hasMetadata(inst, rv::RV_METADATA_INDEX_RANDOM)) return true;
    if (hasMetadata(inst, rv::RV_METADATA_VARIANT_START)) return true;
    if (hasMetadata(inst, rv::RV_METADATA_VARIANT_END)) return true;
    if (hasMetadata(inst, rv::RV_METADATA_VARIANT_DISABLE_VECT)) return true;
    if (hasMetadata(inst, rv::RV_METADATA_VARIANT_SEQUENTIALIZE)) return true;
    if (hasMetadata(inst, rv::RV_METADATA_VARIANT_BOSCC)) return true;
    if (hasMetadata(inst, rv::RV_METADATA_MASK)) return true;
    return false;
}

bool
hasMetadata(const Instruction* inst, const char* const metaDataString)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    assert (inst);
    assert ((strcmp(metaDataString, RV_METADATA_ARGUMENT_CAST) == 0 ||
             strcmp(metaDataString, RV_METADATA_PKT_PTR_CAST) == 0 ||
             strcmp(metaDataString, RV_METADATA_BLEND_INFO) == 0 ||
             strcmp(metaDataString, RV_METADATA_PACK_UNPACK) == 0 ||
             strcmp(metaDataString, RV_METADATA_OP_UNIFORM) == 0 ||
             strcmp(metaDataString, RV_METADATA_OP_VARYING) == 0 ||
             strcmp(metaDataString, RV_METADATA_OP_SEQUENTIAL) == 0 ||
             strcmp(metaDataString, RV_METADATA_OP_SEQUENTIAL_GUARDED) == 0 ||
             strcmp(metaDataString, RV_METADATA_RES_UNIFORM) == 0 ||
             strcmp(metaDataString, RV_METADATA_RES_VECTOR) == 0 ||
             strcmp(metaDataString, RV_METADATA_RES_SCALARS) == 0 ||
             strcmp(metaDataString, RV_METADATA_ALIGNED_TRUE) == 0 ||
             strcmp(metaDataString, RV_METADATA_ALIGNED_FALSE) == 0 ||
             strcmp(metaDataString, RV_METADATA_INDEX_SAME) == 0 ||
             strcmp(metaDataString, RV_METADATA_INDEX_CONSECUTIVE) == 0 ||
             strcmp(metaDataString, RV_METADATA_INDEX_SHUFFLE) == 0 ||
             strcmp(metaDataString, RV_METADATA_INDEX_STRIDED) == 0 ||
             strcmp(metaDataString, RV_METADATA_INDEX_RANDOM) == 0 ||
             strcmp(metaDataString, RV_METADATA_VARIANT_START) == 0 ||
             strcmp(metaDataString, RV_METADATA_VARIANT_END) == 0 ||
             strcmp(metaDataString, RV_METADATA_VARIANT_DISABLE_VECT) == 0 ||
             strcmp(metaDataString, RV_METADATA_VARIANT_SEQUENTIALIZE) == 0 ||
             strcmp(metaDataString, RV_METADATA_VARIANT_BOSCC) == 0 ||
             strcmp(metaDataString, RV_METADATA_MASK) == 0) &&
            "invalid metadata for instruction found!");

    return inst->getMetadata(metaDataString);
}

void
removeMetadata(Instruction* inst, const char* const metaDataString)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    assert (inst && metaDataString);
    assert ((strcmp(metaDataString, RV_METADATA_ARGUMENT_CAST) == 0 ||
             strcmp(metaDataString, RV_METADATA_PKT_PTR_CAST) == 0 ||
             strcmp(metaDataString, RV_METADATA_BLEND_INFO) == 0 ||
             strcmp(metaDataString, RV_METADATA_PACK_UNPACK) == 0 ||
             strcmp(metaDataString, RV_METADATA_OP_UNIFORM) == 0 ||
             strcmp(metaDataString, RV_METADATA_OP_VARYING) == 0 ||
             strcmp(metaDataString, RV_METADATA_OP_SEQUENTIAL) == 0 ||
             strcmp(metaDataString, RV_METADATA_OP_SEQUENTIAL_GUARDED) == 0 ||
             strcmp(metaDataString, RV_METADATA_RES_UNIFORM) == 0 ||
             strcmp(metaDataString, RV_METADATA_RES_VECTOR) == 0 ||
             strcmp(metaDataString, RV_METADATA_RES_SCALARS) == 0 ||
             strcmp(metaDataString, RV_METADATA_ALIGNED_TRUE) == 0 ||
             strcmp(metaDataString, RV_METADATA_ALIGNED_FALSE) == 0 ||
             strcmp(metaDataString, RV_METADATA_INDEX_SAME) == 0 ||
             strcmp(metaDataString, RV_METADATA_INDEX_CONSECUTIVE) == 0 ||
             strcmp(metaDataString, RV_METADATA_INDEX_SHUFFLE) == 0 ||
             strcmp(metaDataString, RV_METADATA_INDEX_STRIDED) == 0 ||
             strcmp(metaDataString, RV_METADATA_INDEX_RANDOM) == 0 ||
             strcmp(metaDataString, RV_METADATA_VARIANT_START) == 0 ||
             strcmp(metaDataString, RV_METADATA_VARIANT_END) == 0 ||
             strcmp(metaDataString, RV_METADATA_VARIANT_DISABLE_VECT) == 0 ||
             strcmp(metaDataString, RV_METADATA_VARIANT_SEQUENTIALIZE) == 0 ||
             strcmp(metaDataString, RV_METADATA_VARIANT_BOSCC) == 0 ||
             strcmp(metaDataString, RV_METADATA_MASK) == 0) &&
            "invalid metadata for instruction found!");

    inst->setMetadata(metaDataString, nullptr);
}

void
copyMetadata(Instruction* target, const Value& source)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    assert (target);
    assert (isa<Instruction>(source) || isa<Argument>(source));

#define RV_COPY_METADATA(metaDataString) \
    if (hasMetadata(&source, metaDataString)) \
        setMetadata(target, metaDataString)

    if (isa<Instruction>(source))
    {
        RV_COPY_METADATA(RV_METADATA_OP_UNIFORM);
        RV_COPY_METADATA(RV_METADATA_OP_VARYING);
        RV_COPY_METADATA(RV_METADATA_OP_SEQUENTIAL);
        RV_COPY_METADATA(RV_METADATA_OP_SEQUENTIAL_GUARDED);
        RV_COPY_METADATA(RV_METADATA_ARGUMENT_CAST);
        RV_COPY_METADATA(RV_METADATA_PKT_PTR_CAST);
        RV_COPY_METADATA(RV_METADATA_BLEND_INFO);
        RV_COPY_METADATA(RV_METADATA_PACK_UNPACK);
        RV_COPY_METADATA(RV_METADATA_VARIANT_START);
        RV_COPY_METADATA(RV_METADATA_VARIANT_END);
        RV_COPY_METADATA(RV_METADATA_VARIANT_DISABLE_VECT);
        RV_COPY_METADATA(RV_METADATA_VARIANT_SEQUENTIALIZE);
        RV_COPY_METADATA(RV_METADATA_VARIANT_BOSCC);
    }
    RV_COPY_METADATA(RV_METADATA_RES_UNIFORM);
    RV_COPY_METADATA(RV_METADATA_RES_VECTOR);
    RV_COPY_METADATA(RV_METADATA_RES_SCALARS);
    RV_COPY_METADATA(RV_METADATA_ALIGNED_TRUE);
    RV_COPY_METADATA(RV_METADATA_ALIGNED_FALSE);
    RV_COPY_METADATA(RV_METADATA_INDEX_SAME);
    RV_COPY_METADATA(RV_METADATA_INDEX_CONSECUTIVE);
    RV_COPY_METADATA(RV_METADATA_INDEX_SHUFFLE);
    RV_COPY_METADATA(RV_METADATA_INDEX_STRIDED);
    RV_COPY_METADATA(RV_METADATA_INDEX_RANDOM);
    RV_COPY_METADATA(RV_METADATA_MASK);

#undef RV_COPY_METADATA
}

void
setMetadata(BasicBlock* block, const char* const metaDataString)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    assert (block && metaDataString);

    if (strcmp(metaDataString, RV_METADATA_MANDATORY) == 0 ||
        strcmp(metaDataString, RV_METADATA_OPTIONAL) == 0)
    {
        removeMetadata(block, RV_METADATA_MANDATORY);
        removeMetadata(block, RV_METADATA_OPTIONAL);
    }
    else if (strcmp(metaDataString, RV_METADATA_DIVERGENT_TRUE) == 0 ||
             strcmp(metaDataString, RV_METADATA_DIVERGENT_FALSE) == 0)
    {
        removeMetadata(block, RV_METADATA_DIVERGENT_TRUE);
        removeMetadata(block, RV_METADATA_DIVERGENT_FALSE);
        // Do not remove RV_METADATA_DIVERGENCE_INFO here.
        // Do not remove RV_METADATA_REWIRE_INFO here.
    }
    else if (strcmp(metaDataString, RV_METADATA_ALWAYS_BY_ALL_TRUE) == 0 ||
             strcmp(metaDataString, RV_METADATA_ALWAYS_BY_ALL_OR_NONE) == 0 ||
             strcmp(metaDataString, RV_METADATA_ALWAYS_BY_ALL_FALSE) == 0)
    {
        removeMetadata(block, RV_METADATA_ALWAYS_BY_ALL_TRUE);
        removeMetadata(block, RV_METADATA_ALWAYS_BY_ALL_OR_NONE);
        removeMetadata(block, RV_METADATA_ALWAYS_BY_ALL_FALSE);
    }
    else if (strcmp(metaDataString, RV_METADATA_LOOP_DIVERGENT_TRUE) == 0 ||
             strcmp(metaDataString, RV_METADATA_LOOP_DIVERGENT_FALSE) == 0)
    {
        removeMetadata(block, RV_METADATA_LOOP_DIVERGENT_TRUE);
        removeMetadata(block, RV_METADATA_LOOP_DIVERGENT_FALSE);
    }
    else if (strcmp(metaDataString, RV_METADATA_LOOP_TOP_LEVEL_DIVERGENT) == 0 ||
             strcmp(metaDataString, RV_METADATA_LOOP_INNERMOST_DIVERGENT) == 0)
    {
        // Nothing to remove for these.
    }
    else
    {
        assert (false && "invalid metadata for block found!");
    }

#if 0
    // This requires LLVM to support block metadata (patch refused).
	block->setMetadata(metaDataString, nullMDN);
    return;
#endif

    // If there is already metadata associated with this block, we
    // do not have to create a new metadata-call.
    if (hasMetadata(block))
    {
        // Find the call.
        CallInst* call = nullptr;
        for (BasicBlock::iterator I=block->begin(), IE=block->end(); I!=IE; ++I)
        {
            if (!isa<CallInst>(I)) continue;

            call = cast<CallInst>(I);
            if (call->getCalledFunction() == argMetadataFn) break;
        }
        assert (call);

        // Set metadata.
        call->setMetadata(metaDataString, nullMDN);

        return;
    }

    // Otherwise, create call.

    IRBuilder<> builder(block);
    SetInsertAtStart(builder, block);

	auto * blockInfoCall = builder.CreateCall(argMetadataFn, ArrayRef<Value*>(), "");
    blockInfoCall->setTailCall();
    blockInfoCall->setDoesNotAccessMemory();
    blockInfoCall->setDoesNotThrow();

    // Set metadata.
    blockInfoCall->setMetadata(metaDataString, nullMDN);
}

bool
hasMetadata(const BasicBlock* block)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    assert (block);

#if 0
    // This requires LLVM to support block metadata (patch refused).
	return block->hasMetadata();
#endif

    for (BasicBlock::const_iterator I=block->begin(), IE=block->end(); I!=IE; ++I)
    {
        if (!isa<CallInst>(I)) continue;

        const CallInst* call = cast<CallInst>(I);
        if (call->getCalledFunction() != argMetadataFn) continue;

        return call->getMetadata(RV_METADATA_MANDATORY) ||
                call->getMetadata(RV_METADATA_OPTIONAL) ||
                call->getMetadata(RV_METADATA_DIVERGENT_TRUE) ||
                call->getMetadata(RV_METADATA_DIVERGENT_FALSE) ||
                call->getMetadata(RV_METADATA_DIVERGENCE_INFO) ||
                call->getMetadata(RV_METADATA_REWIRE_INFO) ||
                call->getMetadata(RV_METADATA_ALWAYS_BY_ALL_TRUE) ||
                call->getMetadata(RV_METADATA_ALWAYS_BY_ALL_OR_NONE) ||
                call->getMetadata(RV_METADATA_ALWAYS_BY_ALL_FALSE) ||
                call->getMetadata(RV_METADATA_LOOP_DIVERGENT_TRUE) ||
                call->getMetadata(RV_METADATA_LOOP_DIVERGENT_FALSE) ||
                call->getMetadata(RV_METADATA_LOOP_TOP_LEVEL_DIVERGENT) ||
                call->getMetadata(RV_METADATA_LOOP_INNERMOST_DIVERGENT) ||
                call->getMetadata(RV_METADATA_ARGUMENT_INFO);
    }

    return false;
}

bool
hasRVMetadata(const BasicBlock* block)
{
    return hasMetadata(block);
}


bool
hasMetadata(const BasicBlock* block, const char* const metaDataString)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    assert (block);
    assert ((strcmp(metaDataString, RV_METADATA_MANDATORY) == 0 ||
             strcmp(metaDataString, RV_METADATA_OPTIONAL) == 0 ||
             strcmp(metaDataString, RV_METADATA_DIVERGENT_TRUE) == 0 ||
             strcmp(metaDataString, RV_METADATA_DIVERGENT_FALSE) == 0 ||
             strcmp(metaDataString, RV_METADATA_DIVERGENCE_INFO) == 0 ||
             strcmp(metaDataString, RV_METADATA_REWIRE_INFO) == 0 ||
             strcmp(metaDataString, RV_METADATA_ALWAYS_BY_ALL_TRUE) == 0 ||
             strcmp(metaDataString, RV_METADATA_ALWAYS_BY_ALL_OR_NONE) == 0 ||
             strcmp(metaDataString, RV_METADATA_ALWAYS_BY_ALL_FALSE) == 0 ||
             strcmp(metaDataString, RV_METADATA_LOOP_DIVERGENT_TRUE) == 0 ||
             strcmp(metaDataString, RV_METADATA_LOOP_DIVERGENT_FALSE) == 0 ||
             strcmp(metaDataString, RV_METADATA_LOOP_TOP_LEVEL_DIVERGENT) == 0 ||
             strcmp(metaDataString, RV_METADATA_LOOP_INNERMOST_DIVERGENT) == 0) &&
            "invalid metadata for block found!");

#if 0
    // This requires LLVM to support block metadata (patch refused).
	return block->getMetadata(metaDataString);
#endif

    for (BasicBlock::const_iterator I=block->begin(), IE=block->end(); I!=IE; ++I)
    {
        if (!isa<CallInst>(I)) continue;

        const CallInst* call = cast<CallInst>(I);
        if (call->getCalledFunction() != argMetadataFn) continue;

        return call->getMetadata(metaDataString);
    }

    return false;
}

void
removeMetadata(BasicBlock* block, const char* const metaDataString)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    assert (block && metaDataString);
    assert ((strcmp(metaDataString, RV_METADATA_MANDATORY) == 0 ||
             strcmp(metaDataString, RV_METADATA_OPTIONAL) == 0 ||
             strcmp(metaDataString, RV_METADATA_DIVERGENT_TRUE) == 0 ||
             strcmp(metaDataString, RV_METADATA_DIVERGENT_FALSE) == 0 ||
             strcmp(metaDataString, RV_METADATA_DIVERGENCE_INFO) == 0 ||
             strcmp(metaDataString, RV_METADATA_REWIRE_INFO) == 0 ||
             strcmp(metaDataString, RV_METADATA_ALWAYS_BY_ALL_TRUE) == 0 ||
             strcmp(metaDataString, RV_METADATA_ALWAYS_BY_ALL_OR_NONE) == 0 ||
             strcmp(metaDataString, RV_METADATA_ALWAYS_BY_ALL_FALSE) == 0 ||
             strcmp(metaDataString, RV_METADATA_LOOP_DIVERGENT_TRUE) == 0 ||
             strcmp(metaDataString, RV_METADATA_LOOP_DIVERGENT_FALSE) == 0 ||
             strcmp(metaDataString, RV_METADATA_LOOP_TOP_LEVEL_DIVERGENT) == 0 ||
             strcmp(metaDataString, RV_METADATA_LOOP_INNERMOST_DIVERGENT) == 0) &&
            "invalid metadata for block found!");

#if 0
    // This requires LLVM to support block metadata (patch refused).
	block->setMetadata(metaDataString, nullptr);
#endif

    for (BasicBlock::iterator I=block->begin(), IE=block->end(); I!=IE; ++I)
    {
        if (!isa<CallInst>(I)) continue;

        CallInst* call = cast<CallInst>(I);
        if (call->getCalledFunction() != argMetadataFn) continue;

        call->setMetadata(metaDataString, nullptr);

        // If there is no metadata anymore, remove the call.
        if (!call->hasMetadata())
        {
            call->eraseFromParent();
        }

        return;
    }
}

void
copyMetadata(BasicBlock* target, const BasicBlock& source)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    assert (target);

#define RV_COPY_METADATA(metaDataString) \
    if (hasMetadata(&source, metaDataString)) \
        setMetadata(target, metaDataString)

    RV_COPY_METADATA(RV_METADATA_OPTIONAL);
    RV_COPY_METADATA(RV_METADATA_MANDATORY);
    RV_COPY_METADATA(RV_METADATA_DIVERGENT_TRUE);
    RV_COPY_METADATA(RV_METADATA_DIVERGENT_FALSE);
    RV_COPY_METADATA(RV_METADATA_ALWAYS_BY_ALL_TRUE);
    RV_COPY_METADATA(RV_METADATA_ALWAYS_BY_ALL_OR_NONE);
    RV_COPY_METADATA(RV_METADATA_ALWAYS_BY_ALL_FALSE);
    RV_COPY_METADATA(RV_METADATA_LOOP_DIVERGENT_TRUE);
    RV_COPY_METADATA(RV_METADATA_LOOP_DIVERGENT_FALSE);
    RV_COPY_METADATA(RV_METADATA_LOOP_INNERMOST_DIVERGENT);
    RV_COPY_METADATA(RV_METADATA_LOOP_TOP_LEVEL_DIVERGENT);

#undef RV_COPY_METADATA
}

void
setMetadata(Argument* arg, const char* const metaDataString)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    assert (arg && metaDataString);

    if (strcmp(metaDataString, RV_METADATA_RES_UNIFORM) == 0 ||
        strcmp(metaDataString, RV_METADATA_RES_VECTOR) == 0 ||
        strcmp(metaDataString, RV_METADATA_RES_SCALARS) == 0)
    {
        removeMetadata(arg, RV_METADATA_RES_UNIFORM);
        removeMetadata(arg, RV_METADATA_RES_VECTOR);
        removeMetadata(arg, RV_METADATA_RES_SCALARS);
    }
    else if (strcmp(metaDataString, RV_METADATA_ALIGNED_TRUE) == 0 ||
             strcmp(metaDataString, RV_METADATA_ALIGNED_FALSE) == 0)
    {
        removeMetadata(arg, RV_METADATA_ALIGNED_TRUE);
        removeMetadata(arg, RV_METADATA_ALIGNED_FALSE);
    }
    else if (strcmp(metaDataString, RV_METADATA_INDEX_SAME) == 0 ||
             strcmp(metaDataString, RV_METADATA_INDEX_CONSECUTIVE) == 0 ||
             strcmp(metaDataString, RV_METADATA_INDEX_SHUFFLE) == 0 ||
             strcmp(metaDataString, RV_METADATA_INDEX_STRIDED) == 0 ||
             strcmp(metaDataString, RV_METADATA_INDEX_RANDOM) == 0)
    {
        removeMetadata(arg, RV_METADATA_INDEX_SAME);
        removeMetadata(arg, RV_METADATA_INDEX_CONSECUTIVE);
        removeMetadata(arg, RV_METADATA_INDEX_SHUFFLE);
        removeMetadata(arg, RV_METADATA_INDEX_STRIDED);
        removeMetadata(arg, RV_METADATA_INDEX_RANDOM);
    }
    else
    {
        assert (strcmp(metaDataString, RV_METADATA_MASK) == 0 &&
                "invalid metadata for argument found!");
    }

    //arg->setMetadata(metaDataString, nullMDN);
    //return;

    const unsigned argIndex = arg->getArgNo();

    // If there is already metadata associated with this block, we
    // do not have to create a new metadata-call.
    CallInst* argInfoCall = nullptr;
    BasicBlock* block = &arg->getParent()->getEntryBlock();
    for (BasicBlock::iterator I=block->begin(), IE=block->end(); I!=IE; ++I)
    {
        if (!isa<CallInst>(I)) continue;

        const CallInst* call = cast<CallInst>(I);
        if (call->getCalledFunction() != argMetadataFn) continue;

        argInfoCall = cast<CallInst>(I);
        break;
    }

    if (argInfoCall)
    {
        assert (argInfoCall->hasMetadata());
        // Get generic argument-metadata node of this function.
        MDNode* argInfo = argInfoCall->getMetadata(rv::RV_METADATA_ARGUMENT_INFO);

        // There might be no argument metadata yet (= there is only block metadata),
        // So we have to create it.
        // TODO: Remove code duplication.
        if (!argInfo)
        {
            // Create node referencing 'metaDataString' for the current argument.
            // Create empty nodes for all other arguments.
            // Each node has a string with function name plus argument index as
            // first metadata element. This is required to prevent multiple
            // arguments pointing to the same metadata.
            SmallVector<Metadata*, 2> argInfos;
            for (unsigned i=0, e=arg->getParent()->getArgumentList().size(); i!=e; ++i)
            {
                MDNode* argNode = nullptr;
                if (i == argIndex)
                {
                    MDString* newMetaData = MDString::get(arg->getContext(), metaDataString);
                    std::string id = arg->getParent()->getName().str() + str<int>(i);
                    MDString* idMetaData = MDString::get(arg->getContext(), id);
                    Metadata* mdArray[2] = { idMetaData, newMetaData };
                    argNode = MDNode::get(arg->getContext(), mdArray);
                }
                else
                {
                    argNode = nullMDN;
                }
                argInfos.push_back(argNode);
            }

            // Create and store generic argument-metadata node for this function.
            argInfo = MDNode::get(arg->getContext(), ArrayRef<Metadata*>(argInfos));
            argInfoCall->setMetadata(rv::RV_METADATA_ARGUMENT_INFO, argInfo);

            return;
        }

        assert (argInfo->getNumOperands() == arg->getParent()->getArgumentList().size());
        assert (isa<MDNode>(argInfo->getOperand(argIndex)));

        // Get metadata for the requested argument.
        MDNode* argNode = cast<MDNode>(argInfo->getOperand(argIndex));

        // Create new vector of metadata for this argument from old vector
        // plus new metadata.
        // If argNode is the null-metadata, there is no metadata attached to this argument
        // yet, so we attach the id metadata before the current one.
        SmallVector<Metadata*, 2> argInfos;
        if (argNode != nullMDN)
        {
            for (unsigned i=0, e=argNode->getNumOperands(); i!=e; ++i)
            {
                argInfos.push_back(argNode->getOperand(i));
            }
        }
        else
        {
            std::string id = arg->getParent()->getName().str() + str<int>(argIndex);
            MDString* idMetaData = MDString::get(arg->getContext(), id);
            argInfos.push_back(idMetaData);
        }
        argInfos.push_back(MDString::get(arg->getContext(), metaDataString));

        // Create new argNode.
        MDNode* newArgNode = MDNode::get(arg->getContext(), ArrayRef<Metadata*>(argInfos));

        // Otherwise, find the requested metadata is in the list.
        // Create a new vector of metadata with all elements except the one to be removed.
        argInfos.clear();
        bool replaced = false; RV_UNUSED(replaced);
        for (unsigned i=0, e=argInfo->getNumOperands(); i!=e; ++i)
        {
            Metadata* metadata = argInfo->getOperand(i);
            if (i == argIndex)
            {
                argInfos.push_back(newArgNode);
                replaced = true;
            }
            else
            {
                argInfos.push_back(metadata);
            }
        }

        assert (replaced);

        // Create new argInfo and replace uses of old one.
        MDNode* newArgInfo = MDNode::get(arg->getContext(), ArrayRef<Metadata*>(argInfos));
        assert (newArgInfo != argInfo);

        argInfoCall->setMetadata(rv::RV_METADATA_ARGUMENT_INFO, newArgInfo);

        return;
    }

    // Otherwise, create call.

    IRBuilder<> builder(block);
    SetInsertAtStart(builder, block);
    argInfoCall = builder.CreateCall(argMetadataFn,
    							   ArrayRef<Value*>(),
                                   "");
    argInfoCall->setTailCall();
    argInfoCall->setDoesNotAccessMemory();
    argInfoCall->setDoesNotThrow();

    // Create node referencing 'id' and 'metaDataString' for the current argument.
    // Create empty nodes for all other arguments.
    SmallVector<Metadata*, 2> argInfos;
    for (unsigned i=0, e=arg->getParent()->getArgumentList().size(); i!=e; ++i)
    {
        MDNode* argNode = nullptr;
        if (i == argIndex)
        {
            MDString* newMetaData = MDString::get(arg->getContext(), metaDataString);
            std::string id = arg->getParent()->getName().str() + str<int>(argIndex);
            MDString* idMetaData = MDString::get(arg->getContext(), id);
            Metadata* mdArray[2] = { idMetaData, newMetaData };
            argNode = MDNode::get(arg->getContext(), mdArray);
        }
        else
        {
            argNode = nullMDN;
        }
        argInfos.push_back(argNode);
    }

    // Create and store generic argument-metadata node for this function.
    MDNode* argInfo = MDNode::get(arg->getContext(), ArrayRef<Metadata*>(argInfos));
    argInfoCall->setMetadata(rv::RV_METADATA_ARGUMENT_INFO, argInfo);
}

bool
hasMetadata(const Argument* arg)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    assert (arg);
    //return arg->hasMetadata();

    const unsigned argIndex = arg->getArgNo();

    const CallInst* argInfoCall = nullptr;
    const BasicBlock* block = &arg->getParent()->getEntryBlock();
    for (BasicBlock::const_iterator I=block->begin(), IE=block->end(); I!=IE; ++I)
    {
        if (!isa<CallInst>(I)) continue;

        const CallInst* call = cast<CallInst>(I);
        if (call->getCalledFunction() != argMetadataFn) continue;

        argInfoCall = cast<CallInst>(I);
        break;
    }

    // If there is no call, then there is no metadata for any argument.
    if (!argInfoCall) return false;

    assert (argInfoCall->hasMetadata());
    // Get generic argument-metadata node of this function.
    MDNode* argInfo = argInfoCall->getMetadata(rv::RV_METADATA_ARGUMENT_INFO);

    // There might be no argument metadata (= there is only block metadata).
    if (!argInfo) return false;

    assert (argInfo->getNumOperands() == arg->getParent()->getArgumentList().size());
    assert (isa<MDNode>(argInfo->getOperand(argIndex)));

    // Get metadata for the requested argument.
    // If the node is empty (except for id string), there is no metadata for this argument.
    return argInfo->getOperand(argIndex) != nullMDN;
}

bool
hasRVMetadata(const Argument* arg)
{
    return hasMetadata(arg);
}

bool
hasMetadata(const Argument* arg, const char* const metaDataString)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    assert (arg);
    assert ((strcmp(metaDataString, RV_METADATA_RES_UNIFORM) == 0 ||
             strcmp(metaDataString, RV_METADATA_RES_VECTOR) == 0 ||
             strcmp(metaDataString, RV_METADATA_RES_SCALARS) == 0 ||
             strcmp(metaDataString, RV_METADATA_ALIGNED_TRUE) == 0 ||
             strcmp(metaDataString, RV_METADATA_ALIGNED_FALSE) == 0 ||
             strcmp(metaDataString, RV_METADATA_INDEX_SAME) == 0 ||
             strcmp(metaDataString, RV_METADATA_INDEX_CONSECUTIVE) == 0 ||
             strcmp(metaDataString, RV_METADATA_INDEX_SHUFFLE) == 0 ||
             strcmp(metaDataString, RV_METADATA_INDEX_STRIDED) == 0 ||
             strcmp(metaDataString, RV_METADATA_INDEX_RANDOM) == 0 ||
             strcmp(metaDataString, RV_METADATA_MASK) == 0) &&
            "invalid metadata for argument found!");

    //return arg->getMetadata(metaDataString);

    const unsigned argIndex = arg->getArgNo();

    const CallInst* argInfoCall = nullptr;
    const BasicBlock* block = &arg->getParent()->getEntryBlock();
    for (BasicBlock::const_iterator I=block->begin(), IE=block->end(); I!=IE; ++I)
    {
        if (!isa<CallInst>(I)) continue;

        const CallInst* call = cast<CallInst>(I);
        if (call->getCalledFunction() != argMetadataFn) continue;

        argInfoCall = cast<CallInst>(I);
        break;
    }

    // If there is no call, then there is no metadata for any argument.
    if (!argInfoCall) return false;

    assert (argInfoCall->hasMetadata());
    // Get generic argument-metadata node of this function.
    MDNode* argInfo = argInfoCall->getMetadata(rv::RV_METADATA_ARGUMENT_INFO);

    // There might be no argument metadata (= there is only block metadata).
    if (!argInfo) return false;

    assert (argInfo->getNumOperands() == arg->getParent()->getArgumentList().size());
    assert (isa<MDNode>(argInfo->getOperand(argIndex)));

    // Get metadata for the requested argument.
    MDNode* argNode = cast<MDNode>(argInfo->getOperand(argIndex));

    // If argNode is the null-metadata, there is no metadata attached to this argument.
    if (argNode == nullMDN) return false;

    assert (argNode->getNumOperands() > 1 && "expected id string plus at least one metadata");

    // Otherwise, look if the requested metadata is in the list.
    for (unsigned i=1, e=argNode->getNumOperands(); i!=e; ++i)
    {
        Metadata* metadata = argNode->getOperand(i);
        assert (isa<MDString>(metadata));
        if (cast<MDString>(metadata)->getString().equals(metaDataString))
        {
            return true;
        }
    }

    return false;
}

void
removeMetadata(Argument* arg, const char* const metaDataString)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    assert (arg && metaDataString);
    assert ((strcmp(metaDataString, RV_METADATA_RES_UNIFORM) == 0 ||
             strcmp(metaDataString, RV_METADATA_RES_VECTOR) == 0 ||
             strcmp(metaDataString, RV_METADATA_RES_SCALARS) == 0 ||
             strcmp(metaDataString, RV_METADATA_ALIGNED_TRUE) == 0 ||
             strcmp(metaDataString, RV_METADATA_ALIGNED_FALSE) == 0 ||
             strcmp(metaDataString, RV_METADATA_INDEX_SAME) == 0 ||
             strcmp(metaDataString, RV_METADATA_INDEX_CONSECUTIVE) == 0 ||
             strcmp(metaDataString, RV_METADATA_INDEX_SHUFFLE) == 0 ||
             strcmp(metaDataString, RV_METADATA_INDEX_STRIDED) == 0 ||
             strcmp(metaDataString, RV_METADATA_INDEX_RANDOM) == 0 ||
             strcmp(metaDataString, RV_METADATA_MASK) == 0) &&
            "invalid metadata for argument found!");

    //arg->setMetadata(metaDataString, nullptr);
    //return;

    const unsigned argIndex = arg->getArgNo();

    CallInst* argInfoCall = nullptr;
    BasicBlock* block = &arg->getParent()->getEntryBlock();
    for (auto &I : *block)
    {
        if (!isa<CallInst>(&I)) continue;

        CallInst* call = cast<CallInst>(&I);
        if (call->getCalledFunction() != argMetadataFn) continue;

        argInfoCall = call;
        break;
    }

    // If there is no call, then there is no metadata that could be removed.
    if (!argInfoCall) return;

    assert (argInfoCall->hasMetadata());
    // Get generic argument-metadata node of this function.
    MDNode* argInfo = argInfoCall->getMetadata(rv::RV_METADATA_ARGUMENT_INFO);

    // If there is no argument metadata, then we cannot remove anything
    // (= there is only block metadata).
    if (!argInfo) return;

    assert (argInfo->getNumOperands() == arg->getParent()->getArgumentList().size());
    assert (isa<MDNode>(argInfo->getOperand(argIndex)));

    // Get metadata for the requested argument.
    MDNode* argNode = cast<MDNode>(argInfo->getOperand(argIndex));

    // If argNode is the null-metadata, there is no metadata attached to this argument.
    if (argNode == nullMDN) return;

    assert (argNode->getNumOperands() > 1 && "expected id string plus at least one metadata");

    // Otherwise, find the requested metadata in the list.
    // Create a new vector of metadata with all elements except the one to be removed.
    SmallVector<Metadata*, 2> newArgNodeInfos;
    bool removed = false;
    for (unsigned i=0, e=argNode->getNumOperands(); i!=e; ++i)
    {
        assert (isa<MDString>(argNode->getOperand(i)));
        if (cast<MDString>(argNode->getOperand(i))->getString().equals(metaDataString))
        {
            removed = true;
            continue;
        }
        newArgNodeInfos.push_back(argNode->getOperand(i));
    }

    if (!removed) return;

    assert (!newArgNodeInfos.empty() && "should always contain argument identifier mdnode!");

    const bool noMoreMetadata = newArgNodeInfos.size() == 1;

    // If there is no metadata for this argument anymore, replace by nullMDN.
    if (noMoreMetadata)
    {
        argInfo->replaceOperandWith(argIndex, nullMDN);

        // For some reason, when executing WFVIndexAndAlignmentAnalysisTest.TestAlloca2
        // in the test suite (not standalone via gtest_filter), argInfo sometimes is
        // invalidated after all operands were replaced by nullMDN. Thus, we simply
        // reset it to prevent this.
        // TODO: Fix the actual source of the problem.
        argInfo = argInfoCall->getMetadata(rv::RV_METADATA_ARGUMENT_INFO);

        // If no argument has metadata anymore, remove the arg info metadata from the call.
        bool hasArgWithMetadata = false;
        for (unsigned i=0, e=arg->getParent()->getArgumentList().size(); i!=e; ++i)
        {
            if (argInfo->getOperand(i) != nullMDN)
            {
                hasArgWithMetadata = true;
                break;
            }
        }
        if (!hasArgWithMetadata)
        {
            argInfoCall->setMetadata(rv::RV_METADATA_ARGUMENT_INFO, nullptr);

            // If there is no metadata (also no block metadata) anymore, remove the call.
            if (!argInfoCall->hasMetadata())
            {
                argInfoCall->eraseFromParent();
            }
        }

        return;
    }

    // Otherwise, create new argNode and replace uses of old one.
    MDNode* newArgNode = MDNode::get(arg->getContext(), ArrayRef<Metadata*>(newArgNodeInfos));
    assert (newArgNode != argNode);

    argInfo->replaceOperandWith(argIndex, newArgNode);
}

void
copyMetadata(Argument* target, const Value& source)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    assert (target);

#define RV_COPY_METADATA(metaDataString) \
    if (hasMetadata(&source, metaDataString)) \
        setMetadata(target, metaDataString)

    RV_COPY_METADATA(RV_METADATA_RES_UNIFORM);
    RV_COPY_METADATA(RV_METADATA_RES_VECTOR);
    RV_COPY_METADATA(RV_METADATA_RES_SCALARS);
    RV_COPY_METADATA(RV_METADATA_ALIGNED_TRUE);
    RV_COPY_METADATA(RV_METADATA_ALIGNED_FALSE);
    RV_COPY_METADATA(RV_METADATA_INDEX_SAME);
    RV_COPY_METADATA(RV_METADATA_INDEX_CONSECUTIVE);
    RV_COPY_METADATA(RV_METADATA_INDEX_SHUFFLE);
    RV_COPY_METADATA(RV_METADATA_INDEX_STRIDED);
    RV_COPY_METADATA(RV_METADATA_INDEX_RANDOM);
    RV_COPY_METADATA(RV_METADATA_MASK);

#undef RV_COPY_METADATA
}


void
setMetadata(Value* value, const char* const metaDataString)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    assert (value);

    if (Instruction* inst = dyn_cast<Instruction>(value))
    {
        setMetadata(inst, metaDataString);
    }
    else if (Argument* arg = dyn_cast<Argument>(value))
    {
        setMetadata(arg, metaDataString);
    }
    else if (BasicBlock* block = dyn_cast<BasicBlock>(value))
    {
        setMetadata(block, metaDataString);
    }
    else
    {
        assert (false && "only arguments, instructions, and blocks can store metadata!");
    }
}

bool
hasMetadata(const Value* value)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    assert (value);

    if (const Instruction* inst = dyn_cast<Instruction>(value))
    {
        return hasMetadata(inst);
    }
    else if (const Argument* arg = dyn_cast<Argument>(value))
    {
        return hasMetadata(arg);
    }
    else if (const BasicBlock* block = dyn_cast<BasicBlock>(value))
    {
        return hasMetadata(block);
    }

    assert (false && "only arguments, instructions, and blocks can store metadata!");
    return false;
}

bool
hasRVMetadata(const Value* value)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    assert (value);

    if (const Instruction* inst = dyn_cast<Instruction>(value))
    {
        return hasRVMetadata(inst);
    }
    else if (const Argument* arg = dyn_cast<Argument>(value))
    {
        return hasRVMetadata(arg);
    }
    else if (const BasicBlock* block = dyn_cast<BasicBlock>(value))
    {
        return hasRVMetadata(block);
    }

    assert (false && "only arguments, instructions, and blocks can store metadata!");
    return false;
}

bool
hasMetadata(const Value* value, const char* const metaDataString)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    assert (value && metaDataString);


    if (const Instruction* inst = dyn_cast<Instruction>(value)){
        return hasMetadata(inst, metaDataString);

    } else if (const Argument* arg = dyn_cast<Argument>(value)) {
        return hasMetadata(arg, metaDataString);

    } else if (const BasicBlock* block = dyn_cast<BasicBlock>(value)) {
        return hasMetadata(block, metaDataString);

    } else if (isa<GlobalVariable>(value)) {
		// default metadata for GlobalVariables
    	return !strcmp(metaDataString, RV_METADATA_RES_UNIFORM);
	}


    assert (false && "only arguments, instructions, and blocks can store metadata!");
    return false;
}

void
removeMetadata(Value* value, const char* const metaDataString)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    assert (value && metaDataString);

    if (Instruction* inst = dyn_cast<Instruction>(value))
    {
        removeMetadata(inst, metaDataString);
    }
    else if (Argument* arg = dyn_cast<Argument>(value))
    {
        removeMetadata(arg, metaDataString);
    }
    else if (BasicBlock* block = dyn_cast<BasicBlock>(value))
    {
        removeMetadata(block, metaDataString);
    }
    else
    {
        assert (false && "only arguments, instructions, and blocks can store metadata!");
    }
}

void
copyMetadata(Value* value, const Value& source)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    assert (value);

    if (Instruction* inst = dyn_cast<Instruction>(value))
    {
        copyMetadata(inst, source);
    }
    else if (Argument* arg = dyn_cast<Argument>(value))
    {
        copyMetadata(arg, source);
    }
    else if (BasicBlock* block = dyn_cast<BasicBlock>(value))
    {
        assert (isa<BasicBlock>(source));
        copyMetadata(block, cast<BasicBlock>(source));
    }
    else
    {
        assert (false && "only arguments, instructions, and blocks can store metadata!");
    }
}


void
setMetadata(Loop* loop, const char* const metaDataString)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    assert (loop && metaDataString);
    assert ((strcmp(metaDataString, RV_METADATA_LOOP_DIVERGENT_TRUE) == 0 ||
             strcmp(metaDataString, RV_METADATA_LOOP_DIVERGENT_FALSE) == 0 ||
             strcmp(metaDataString, RV_METADATA_LOOP_TOP_LEVEL_DIVERGENT) == 0 ||
             strcmp(metaDataString, RV_METADATA_LOOP_INNERMOST_DIVERGENT) == 0) &&
            "loop metadata must be 'divergent' or 'non_divergent'!");

    if (hasMetadata(loop, RV_METADATA_LOOP_DIVERGENT_TRUE) ||
        hasMetadata(loop, RV_METADATA_LOOP_DIVERGENT_FALSE))
    {
        removeMetadata(loop, RV_METADATA_LOOP_DIVERGENT_TRUE);
        removeMetadata(loop, RV_METADATA_LOOP_DIVERGENT_FALSE);
    }

    BasicBlock* header = loop->getHeader();
    setMetadata(header, metaDataString);
}

bool
hasMetadata(const Loop* loop)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    assert (loop);

    const BasicBlock* header = loop->getHeader();

    return hasMetadata(header, RV_METADATA_LOOP_DIVERGENT_TRUE) ||
            hasMetadata(header, RV_METADATA_LOOP_DIVERGENT_FALSE) ||
            hasMetadata(header, RV_METADATA_LOOP_TOP_LEVEL_DIVERGENT) ||
            hasMetadata(header, RV_METADATA_LOOP_INNERMOST_DIVERGENT);
}

bool
hasMetadata(const Loop* loop, const char* const metaDataString)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    assert (loop);

    const BasicBlock* header = loop->getHeader();
    return hasMetadata(header, metaDataString);
}

void
removeMetadata(Loop* loop, const char* const metaDataString)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    assert (loop && metaDataString);
    assert ((strcmp(metaDataString, RV_METADATA_LOOP_DIVERGENT_TRUE) == 0 ||
             strcmp(metaDataString, RV_METADATA_LOOP_DIVERGENT_FALSE) == 0 ||
             strcmp(metaDataString, RV_METADATA_LOOP_TOP_LEVEL_DIVERGENT) == 0 ||
             strcmp(metaDataString, RV_METADATA_LOOP_INNERMOST_DIVERGENT) == 0) &&
            "loop metadata must be 'divergent' or 'non_divergent'!");

    BasicBlock* header = loop->getHeader();
    removeMetadata(header, metaDataString);
}

BasicBlock*
getIncomingBlockTrue(const SelectInst& select)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    const MDNode* metaInfo = select.getMetadata(rv::RV_METADATA_BLEND_INFO);

    if (!metaInfo) return nullptr;

    assert (isa<BasicBlock>(getV(metaInfo->getOperand(0))));

    return cast<BasicBlock>(getV(metaInfo->getOperand(0)));
}

BasicBlock*
getIncomingBlockFalse(const SelectInst& select)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    const MDNode* metaInfo = select.getMetadata(rv::RV_METADATA_BLEND_INFO);

    if (!metaInfo) return nullptr;
    if (!metaInfo->getOperand(1)) return nullptr; // Loop result blend

    assert (isa<BasicBlock>(getV(metaInfo->getOperand(1))));

    return cast<BasicBlock>(getV(metaInfo->getOperand(1)));
}

#if 0
void
setMetadataForDivergentBlock(BasicBlock*             block,
                             SmallVector<Value*, 2>& divergenceCausingBlocks)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    assert (block && !divergenceCausingBlocks.empty());

    SmallVector<Metadata*,2 > divCauseBlocksMD;
    for (Value * v : divergenceCausingBlocks) {
    	divCauseBlocksMD.push_back(ValueAsMetadata::get(v));
    }
    MDNode* metaInfo = MDNode::get(block->getContext(),
                                   ArrayRef<Metadata*>(divCauseBlocksMD));

    // If there is already metadata associated with this block, we
    // do not have to create a new metadata-call.
    if (hasMetadata(block))
    {
        // Find the call.
        CallInst* call = nullptr;
        for (BasicBlock::iterator I=block->begin(), IE=block->end(); I!=IE; ++I)
        {
            if (!isa<CallInst>(I)) continue;

            call = cast<CallInst>(I);
            if (call->getCalledFunction() == argMetadataFn) break;
        }
        assert (call);

        // Set metadata.
        call->setMetadata(rv::RV_METADATA_DIVERGENCE_INFO, metaInfo);

        return;
    }

    // Otherwise, create call.

    IRBuilder<> builder(block);
    SetInsertAtStart(builder, block);
    auto * blockInfoCall = builder.CreateCall(argMetadataFn, ArrayRef<Value*>(), "");

    blockInfoCall->setTailCall();
    blockInfoCall->setDoesNotAccessMemory();
    blockInfoCall->setDoesNotThrow();

    // Set metadata.
    blockInfoCall->setMetadata(rv::RV_METADATA_DIVERGENCE_INFO, metaInfo);
}

void
getDivergenceCausingBlocks(const BasicBlock&       block,
                           SmallVector<Value*, 2>& blocks)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    const MDNode* metaInfo = nullptr;

    for (const auto &I : block)
    {
        if (!isa<CallInst>(I)) continue;

        const CallInst& call = cast<CallInst>(I);
        if (call.getCalledFunction() != argMetadataFn) continue;

        metaInfo = call.getMetadata(rv::RV_METADATA_DIVERGENCE_INFO);
    }

    if (!metaInfo) return;

    for (unsigned i=0, e=metaInfo->getNumOperands(); i<e; ++i)
    {
        assert (isa<BasicBlock>(getV(metaInfo->getOperand(i))));
        blocks.push_back(getV(metaInfo->getOperand(i)));
    }
}

void
addRewireTargetForDCBlock(BasicBlock* divergenceCausingBlock,
                          BasicBlock* rewireTarget)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    assert (divergenceCausingBlock && rewireTarget);

    // If there is already metadata associated with this block, we
    // do not have to create a new metadata-call.
    if (hasMetadata(divergenceCausingBlock))
    {
        // Find the call.
        CallInst* call = nullptr;
        for (BasicBlock::iterator I=divergenceCausingBlock->begin(),
             IE=divergenceCausingBlock->end(); I!=IE; ++I)
        {
            if (!isa<CallInst>(I)) continue;

            call = cast<CallInst>(I);
            if (call->getCalledFunction() == argMetadataFn) break;
        }
        assert (call);

        // Check if there are already rewire targets set.
        // Create new array with old targets (if any) and new target.
        MDNode* metaInfo = call->getMetadata(rv::RV_METADATA_REWIRE_INFO);
        if (!metaInfo)
        {
            metaInfo = MDNode::get(divergenceCausingBlock->getContext(),
                                   ArrayRef<Metadata*>(ValueAsMetadata::get(rewireTarget)));
        }
        else
        {
            // Make sure we add each target only once.
            SmallPtrSet<Value*, 2> alreadyIn;
            SmallVector<Metadata*, 2> rewireTargets;
            for (unsigned i=0, e=metaInfo->getNumOperands(); i<e; ++i)
            {
                Value* op = getV(metaInfo->getOperand(i));
                assert (isa<BasicBlock>(op));
                assert (!alreadyIn.count(op));
                alreadyIn.insert(op);
                rewireTargets.push_back(ValueAsMetadata::get(op));
            }
            if (!alreadyIn.count(rewireTarget)) rewireTargets.push_back(ValueAsMetadata::get(rewireTarget));
            metaInfo = MDNode::get(divergenceCausingBlock->getContext(),
                                   ArrayRef<Metadata*>(rewireTargets));
        }

        call->setMetadata(rv::RV_METADATA_REWIRE_INFO, metaInfo);

        return;
    }

    // Otherwise, create call.

    IRBuilder<> builder(divergenceCausingBlock);
    SetInsertAtStart(builder, divergenceCausingBlock);

    auto * blockInfoCall = builder.CreateCall(argMetadataFn,
        								 ArrayRef<Value*>(),
                                         "");

    blockInfoCall->setTailCall();
    blockInfoCall->setDoesNotAccessMemory();
    blockInfoCall->setDoesNotThrow();

    // Set metadata.
    MDNode* metaInfo = MDNode::get(divergenceCausingBlock->getContext(),
                                   ArrayRef<Metadata*>(ValueAsMetadata::get(rewireTarget)));
    blockInfoCall->setMetadata(rv::RV_METADATA_REWIRE_INFO, metaInfo);
}

void
getRewireTargetsOfDCBlock(const BasicBlock&       divergenceCausingBlock,
                          SmallVector<Value*, 2>& rewireTargets)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    const MDNode* metaInfo = nullptr;

    for (const auto &I : divergenceCausingBlock)
    {
        if (!isa<CallInst>(I)) continue;

        const CallInst& call = cast<CallInst>(I);
        if (call.getCalledFunction() != argMetadataFn) continue;

        metaInfo = call.getMetadata(rv::RV_METADATA_REWIRE_INFO);
    }

    if (!metaInfo) return;

    for (unsigned i=0, e=metaInfo->getNumOperands(); i<e; ++i)
    {
        assert (isa<BasicBlock>(getV(metaInfo->getOperand(i))));
        rewireTargets.push_back(getV(metaInfo->getOperand(i)));
    }
}

#endif

void
markMaskOperation(Instruction* maskOp)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    assert (maskOp);

    // Mark new mask operation as OP_VARYING/RES_VECTOR/MASK
    // or OP_UNIFORM/RES_VECTOR/MASK in case of a phi.
    rv::setMetadata(maskOp, isa<PHINode>(maskOp) ?
        rv::RV_METADATA_OP_UNIFORM :
        rv::RV_METADATA_OP_VARYING);
    rv::setMetadata(maskOp, rv::RV_METADATA_RES_VECTOR);
    rv::setMetadata(maskOp, rv::RV_METADATA_MASK);
}

const char*
getVariantMetadata(const Instruction* inst)
{
    assert (inst);

    if (inst->getMetadata(RV_METADATA_VARIANT_DISABLE_VECT))
        return RV_METADATA_VARIANT_DISABLE_VECT;
    if (inst->getMetadata(RV_METADATA_VARIANT_SEQUENTIALIZE))
        return RV_METADATA_VARIANT_SEQUENTIALIZE;
    if (inst->getMetadata(RV_METADATA_VARIANT_BOSCC))
        return RV_METADATA_VARIANT_BOSCC;

    return nullptr;
}

void
removeAllMetadata(Instruction* inst)
{
    removeMetadata(inst, RV_METADATA_ARGUMENT_CAST);
    removeMetadata(inst, RV_METADATA_PKT_PTR_CAST);
    removeMetadata(inst, RV_METADATA_BLEND_INFO);
    removeMetadata(inst, RV_METADATA_PACK_UNPACK);
    removeMetadata(inst, RV_METADATA_OP_UNIFORM);
    removeMetadata(inst, RV_METADATA_OP_VARYING);
    removeMetadata(inst, RV_METADATA_OP_SEQUENTIAL);
    removeMetadata(inst, RV_METADATA_OP_SEQUENTIAL_GUARDED);
    removeMetadata(inst, RV_METADATA_RES_UNIFORM);
    removeMetadata(inst, RV_METADATA_RES_VECTOR);
    removeMetadata(inst, RV_METADATA_RES_SCALARS);
    removeMetadata(inst, RV_METADATA_ALIGNED_TRUE);
    removeMetadata(inst, RV_METADATA_ALIGNED_FALSE);
    removeMetadata(inst, RV_METADATA_INDEX_SAME);
    removeMetadata(inst, RV_METADATA_INDEX_CONSECUTIVE);
    removeMetadata(inst, RV_METADATA_INDEX_SHUFFLE);
    removeMetadata(inst, RV_METADATA_INDEX_STRIDED);
    removeMetadata(inst, RV_METADATA_INDEX_RANDOM);
    removeMetadata(inst, RV_METADATA_VARIANT_START);
    removeMetadata(inst, RV_METADATA_VARIANT_END);
    removeMetadata(inst, RV_METADATA_VARIANT_DISABLE_VECT);
    removeMetadata(inst, RV_METADATA_VARIANT_SEQUENTIALIZE);
    removeMetadata(inst, RV_METADATA_VARIANT_BOSCC);
    removeMetadata(inst, RV_METADATA_MASK);
}

void
removeAllMetadata(Function* f)
{
    assert (isMetadataSetUp() && "metadata not initialized, call setUpMetadata() first!");
    assert (f);

    for (auto &BB : *f)
    {
        // Remove instruction metadata.
        for (auto &I : BB)
        {
            removeAllMetadata(&I);
        }

        // Remove block metadata.
        removeMetadata(&BB, RV_METADATA_MANDATORY);
        removeMetadata(&BB, RV_METADATA_OPTIONAL);
        removeMetadata(&BB, RV_METADATA_DIVERGENT_TRUE);
        removeMetadata(&BB, RV_METADATA_DIVERGENT_FALSE);
        removeMetadata(&BB, RV_METADATA_DIVERGENCE_INFO);
        removeMetadata(&BB, RV_METADATA_REWIRE_INFO);
        removeMetadata(&BB, RV_METADATA_ALWAYS_BY_ALL_TRUE);
        removeMetadata(&BB, RV_METADATA_ALWAYS_BY_ALL_OR_NONE);
        removeMetadata(&BB, RV_METADATA_ALWAYS_BY_ALL_FALSE);
        removeMetadata(&BB, RV_METADATA_LOOP_DIVERGENT_TRUE);
        removeMetadata(&BB, RV_METADATA_LOOP_DIVERGENT_FALSE);
        removeMetadata(&BB, RV_METADATA_LOOP_TOP_LEVEL_DIVERGENT);
        removeMetadata(&BB, RV_METADATA_LOOP_INNERMOST_DIVERGENT);
    }

    // Remove argument metadata.
    for (Function::arg_iterator A=f->arg_begin(), AE=f->arg_end(); A!=AE; ++A)
    {
        Argument* arg = &*A;
        removeMetadata(arg, RV_METADATA_RES_UNIFORM);
        removeMetadata(arg, RV_METADATA_RES_VECTOR);
        removeMetadata(arg, RV_METADATA_RES_SCALARS);
        removeMetadata(arg, RV_METADATA_ALIGNED_TRUE);
        removeMetadata(arg, RV_METADATA_ALIGNED_FALSE);
        removeMetadata(arg, RV_METADATA_INDEX_SAME);
        removeMetadata(arg, RV_METADATA_INDEX_CONSECUTIVE);
        removeMetadata(arg, RV_METADATA_INDEX_SHUFFLE);
        removeMetadata(arg, RV_METADATA_INDEX_STRIDED);
        removeMetadata(arg, RV_METADATA_INDEX_RANDOM);
    }
}

} /*  namespace WFV */
