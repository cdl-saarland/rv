/**
 * @file   utils.h
 * @date   29.12.2013
 * @author Ralf Karrenberg
 *
 * This file is distributed under the University of Illinois Open Source
 * License. See the COPYING file in the root directory for details.
 *
 * Copyright (C) 2013 Saarland University
 *
 */

#ifndef UTILS_H
#define	UTILS_H

#include <llvm/IR/Module.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>          // ReturnInst
#include <llvm/Transforms/Utils/Cloning.h> // CloneFunctionInto

using namespace llvm;

namespace {

// Due to imprecisions etc. we apply a few rules
// when to treat results as equal.
// - float == float -> equality of first 5 decimal places
// - NaN == NaN -> true ;)
bool
resultMatches(const float a, const float b)
{
	return a == b ||
		(std::isnan(a) && std::isnan(b)) ||
		(fabs(a-b) < 0.000001);
}

struct Result {
    char* info;
    float* scalarRes;
    float* vectorRes;
};

// This is a trick to instantiate a template-dependent type.
template<int W>
struct VecTypeStruct {
    typedef __attribute__ ((vector_size (W*8))) float type;
};

////////////////////////////////////////////////////////////////////////////////

FunctionType*
createWrapperType(FunctionType* type, LLVMContext& context)
{
    SmallVector<Type*, 8> argTypes;

    for (FunctionType::param_iterator A=type->param_begin(),
         AE=type->param_end(); A!=AE; ++A)
    {
        if (isa<VectorType>(*A))
        {
            Type* elemType = (*A)->getVectorElementType();
            Type* newType = PointerType::getUnqual(elemType);
            argTypes.push_back(newType);
        }
        else
        {
            argTypes.push_back(*A);
        }
    }

    Type* oldRetType = type->getReturnType();
    Type* newRetType = Type::getVoidTy(context);
    if (isa<VectorType>(oldRetType))
    {
        Type* elemType = oldRetType->getVectorElementType();
        Type* newType = PointerType::getUnqual(PointerType::getUnqual(elemType));
        argTypes.push_back(newType);
    }
    else
    {
        newRetType = oldRetType;
    }

    return FunctionType::get(newRetType, argTypes, false);
}

////////////////////////////////////////////////////////////////////////////////

// TODO: The wrapper should generate vector loads/stores instead of scalar ones
//       in case of simple float/int types. The supplied arrays then have to be
//       aligned, of course.
Function*
wrapIntoScalarSignature(Function* f_SIMD, Module* mod)
{
    LLVMContext& context = f_SIMD->getContext();

    // Create new type.
    FunctionType* oldType = f_SIMD->getFunctionType();
    FunctionType* newType = createWrapperType(oldType, context);

    // Create function declaration.
    Function* wrapF = Function::Create(newType,
                                       GlobalValue::ExternalLinkage,
                                       f_SIMD->getName()+".wrap",
                                       mod);

    wrapF->setCallingConv(CallingConv::C);
    //wrapF->setAttributes(f_SIMD->getAttributes()); // TODO: We have more arguments.
    wrapF->setAlignment(f_SIMD->getAlignment());
    wrapF->setLinkage(f_SIMD->getLinkage());

    // Create BasicBlock.
    BasicBlock* entryBB = BasicBlock::Create(context, "entry", wrapF);

    // Create pack operations that create vector arguments for f_SIMD from the
    // array arguments of wrapF.
    // TODO: This currently can not handle types with "nested" vectors.
    //       Use generic functionality implemented in libWFV for packing.
    SmallVector<Value*, 4> args;
    Function::arg_iterator WA=wrapF->arg_begin();
    for (Function::const_arg_iterator A=f_SIMD->arg_begin(),
         AE=f_SIMD->arg_end(); A!=AE; ++A, ++WA)
    {
        assert (WA != wrapF->arg_end());

        Type* argType = A->getType();
        if (!isa<VectorType>(argType))
        {
            assert (argType == WA->getType());
            args.push_back(WA);
            continue;
        }

        assert (WA->getType()->isPointerTy());
        assert (WA->getType()->getPointerElementType() == argType->getVectorElementType());

        const unsigned numVals = argType->getVectorNumElements();
        assert (numVals > 1);

        Value* packedVal = UndefValue::get(argType);
        for (unsigned i=0; i<numVals; ++i)
        {
            ConstantInt* idxVal = ConstantInt::get(Type::getInt32Ty(context), i, false);
            GetElementPtrInst* gep = GetElementPtrInst::Create(WA, idxVal, "", entryBB);
            Value* arrElem = new LoadInst(gep, "", entryBB);
            packedVal = InsertElementInst::Create(packedVal, arrElem, idxVal, "", entryBB);
        }

        args.push_back(packedVal);
    }

    // Create call to f_SIMD.
    CallInst* call = CallInst::Create(f_SIMD,
                                      ArrayRef<Value*>(args),
                                      "f_SIMD",
                                      entryBB);

    call->setTailCall();
    call->setDoesNotThrow();

    // Create unpack and store operations that write back results from f_SIMD
    // to the result array pointer of wrapF.
    // TODO: This currently can not handle types with "nested" vectors.
    //       Use generic functionality implemented in libWFV for unpacking.
    Type* retType = f_SIMD->getReturnType();

    if (!isa<VectorType>(retType))
    {
        assert (retType == wrapF->getReturnType());
        assert (WA == wrapF->arg_end());
        ReturnInst::Create(context, call, entryBB);
    }
    else
    {
        assert (WA != wrapF->arg_end());
        assert (wrapF->getReturnType()->isVoidTy());
        assert (WA->getType()->isPointerTy());
        assert (WA->getType()->getPointerElementType()->isPointerTy());
        const unsigned numVals = retType->getVectorNumElements();
        assert (numVals > 1);

        assert (WA->getType()->getPointerElementType()->getArrayElementType() ==
                retType->getVectorElementType());

        Value* retArr = new LoadInst(WA, "", entryBB);
        for (unsigned i=0; i<numVals; ++i)
        {
            ConstantInt* idxVal = ConstantInt::get(Type::getInt32Ty(context), i, false);
            Value* elem = ExtractElementInst::Create(call, idxVal, "", entryBB);
            GetElementPtrInst* loc = GetElementPtrInst::Create(retArr, idxVal, "", entryBB);
            new StoreInst(elem, loc, entryBB);
        }

        ReturnInst::Create(context, entryBB);
        ++WA;
    }

    assert (WA == wrapF->arg_end());

    return wrapF;
}

////////////////////////////////////////////////////////////////////////////////

} // unnamed namespace

#endif	/* UTILS_H */

