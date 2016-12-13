//===- rvInfo.cpp ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @authors karrenberg, simon
//


#include "rv/rvInfo.h"

#include <llvm/IR/Module.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/Support/raw_ostream.h>

// #include <llvm/Support/system_error.h> // Required for MemoryBuffer::getFile()
#include <llvm/Linker/Linker.h>               // Linker
#include <llvm/IR/Intrinsics.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <stdexcept>

#include "utils/rvTools.h"

#include "rvConfig.h"

using namespace llvm;

namespace rv {

RVInfo::RVInfo(Module*         M,
				 LLVMContext*    C,
				 Function* scalarFunction,
				 Function*       simdFunction,
				 const unsigned  vectorizationFactor,
				 const int       maskPosition,
				 const bool      disableMemAccessAnalysis,
				 const bool      disableControlFlowDivAnalysis,
				 const bool      disableAllAnalyses,
				 const bool      verbose,
                 TimerGroup*     timerGroup)
        : mModule(M),
        mContext(C),
        mDataLayout(new DataLayout(M)),
        mScalarFunction(scalarFunction),
        mSimdFunction(simdFunction),
        mVectorizationFactor(vectorizationFactor),
        mMaskPosition(maskPosition),
        mDisableMemAccessAnalysis(disableMemAccessAnalysis),
        mDisableControlFlowDivAnalysis(disableControlFlowDivAnalysis),
        mDisableAllAnalyses(disableAllAnalyses),
        mVerbose(verbose),
        mTimerGroup(timerGroup),
        mIsAnalyzed(false),
        mRVLibLinked(false),
	mConfigured(false)
{
    // RVInterface should set disableMemAccessAnalysis and disableControlFlowDivAnalysis
    // to 'false' if disableAllAnalyses is set.
    assert ((!mDisableAllAnalyses ||
             (mDisableMemAccessAnalysis && mDisableControlFlowDivAnalysis)) &&
            "expecting all analyses to internally be disabled if 'disableAllAnalyses' is set!");
    initialize();
#ifdef _DEBUG
    rvVerbose = mVerbose;
#endif
}

RVInfo::RVInfo(const RVInfo& other)
        : funcMappings(other.funcMappings),
        mModule(other.mModule),
        mContext(other.mContext),
        mDataLayout(other.mDataLayout),
        mScalarFunction(other.mScalarFunction),
        mSimdFunction(other.mSimdFunction),
        mValueInfoMap(other.mValueInfoMap),
        mFunctionInfoMap(other.mFunctionInfoMap),
        mVectorizationFactor(other.mVectorizationFactor),
        mMaskPosition(other.mMaskPosition),
        mDisableMemAccessAnalysis(other.mDisableMemAccessAnalysis),
        mDisableControlFlowDivAnalysis(other.mDisableControlFlowDivAnalysis),
        mDisableAllAnalyses(other.mDisableAllAnalyses),
        mVerbose(other.mVerbose),
        mTimerGroup(other.mTimerGroup),
        mIsAnalyzed(other.mIsAnalyzed),
        mRVLibLinked(other.mRVLibLinked),
		mConfigured(other.mConfigured)
{
    initialize();
}

bool
RVInfo::configure() {
	if (mConfigured) {
		errs() << "RVInfo: already configured!\n";
		return false;
	}

	// FIXME configure through interface

	mConfigured = true;
	return true;
}

RVInfo::~RVInfo()
{
  for (auto it : funcMappings) {
    delete it.second;
  }
  delete mDataLayout;
}

void
RVInfo::initialize()
{
    if (mVectorizationFactor < 1)
    {
        errs() << "ERROR: Vectorization factor must be larger than zero!\n";
        throw std::logic_error("vectorization factor must be larger than zero");
    }

    if (mMaskPosition != -1)
    {
        if (mMaskPosition < -1)
        {
            errs() << "ERROR: Mask position must be >= -1!\n";
            throw std::logic_error("mask position must be >= -1");
        }

        if (mMaskPosition > (int)mSimdFunction->getFunctionType()->getNumParams())
        {
            errs() << "ERROR: Mask position is larger than the number of parameters!\n";
            throw std::logic_error("mask position is larger than the number of parameters");
        }

        Function::const_arg_iterator A = mScalarFunction->arg_begin();
        std::advance(A, mMaskPosition);
        if (!A->getType()->isIntegerTy(1))
        {
            errs() << "ERROR: Mask argument at position " << mMaskPosition
                << " is of non-boolean type: expected i1, found " << *A->getType() << "!\n";
            throw std::logic_error("mask argument has bad type");
        }
    }

    // This is not the "real" simd register width, just what we need
    // for the sext/bc construct to make codegen create a ptest.
    const unsigned simdRegWidth = mVectorizationFactor * 32;

    // Create packet-datatypes.
    mVectorTyFloatSIMD  = VectorType::get(Type::getFloatTy(*mContext), mVectorizationFactor);
    mVectorTyIntSIMD    = VectorType::get(Type::getInt32Ty(*mContext), mVectorizationFactor);
    mVectorTyDoubleSIMD = VectorType::get(Type::getDoubleTy(*mContext), mVectorizationFactor);
    mVectorTyLongSIMD   = VectorType::get(Type::getInt64Ty(*mContext), mVectorizationFactor);
    mVectorTyBoolSIMD   = VectorType::get(Type::getInt1Ty(*mContext), mVectorizationFactor);
    mScalarTyIntSIMD    = Type::getIntNTy(*mContext, simdRegWidth);
    mScalarBoolTy       = Type::getInt1Ty(*mContext);

    // Generate constants.
    mConstVecSIMDInt32MinusOne = createPacketConstantInt(-1);
    mConstVecSIMDF32One        = createPacketConstantFloat(1.000000e+00f);
    mConstIntSIMDRegWidthZero  = ConstantInt::get(*mContext, APInt(simdRegWidth,  "0", 10));
    mConstInt32Zero            = ConstantInt::get(*mContext, APInt(32,  "0", 10));
    mConstInt32One             = ConstantInt::get(*mContext, APInt(32,  "1", 10));
    mConstInt32Two             = ConstantInt::get(*mContext, APInt(32,  "2", 10));
    mConstInt32Three           = ConstantInt::get(*mContext, APInt(32,  "3", 10));
    mConstBoolTrue             = Constant::getAllOnesValue(Type::getInt1Ty(*mContext));
    mConstBoolFalse            = Constant::getNullValue(Type::getInt1Ty(*mContext));

    // Initialize "dynamic" stuff.

    mAlignmentScalar  = mDataLayout->getABITypeAlignment(Type::getFloatTy(*mContext));
    mAlignmentPtr     = mDataLayout->getABITypeAlignment(
        PointerType::getUnqual(Type::getFloatTy(*mContext)));
    mAlignmentSIMDPtr = mDataLayout->getABITypeAlignment(
        PointerType::getUnqual(mVectorTyFloatSIMD));
    mAlignmentSIMD   = mDataLayout->getABITypeAlignment(mVectorTyFloatSIMD);

    mConstAlignmentSIMD = ConstantInt::get(*mContext, APInt(32, mAlignmentSIMD));

	if (mModule != mScalarFunction->getParent())
	{
		errs() << "ERROR: source function has parent that differs from base module!\n";
        throw std::logic_error("source function has parent that differs from base module");
	}
	if (mModule != mSimdFunction->getParent())
	{
		errs() << "ERROR: target function has parent that differs from base module!\n";
        throw std::logic_error("target function has parent that differs from base module");
	}
    if (mContext != &mModule->getContext())
	{
        errs() << "WARNING: context differs from module's context!\n";
    }
}


rv::VectorMapping*
RVInfo::inferSelfMapping(
		Function& func,
		const bool      isOpUniform,
		const bool      isOpVarying,
		const bool      isOpSequential,
		const bool      isOpSequentialGuarded,
		const bool      isResultUniform,
		const bool      isResultVector,
		const bool      isResultScalars,
		const bool      isAligned,
		const bool      isIndexSame,
		const bool      isIndexConsecutive)
{
	using namespace rv;

    const int aligned = isAligned ? mVectorizationFactor : 1;

// result shape
	rv::VectorShape resultShape;
	if (isIndexConsecutive) {
		resultShape = VectorShape::cont(aligned);
	} else if (isIndexSame) {
		resultShape = VectorShape::uni(aligned);
	} else {
		resultShape = VectorShape::varying(aligned);
	}

	auto * retTy = func.getReturnType();

// argument shapes (default to uniform)
	rv::VectorShapeVec argShapes;

	for (auto & arg : func.getArgumentList()) {
		argShapes.push_back(VectorShape::uni()); // unaligned
	}

	return new rv::VectorMapping(
			&func,
			&func,
			mVectorizationFactor,
			-1, // no mask
			resultShape,
			argShapes
		);
}

rv::VectorMapping*
RVInfo::inferMapping(llvm::Function & scalarFnc, llvm::Function & simdFnc, int maskPos) {
	using namespace rv;

// return shape
	rv::VectorShape resultShape;

	auto * scalarRetTy = scalarFnc.getReturnType();
	auto * simdRetTy = simdFnc.getReturnType();

	if (rv::typesMatch(scalarRetTy, simdRetTy)) {
		resultShape = VectorShape::uni();
	} else {
		assert(simdRetTy->isVectorTy() && "return type mismatch");
		resultShape = VectorShape::varying();
	}

// argument shapes
	rv::VectorShapeVec argShapes;

	auto & scalarArgList = scalarFnc.getArgumentList();
	auto itScalarArg = scalarArgList.begin();

	auto & simdArgList = simdFnc.getArgumentList();
	auto itSimdArg = simdArgList.begin();

	for (int i = 0; i < simdArgList.size(); ++i) {
	// mask special case
		if (i == maskPos) {
			argShapes.push_back(VectorShape::varying());
			++itSimdArg;
			continue;
		}

	// trailing additional argument case
		if (itScalarArg == scalarArgList.end()) {
			IF_DEBUG errs() << "Unexpected additional argument (pos " << i << ") in simd function " << simdFnc << "\n";
			argShapes.push_back(VectorShape::varying());
			++itSimdArg;
			continue;
		}

	// default argument case
		if (rv::typesMatch(itScalarArg->getType(), itSimdArg->getType())) {
			argShapes.push_back(VectorShape::uni()); // unaligned
		} else {
			argShapes.push_back(VectorShape::varying());
		}

		++itScalarArg;
		++itSimdArg;
	}

	assert(itScalarArg == scalarArgList.end());
	assert(itSimdArg == simdArgList.end());

	return new rv::VectorMapping(
				&scalarFnc,
				&simdFnc,
				mVectorizationFactor,
				maskPos,
				resultShape,
				argShapes
			);
}

bool
RVInfo::addSIMDMapping(rv::VectorMapping & mapping) {
  funcMappings[mapping.scalarFn] = new rv::VectorMapping(mapping);
}

// This function should be called *before* run().
bool
RVInfo::addSIMDMapping(const Function& scalarFunction,
                        const Function& simdFunction,
                        const int       maskPosition,
                        const bool      mayHaveSideEffects)
{
    if (scalarFunction.getParent() != mModule)
	{
        errs() << "ERROR: scalar function has parent that differs from base module!\n";
        return false;
    }
    if (simdFunction.getParent() != mModule)
	{
        errs() << "ERROR: SIMD function has parent that differs from base module!\n";
        return false;
    }

    // Find out which arguments are UNIFORM and which are VARYING.
    SmallVector<bool, 4> uniformArgs;
    uniformArgs.reserve(scalarFunction.getArgumentList().size());

    Function::const_arg_iterator scalarA = scalarFunction.arg_begin();
    Function::const_arg_iterator simdA   = simdFunction.arg_begin();

    for (Function::const_arg_iterator scalarE = scalarFunction.arg_end();
            scalarA != scalarE; ++scalarA, ++simdA)
    {
        Type*      scalarType = scalarA->getType();
        Type*      simdType   = simdA->getType();
        const bool isUniform  = rv::typesMatch(scalarType, simdType);

        uniformArgs.push_back(isUniform);
    }

    // Store info.
    const bool success = mFunctionInfoMap.add(scalarFunction,
                                              simdFunction,
                                              maskPosition,
                                              mayHaveSideEffects,
                                              uniformArgs);

    if (!success)
	{
        errs() << "ERROR: Insertion of function mapping failed for function: "
                << scalarFunction.getName() << "\n";
        return false;
    }

    funcMappings[&scalarFunction] = inferMapping(const_cast<Function&>(scalarFunction), const_cast<Function&>(simdFunction), maskPosition);

	return true;
}
namespace {

bool
checkSanity(const Function& f,
            const bool      isOpUniform,
            const bool      isOpVarying,
            const bool      isOpSequential,
            const bool      isOpSequentialGuarded,
            const bool      isResultUniform,
            const bool      isResultVector,
            const bool      isResultScalars,
            const bool      isAligned,
            const bool      isIndexSame,
            const bool      isIndexConsecutive)
{
    bool isSane = true;

    if (!isOpSequential && !isOpSequentialGuarded && isResultScalars)
    {
        errs() << "ERROR while adding SIMD semantics for function '"
            << f.getName() << "': Only OP_SEQUENTIAL functions can be RESULT_SCALARS!\n";
        isSane = false;
    }

    if (isOpUniform)
    {
        if (!isResultUniform)
        {
            errs() << "ERROR while adding SIMD semantics for function '"
                << f.getName() << "': OP_UNIFORM function has to be RESULT_SAME!\n";
            isSane = false;
        }
        if (!isIndexSame)
        {
            errs() << "ERROR while adding SIMD semantics for function '"
                << f.getName() << "': OP_UNIFORM function has to be INDEX_SAME!\n";
            isSane = false;
        }
    }
    else
    {
        if (isResultUniform)
        {
            errs() << "ERROR while adding SIMD semantics for function '"
                << f.getName() << "': Only OP_UNIFORM function can be RESULT_SAME!\n";
            isSane = false;
        }
        if (isIndexSame)
        {
            errs() << "ERROR while adding SIMD semantics for function '"
                << f.getName() << "': Only OP_UNIFORM function can be INDEX_SAME!\n";
            isSane = false;
        }
    }

    return isSane;
}

bool
checkSanity(const Argument& arg,
            const bool      isResultUniform,
            const bool      isResultVector,
            const bool      isResultScalars,
            const bool      isAligned,
            const bool      isIndexSame,
            const bool      isIndexConsecutive)
{
    bool isSane = true;

    if ((isResultUniform + isResultVector + isResultScalars) != 1)
    {
        errs() << "ERROR while adding SIMD semantics for argument '"
            << arg.getName() << "': argument has to have exactly one RESULT_ property!\n";
        isSane = false;
    }

    if ((isIndexSame + isIndexConsecutive) != 1)
    {
        errs() << "ERROR while adding SIMD semantics for argument '"
            << arg.getName() << "': argument has to have exactly one INDEX_ property!\n";
        isSane = false;
    }

    if (isIndexSame != isResultUniform)
    {
        errs() << "ERROR while adding SIMD semantics for argument '"
            << arg.getName() << "': INDEX_SAME property has to match RESULT_UNIFORM!\n";
        isSane = false;
    }

    return isSane;
}

bool
checkSanity(const Instruction& inst,
            const bool         isOpUniform,
            const bool         isOpVarying,
            const bool         isOpSequential,
            const bool         isOpSequentialGuarded,
            const bool         isResultUniform,
            const bool         isResultVector,
            const bool         isResultScalars,
            const bool         isAligned,
            const bool         isIndexSame,
            const bool         isIndexConsecutive)
{
    bool isSane = true;

    if (!isOpSequential && !isOpSequentialGuarded && isResultScalars)
    {
        errs() << "ERROR while adding SIMD semantics for instruction '"
            << inst << "': Only OP_SEQUENTIAL instructions can be RESULT_SCALARS!\n";
        isSane = false;
    }

    if (isOpUniform)
    {
        if (!isResultUniform)
        {
            errs() << "ERROR while adding SIMD semantics for instruction '"
                << inst << "': OP_UNIFORM instruction has to be RESULT_SAME!\n";
            isSane = false;
        }
        if (!isIndexSame)
        {
            errs() << "ERROR while adding SIMD semantics for instruction '"
                << inst << "': OP_UNIFORM instruction has to be INDEX_SAME!\n";
            isSane = false;
        }
    }
    else
    {
        if (isResultUniform)
        {
            errs() << "ERROR while adding SIMD semantics for instruction '"
                << inst << "': Only OP_UNIFORM instruction can be RESULT_SAME!\n";
            isSane = false;
        }
        if (isIndexSame)
        {
            errs() << "ERROR while adding SIMD semantics for instruction '"
                << inst << "': Only OP_UNIFORM instruction can be INDEX_SAME!\n";
            isSane = false;
        }
    }

    return isSane;
}

}

// This function should be called *before* run()
bool
RVInfo::addSIMDSemantics(const Function& f,
                          const bool      isOpUniform,
                          const bool      isOpVarying,
                          const bool      isOpSequential,
                          const bool      isOpSequentialGuarded,
                          const bool      isResultUniform,
                          const bool      isResultVector,
                          const bool      isResultScalars,
                          const bool      isAligned,
                          const bool      isIndexSame,
                          const bool      isIndexConsecutive)
{
    bool allSuccessful = true;

    // Check sanity of properties.
    if (!checkSanity(f, isOpUniform, isOpVarying, isOpSequential, isOpSequentialGuarded,
                     isResultUniform, isResultVector, isResultScalars,
                     isAligned, isIndexSame, isIndexConsecutive))
    {
        return false;
    }

    funcMappings[&f] = inferSelfMapping(const_cast<Function&>(f), isOpUniform, isOpVarying, isOpSequential, isOpSequentialGuarded, isResultUniform, isResultVector, isResultScalars, isAligned, isIndexSame, isIndexConsecutive);

    return allSuccessful;
}

// TODO: The following functions do not exist as scalar intrinsics,
//       so we have to create them manually:
//       * fmodf  -> SIMD version available in IR directly
//       * addsub -> SIMD version available in AVX / SSE3
//       * sincos -> SIMD version available in RV lib
void
RVInfo::addCommonMappingsSSE(const bool useSSE41, const bool useSSE42)
{
    // Store mappings of library functions.

    Type*         floatTy  = Type::getFloatTy(*mContext);
    //Type*         doubleTy = Type::getDoubleTy(*mContext);
    FunctionType* fnFloatTy1  = FunctionType::get(floatTy,
                                                  ArrayRef<Type*>(floatTy),
                                                  false);
    //FunctionType* fnDoubleTy1 = FunctionType::get(doubleTy,
                                                  //ArrayRef<Type*>(doubleTy),
                                                  //false);
    std::vector<Type*> types;
    types.push_back(floatTy);
    types.push_back(floatTy);
    FunctionType* fnFloatTy2 = FunctionType::get(floatTy,
                                                 ArrayRef<Type*>(types),
                                                 false);
    //types.clear();
    //types.push_back(doubleTy);
    //types.push_back(doubleTy);
    //FunctionType* fnDoubleTy2 = FunctionType::get(doubleTy,
                                                  //ArrayRef<Type*>(types),
                                                  //false);

#define RV_GET_OR_CREATE_FUNCTION(fnName, fnType) \
    Function* fnName ## _scalar = mModule->getFunction(#fnName) ? \
        mModule->getFunction(#fnName) : \
        Function::Create(fnType, \
                         GlobalValue::InternalLinkage, \
                         #fnName, \
                         mModule); \
    fnName ## _scalar->setLinkage(GlobalValue::ExternalLinkage); \
    assert (fnName ## _scalar)

#define RV_GET_INTRINSIC(avxName, sseName) \
    Function* sseName ## _simd = \
        Intrinsic::getDeclaration(mModule, Intrinsic::x86_sse_##sseName); \
    assert (sseName ## _simd);

#define RV_ADD_MATHFUN_MAPPING(fnName) \
    { \
        Function* scalarFn = Intrinsic::getDeclaration(mModule, \
                                                       Intrinsic::fnName, \
                                                       ArrayRef<Type*>(floatTy)); \
        Function* simdFn   = mModule->getFunction(#fnName "_ps"); \
        assert (scalarFn && simdFn); \
        addSIMDMapping(*scalarFn, *simdFn, -1, false); \
    } ((void)0)

#define RV_ADD_LLVM_INTERNAL_MAPPING(fnName) \
    { \
        Function* scalarFn = Intrinsic::getDeclaration(mModule, \
                                                       Intrinsic::fnName, \
                                                       ArrayRef<Type*>(floatTy)); \
        Function* simdFn   = Intrinsic::getDeclaration(mModule, \
                                                       Intrinsic::fnName, \
                                                       ArrayRef<Type*>(mVectorTyFloatSIMD)); \
        assert (scalarFn && simdFn); \
        addSIMDMapping(*scalarFn, *simdFn, -1, false); \
    } ((void)0)



    {
        Function* scalarFn = Intrinsic::getDeclaration(mModule,
                                                       Intrinsic::sqrt,
                                                       ArrayRef<Type*>(floatTy));
        assert (scalarFn);
        RV_GET_INTRINSIC(sqrt_ps_256, sqrt_ps);
        addSIMDMapping(*scalarFn, *sqrt_ps_simd, -1, false);
    }

    {
        RV_GET_OR_CREATE_FUNCTION(rsqrtf, fnFloatTy1);
        RV_GET_INTRINSIC(rsqrt_ps_256, rsqrt_ps);
        addSIMDMapping(*rsqrtf_scalar, *rsqrt_ps_simd, -1, false);
    }
    {
        RV_GET_OR_CREATE_FUNCTION(min, fnFloatTy2);
        RV_GET_INTRINSIC(min_ps_256, min_ps);
        addSIMDMapping(*min_scalar, *min_ps_simd, -1, false);
    }
    {
        RV_GET_OR_CREATE_FUNCTION(max, fnFloatTy2);
        RV_GET_INTRINSIC(max_ps_256, max_ps);
        addSIMDMapping(*max_scalar, *max_ps_simd, -1, false);
    }

    if (useSSE41 || useSSE42)
    {
        RV_GET_OR_CREATE_FUNCTION(roundf, fnFloatTy1);
        RV_GET_OR_CREATE_FUNCTION(ceilf,  fnFloatTy1);
        RV_GET_OR_CREATE_FUNCTION(floorf, fnFloatTy1);

        Function* simdFn = Intrinsic::getDeclaration(mModule, Intrinsic::x86_sse41_round_ps);
        assert (simdFn);
        addSIMDMapping(*roundf_scalar, *simdFn, -1, false);
        addSIMDMapping(*ceilf_scalar,  *simdFn, -1, false);
        addSIMDMapping(*floorf_scalar, *simdFn, -1, false);
    }

    // TODO: Add mappings for double.
    RV_ADD_MATHFUN_MAPPING(sin);
    RV_ADD_MATHFUN_MAPPING(cos);
    RV_ADD_MATHFUN_MAPPING(log);
    RV_ADD_MATHFUN_MAPPING(log2);
    RV_ADD_MATHFUN_MAPPING(exp);
    RV_ADD_MATHFUN_MAPPING(exp2);
    RV_ADD_MATHFUN_MAPPING(fabs);
#ifdef RV_USE_NATIVE_POW_PS
    RV_ADD_MATHFUN_MAPPING(pow);
#endif

    RV_ADD_LLVM_INTERNAL_MAPPING(sqrt);
    RV_ADD_LLVM_INTERNAL_MAPPING(powi);
    //RV_ADD_LLVM_INTERNAL_MAPPING(sin);
    //RV_ADD_LLVM_INTERNAL_MAPPING(cos);
    RV_ADD_LLVM_INTERNAL_MAPPING(pow);
    //RV_ADD_LLVM_INTERNAL_MAPPING(exp);
    //RV_ADD_LLVM_INTERNAL_MAPPING(exp2);
    RV_ADD_LLVM_INTERNAL_MAPPING(log);
    //RV_ADD_LLVM_INTERNAL_MAPPING(log2);
    RV_ADD_LLVM_INTERNAL_MAPPING(log10);
    RV_ADD_LLVM_INTERNAL_MAPPING(fma);
    // TODO: Enable these for SSE as well (and for double).
    RV_ADD_LLVM_INTERNAL_MAPPING(fabs);
    RV_ADD_LLVM_INTERNAL_MAPPING(floor);
    RV_ADD_LLVM_INTERNAL_MAPPING(ceil);
    RV_ADD_LLVM_INTERNAL_MAPPING(rint);
    RV_ADD_LLVM_INTERNAL_MAPPING(nearbyint);
    RV_ADD_LLVM_INTERNAL_MAPPING(fmuladd);


#undef RV_GET_OR_CREATE_FUNCTION
#undef RV_GET_INTRINSIC
#undef RV_ADD_MATHFUN_MAPPING
#undef RV_ADD_LLVM_INTERNAL_MAPPING
}

// TODO: The following functions do not exist as scalar intrinsics,
//       so we have to create them manually:
//       * fmodf  -> SIMD version available in IR directly
//       * addsub -> SIMD version available in AVX / SSE3
//       * sincos -> SIMD version available in RV lib
void
RVInfo::addCommonMappingsAVX()
{
    // Store mappings of library functions.

    Type*         floatTy  = Type::getFloatTy(*mContext);
    Type*         doubleTy = Type::getDoubleTy(*mContext);
    FunctionType* fnFloatTy1  = FunctionType::get(floatTy,
                                                  ArrayRef<Type*>(floatTy),
                                                  false);
    FunctionType* fnDoubleTy1 = FunctionType::get(doubleTy,
                                                  ArrayRef<Type*>(doubleTy),
                                                  false);
    std::vector<Type*> types;
    types.push_back(floatTy);
    types.push_back(floatTy);
    FunctionType* fnFloatTy2 = FunctionType::get(floatTy,
                                                 ArrayRef<Type*>(types),
                                                 false);
    types.clear();
    types.push_back(doubleTy);
    types.push_back(doubleTy);
    FunctionType* fnDoubleTy2 = FunctionType::get(doubleTy,
                                                  ArrayRef<Type*>(types),
                                                  false);

#define RV_GET_OR_CREATE_FUNCTION(fnName, fnType) \
    Function* fnName ## _scalar = mModule->getFunction(#fnName) ? \
        mModule->getFunction(#fnName) : \
        Function::Create(fnType, \
                         GlobalValue::InternalLinkage, \
                         #fnName, \
                         mModule); \
    fnName ## _scalar->setLinkage(GlobalValue::ExternalLinkage); \
    assert (fnName ## _scalar)

#define RV_GET_INTRINSIC(avxName, sseName) \
    Function* sseName ## _simd = \
        Intrinsic::getDeclaration(mModule, Intrinsic::x86_avx_##avxName); \
    assert (sseName ## _simd);

#define RV_ADD_MATHFUN_MAPPING(fnName) \
    { \
        Function* scalarFn = Intrinsic::getDeclaration(mModule, \
                                                       Intrinsic::fnName, \
                                                       ArrayRef<Type*>(floatTy)); \
        Function* simdFn   = mModule->getFunction(#fnName "256_ps"); \
        assert (simdFn); \
        if (scalarFn) addSIMDMapping(*scalarFn, *simdFn, -1, false); \
    } ((void)0)

#define RV_ADD_LLVM_INTERNAL_MAPPING(fnName) \
    { \
        Function* scalarFn = Intrinsic::getDeclaration(mModule, \
                                                       Intrinsic::fnName, \
                                                       ArrayRef<Type*>(floatTy)); \
        Function* simdFn   = Intrinsic::getDeclaration(mModule, \
                                                       Intrinsic::fnName, \
                                                       ArrayRef<Type*>(mVectorTyFloatSIMD)); \
        assert (scalarFn && simdFn); \
        if (scalarFn) addSIMDMapping(*scalarFn, *simdFn, -1, false); \
    } \
    { \
        Function* scalarFn = Intrinsic::getDeclaration(mModule, \
                                                       Intrinsic::fnName, \
                                                       ArrayRef<Type*>(doubleTy)); \
        Function* simdFn   = Intrinsic::getDeclaration(mModule, \
                                                       Intrinsic::fnName, \
                                                       ArrayRef<Type*>(mVectorTyDoubleSIMD)); \
        assert (scalarFn && simdFn); \
        addSIMDMapping(*scalarFn, *simdFn, -1, false); \
    } ((void)0)



    {
        Function* scalarFn = Intrinsic::getDeclaration(mModule,
                                                       Intrinsic::sqrt,
                                                       ArrayRef<Type*>(floatTy));
        assert (scalarFn);
        RV_GET_INTRINSIC(sqrt_ps_256, sqrt_ps);
        addSIMDMapping(*scalarFn, *sqrt_ps_simd, -1, false);
    }
    {
        Function* scalarFn = Intrinsic::getDeclaration(mModule,
                                                       Intrinsic::sqrt,
                                                       ArrayRef<Type*>(doubleTy));
        assert (scalarFn);
        RV_GET_INTRINSIC(sqrt_pd_256, sqrt_pd);
        addSIMDMapping(*scalarFn, *sqrt_pd_simd, -1, false);
    }

    {
        // rsqrt only exists for <8 x float>, not <4 x double>, apparently.
        RV_GET_OR_CREATE_FUNCTION(rsqrtf, fnFloatTy1);
        RV_GET_INTRINSIC(rsqrt_ps_256, rsqrt_ps);
        addSIMDMapping(*rsqrtf_scalar, *rsqrt_ps_simd, -1, false);
    }
    {
        RV_GET_OR_CREATE_FUNCTION(minf, fnFloatTy2);
        RV_GET_INTRINSIC(min_ps_256, min_ps);
        addSIMDMapping(*minf_scalar, *min_ps_simd, -1, false);
    }
    {
        RV_GET_OR_CREATE_FUNCTION(min, fnDoubleTy2);
        RV_GET_INTRINSIC(min_pd_256, min_pd);
        addSIMDMapping(*min_scalar, *min_pd_simd, -1, false);
    }
    {
        RV_GET_OR_CREATE_FUNCTION(maxf, fnFloatTy2);
        RV_GET_INTRINSIC(max_ps_256, max_ps);
        addSIMDMapping(*maxf_scalar, *max_ps_simd, -1, false);
    }
    {
        RV_GET_OR_CREATE_FUNCTION(max, fnDoubleTy2);
        RV_GET_INTRINSIC(max_pd_256, max_pd);
        addSIMDMapping(*max_scalar, *max_pd_simd, -1, false);
    }

    {
        RV_GET_OR_CREATE_FUNCTION(roundf, fnFloatTy1);
        RV_GET_OR_CREATE_FUNCTION(ceilf,  fnFloatTy1);
        RV_GET_OR_CREATE_FUNCTION(floorf, fnFloatTy1);

        Function* simdFn = Intrinsic::getDeclaration(mModule, Intrinsic::x86_avx_round_ps_256);
        assert (simdFn);
        addSIMDMapping(*roundf_scalar, *simdFn, -1, false);
        addSIMDMapping(*ceilf_scalar,  *simdFn, -1, false);
        addSIMDMapping(*floorf_scalar, *simdFn, -1, false);
    }
    {
        RV_GET_OR_CREATE_FUNCTION(round, fnDoubleTy1);
        RV_GET_OR_CREATE_FUNCTION(ceil,  fnDoubleTy1);
        RV_GET_OR_CREATE_FUNCTION(floor, fnDoubleTy1);

        Function* simdFn = Intrinsic::getDeclaration(mModule, Intrinsic::x86_avx_round_pd_256);
        assert (simdFn);
        addSIMDMapping(*round_scalar, *simdFn, -1, false);
        addSIMDMapping(*ceil_scalar,  *simdFn, -1, false);
        addSIMDMapping(*floor_scalar, *simdFn, -1, false);
    }


    // TODO: Add mappings for double.
    RV_ADD_MATHFUN_MAPPING(sin);
    RV_ADD_MATHFUN_MAPPING(cos);
    RV_ADD_MATHFUN_MAPPING(log);
    RV_ADD_MATHFUN_MAPPING(log2);
    RV_ADD_MATHFUN_MAPPING(exp);
    RV_ADD_MATHFUN_MAPPING(exp2);
    RV_ADD_MATHFUN_MAPPING(fabs);
#ifdef RV_USE_NATIVE_POW_PS
    RV_ADD_MATHFUN_MAPPING(pow);
#endif

    //RV_ADD_LLVM_INTERNAL_MAPPING(sqrt);
    RV_ADD_LLVM_INTERNAL_MAPPING(powi);
    //RV_ADD_LLVM_INTERNAL_MAPPING(sin);
    //RV_ADD_LLVM_INTERNAL_MAPPING(cos);
    RV_ADD_LLVM_INTERNAL_MAPPING(pow);
    //RV_ADD_LLVM_INTERNAL_MAPPING(exp);
    //RV_ADD_LLVM_INTERNAL_MAPPING(exp2);
    //RV_ADD_LLVM_INTERNAL_MAPPING(log);
    //RV_ADD_LLVM_INTERNAL_MAPPING(log2);
    RV_ADD_LLVM_INTERNAL_MAPPING(log10);
    RV_ADD_LLVM_INTERNAL_MAPPING(fma);
    //RV_ADD_LLVM_INTERNAL_MAPPING(fabs);
    RV_ADD_LLVM_INTERNAL_MAPPING(trunc);
    RV_ADD_LLVM_INTERNAL_MAPPING(floor);
    RV_ADD_LLVM_INTERNAL_MAPPING(ceil);
    RV_ADD_LLVM_INTERNAL_MAPPING(rint);
    RV_ADD_LLVM_INTERNAL_MAPPING(nearbyint);
    RV_ADD_LLVM_INTERNAL_MAPPING(fmuladd);

#undef RV_GET_OR_CREATE_FUNCTION
#undef RV_GET_INTRINSIC
#undef RV_ADD_MATHFUN_MAPPING
#undef RV_ADD_LLVM_INTERNAL_MAPPING
}

void
RVInfo::addCommonMappingsNEON()
{
    assert (false && "not implemented!");
}

// NOTE: we must not pre-generate this due to possibly different address spaces
const PointerType*
RVInfo::getPointerVectorType(const PointerType* oldType) const
{
    return PointerType::get(VectorType::get(oldType->getElementType(), mVectorizationFactor),
                            oldType->getAddressSpace());
}

Constant*
RVInfo::createPacketConstantInt(const int c) const
{
    std::vector<Constant*> cVec; //packet of 'vectorizationFactor' int32
    ConstantInt* const_int32 = ConstantInt::get(*mContext, APInt(32, c));
    for (unsigned i=0; i<mVectorizationFactor; ++i)
    {
        cVec.push_back(const_int32);
    }
    return ConstantVector::get(ArrayRef<Constant*>(cVec));
}

Constant*
RVInfo::createPacketConstantFloat(const float c) const
{
    std::vector<Constant*> fVec; //packet of 'vectorizationFactor' f32
    ConstantFP* const_f32 = ConstantFP::get(*mContext, APFloat(c));
    for (unsigned i=0; i<mVectorizationFactor; ++i)
    {
        fVec.push_back(const_f32);
    }
    return ConstantVector::get(ArrayRef<Constant*>(fVec));
}


rv::VectorMapping
RVInfo::inferTargetMapping(Function * actualScalarFn) {
	using namespace rv;

	VectorShape resultShape;
	VectorShapeVec argShapes;

	auto itVectorArg = mSimdFunction->getArgumentList().begin();
	for (const Argument & arg : mScalarFunction->getArgumentList()) {
		VectorShape shape;
		if (mValueInfoMap.hasMapping(arg)) {
			const auto & valueInfo = mValueInfoMap.get(arg);
            const unsigned aligned = valueInfo.mIsAligned ? mVectorizationFactor : 1U;
			if (valueInfo.mIsIndexSame) {
				shape = VectorShape::uni(aligned);
			} else if (valueInfo.mIsIndexConsecutive) {
				shape = VectorShape::cont(aligned);
			} else {
				shape = VectorShape::varying(aligned);
			}

		} else { // infer this mapping manually
			Type* vectorArgTy = itVectorArg->getType();
		    Type* scalarType = arg.getType();
			const bool isUniform  = rv::typesMatch(scalarType, vectorArgTy);

			if (isUniform) {
				shape = VectorShape::uni();
			} else {
				shape = VectorShape::varying();
			}
		}

		argShapes.push_back(shape);
		++itVectorArg;
	}

	return VectorMapping(actualScalarFn, mSimdFunction, mVectorizationFactor, mMaskPosition, resultShape, argShapes);
}


}
