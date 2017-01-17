//===- rvInfo.h ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//

#ifndef _RVINFO_H
#define	_RVINFO_H

#include "utils/functionInfoMap.h"
#include "utils/valueInfoMap.h"

#include <llvm/Pass.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Transforms/Utils/ValueMapper.h> // ValueToValueMapTy
#include <llvm/IR/DataLayout.h>            // DataLayout
#include <llvm/IR/DerivedTypes.h>                 // VectorType
#include <llvm/IR/Constants.h>                    // ConstantInt
#include <llvm/Support/Timer.h>                // TimerGroup
#include <memory>


#include "rv/vectorMapping.h"

// forward declaration of initializer
namespace llvm {
	void initializeRVInfoPass(PassRegistry&);
}

namespace native {
	typedef std::map<const llvm::Function*, const rv::VectorMapping*> VectorMappingMap;
}

using namespace llvm;

namespace rv {

class RVInfo {
	native::VectorMappingMap funcMappings;

// adapter functions for new rv-namespace interface
	rv::VectorMapping*
	inferMapping(llvm::Function & scalarFnc, llvm::Function & simdFnc, int maskPos);

	rv::VectorMapping*
	inferSelfMapping(Function& f,
            const bool      isOpUniform,
            const bool      isOpVarying,
            const bool      isOpSequential,
            const bool      isOpSequentialGuarded,
            const bool      isResultUniform,
            const bool      isResultVector,
            const bool      isResultScalars,
            const bool      isAligned,
            const bool      isIndexSame,
            const bool      isIndexConsecutive);

    bool addSIMDSemantics(const Function& f,
                          const bool      isOpUniform,
                          const bool      isOpVarying,
                          const bool      isOpSequential,
                          const bool      isOpSequentialGuarded,
                          const bool      isResultUniform,
                          const bool      isResultVector,
                          const bool      isResultScalars,
                          const bool      isAligned,
                          const bool      isIndexSame,
                          const bool      isIndexConsecutive);
public:
    native::VectorMappingMap & getVectorFuncMap() { return funcMappings; }

    friend class VectorizerInterface;

    explicit RVInfo(const RVInfo& other);
    explicit RVInfo(Module*         M,
                     LLVMContext*    C,
                     Function* scalarFunction,
                     Function*       simdFunction,
                     const unsigned  vectorizationFactor,
                     const int       maskPosition=-1,
                     const bool      disableMemAccessAnalysis=false,
                     const bool      disableControlFlowDivAnalysis=false,
                     const bool      disableAllAnalyses=false,
                     const bool      verbose=false,
                     TimerGroup*     timerGroup=nullptr);

    ~RVInfo();

    // legacy code adapter
    rv::VectorMapping inferTargetMapping(Function * actualScalarFn);

#if 0
    // add a new SIMD function mapping
    bool addSIMDMapping(rv::VectorMapping & mapping);

    bool addSIMDMapping(const Function& scalarFunction,
                        const Function& simdFunction,
                        const int       maskPosition,
                        const bool      mayHaveSideEffects);
#endif

    Module*              mModule;
    LLVMContext*         mContext;
    DataLayout*          mDataLayout;
    Function*      mScalarFunction;
    Function*            mSimdFunction;

    // Information about user-defined functions/values.
    rv::ValueInfoMap    mValueInfoMap;
    rv::FunctionInfoMap mFunctionInfoMap;

    // Target information.
    const unsigned       mVectorizationFactor;

    // Position of mask argument (if any, -1 otherwise).
    const int            mMaskPosition;

    // Misc information.
    const bool           mDisableMemAccessAnalysis;
    const bool           mDisableControlFlowDivAnalysis;
    const bool           mDisableAllAnalyses;
    const bool           mVerbose;
    TimerGroup*          mTimerGroup;

    // RV state information.
    bool                 mIsAnalyzed;

    // Vectorized datatypes.
    VectorType*          mVectorTyFloatSIMD;
    VectorType*          mVectorTyIntSIMD;
    VectorType*          mVectorTyDoubleSIMD;
    VectorType*          mVectorTyLongSIMD;
    VectorType*          mVectorTyBoolSIMD;
    Type*                mScalarTyIntSIMD;
    Type*                mScalarBoolTy;

    // LLVM Constants.
    Constant*            mConstVecSIMDInt32MinusOne;
    Constant*            mConstVecSIMDF32One;
    ConstantInt*         mConstIntSIMDRegWidthZero;
    ConstantInt*         mConstInt32Zero;
    ConstantInt*         mConstInt32One;
    ConstantInt*         mConstInt32Two;
    ConstantInt*         mConstInt32Three;
    Constant*            mConstBoolTrue;
    Constant*            mConstBoolFalse;
    Constant*            mConstAlignmentSIMD;

    // Alignment information.
    unsigned             mAlignmentScalar;
    unsigned             mAlignmentPtr;
    unsigned             mAlignmentSIMDPtr;
    unsigned             mAlignmentSIMD;


    // NOTE: we must not pre-generate this due to possibly different address spaces
    const PointerType* getPointerVectorType(const PointerType* oldType) const;
protected:
    // set a default SIMDModel
    // prepare for usage
    // FIXME add a finalize() pair for destruction (dtor usage not recommended)
    bool configure();

private:
    inline Constant* createPacketConstantInt(const int c) const;
    inline Constant* createPacketConstantFloat(const float c) const;

    bool mConfigured;
    bool mRVLibLinked;
    void addCommonMappingsSSE(const bool useSSE41, const bool useSSE42);
    void addCommonMappingsAVX();
    void addCommonMappingsNEON();

    void initialize();
};

}

#endif	/* _RVINFO_H */
