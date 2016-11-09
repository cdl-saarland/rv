//===- vectorizationAnalysis.h ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//



#ifndef _VECTORIZATIONANALYSIS_H
#define	_VECTORIZATIONANALYSIS_H

#include <llvm/Pass.h>

#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/DenseSet.h>
#include <llvm/ADT/SmallPtrSet.h>
#include <llvm/Analysis/PostDominators.h>

#include "rv/rvInfo.h"
#include "rv/vectorizationInfo.h"

#include "analysis/analysisCfg.h"
#include "utils/rvTools.h"

namespace RV {
class ValueInfoMap;
class FunctionInfoMap;
}

namespace llvm {
class Value;
class Constant;
class PHINode;
class LoopInfo;
class Instruction;
class BranchInst;
class Loop;
struct PostDominatorTree;
struct DominatorTree;
class LoadInst;
class StoreInst;
class GetElementPtrInst;
class SelectInst;
class CallInst;
}

using namespace llvm;
using rv::VectorizationInfo;

//namespace {

class VectorizationAnalysisWrapper : public FunctionPass {
public:
	static char ID __attribute__((unused)); // Pass identification, replacement for typeid.

	VectorizationAnalysisWrapper();

	virtual void releaseMemory   ();
	virtual void getAnalysisUsage(AnalysisUsage &AU) const;
	virtual bool doInitialization(Module& M);
	virtual bool doFinalization  (Module& M);
	virtual bool runOnFunction   (Function& F);
	virtual void print           (raw_ostream& O, const Module* M) const;
};

class VectorizationAnalysis
{
public:
    VectorizationAnalysis(Function*                   scalarFn,
                          const Function*             simdFn,
                          const unsigned              vectorizationFactor,
                          const int                   maskPosition,
                          const rv::ValueInfoMap*    valueInfoMap,
                          const rv::FunctionInfoMap* functionInfoMap,
                          const bool                  disableMemAccessAnalysis,
                          const bool                  disableControlFlowDivAnalysis,
                          const bool                  disableAllAnalyses,
                          const bool                  verbose,
						  rv::RVInfo*                    info,
						  VectorizationInfo&          vecInfo,
						  const LoopInfo&             loopInfo,
						  const PostDominatorTree&    postDomTree,
						  const DominatorTree&        domTree);

	~VectorizationAnalysis();

	void run(Function& F);

private:
        rv::RVInfo *                   mInfo;
	VectorizationInfo&          mVecInfo;
    const LoopInfo&             mLoopInfo;
    const PostDominatorTree&    mPostDomTree;
    const DominatorTree&        mDomTree;

    Function*                   mScalarFunction;
    const Function*             mSimdFunction;

    const int                   mMaskPosition;

    const rv::ValueInfoMap*    mValueInfoMap;
    const rv::FunctionInfoMap* mFunctionInfoMap;

    const bool                  mDisableMemAccessAnalysis;
    const bool                  mDisableControlFlowDivAnalysis;
    const bool                  mDisableAllAnalyses;
    const bool                  mVerbose;

    std::set<BasicBlock*> uniformBlocks;

    // find all blocks that are guarded by a uniform mask intrinsic
    bool markValueAs(Value* value, const char* mark);

    ///////////////////////////////////////////////////////////////////////////
    // Place marks according to user calls to addSIMDMapping / addSIMDSemantics.
    ///////////////////////////////////////////////////////////////////////////

    void setMarksFromOutside(Value*                   value,
                             const rv::ValueInfoMap& valueInfoMap);
    void setUserDefinedMarks(Function*                   scalarFn,
                             const rv::ValueInfoMap&    valueInfoMap,
                             const rv::FunctionInfoMap& functionInfoMap);

    ///////////////////////////////////////////////////////////////////////////
    // No Analysis
    ///////////////////////////////////////////////////////////////////////////

    void analyzeNothing(Function* scalarFn, const bool uniformReturn);

    ///////////////////////////////////////////////////////////////////////////
    // Uniform Analysis
    ///////////////////////////////////////////////////////////////////////////

    void analyzeUniformInfo(Function*                   scalarFn,
                            const bool                  uniformReturn,
                            const SmallVector<bool, 4>& uniformArgs);

    bool recursivelyMarkVarying(Instruction* inst, BasicBlock* block);

    ///////////////////////////////////////////////////////////////////////////
    // Divergence Analysis
    ///////////////////////////////////////////////////////////////////////////


    bool markAlwaysByAllBlocks(Function* scalarFn);
    bool markDivergentBlocks(Function* scalarFn);

    bool markMandatoryBlocks(Function* scalarFn);
    bool markABAONBlocks(Function* scalarFn);

    // efficient implementation of the disjoint paths criterion
    // used for isDivergent() [works]
    bool checkForDisjointPaths(const BasicBlock* block,
    				           bool doNotBranchToOuterLoops) const;

    // used for isMandatoyExit() [work-in-progress]
    bool checkForDisjointLoopPaths(const BasicBlock*             block,
    				               const Loop * loop,
					               BlockSet * divCauseBlocks) const;

    Loop *
    getCommonLoop(const BasicBlock * A, const BasicBlock * B) const;

    bool isDivergent(BasicBlock* block) const;
    bool isMandatory(const BasicBlock* block,
                     const LoopInfo&   loopInfo) const;
    bool isMandatoryExit(const BasicBlock* block,
                         const Loop*       loop) const;

    ///////////////////////////////////////////////////////////////////////////
    // Updating of phis that depend on divergent control-flow.
    ///////////////////////////////////////////////////////////////////////////

    bool updateUniformPhisWithDivergenceInfo(Function* scalarFn);

    ///////////////////////////////////////////////////////////////////////////
    // Updating of operations with possible side effects that depend on
    // divergent control-flow.
    ///////////////////////////////////////////////////////////////////////////

    bool updateUniformSideEffectOperations(Function* scalarFn);

    ///////////////////////////////////////////////////////////////////////////
    // Updating of uniform allocas.
    ///////////////////////////////////////////////////////////////////////////

    bool updateUniformAllocas(Function* scalarFn);

    ///////////////////////////////////////////////////////////////////////////
    // Updating of values whose live range crosses a loop boundary.
    ///////////////////////////////////////////////////////////////////////////

    bool updateUniformLALBValues(Function* scalarFn);
    bool updateUniformLALBValues(Loop* loop);
    PHINode* findLoopPhiForInstruction(Instruction* inst, Loop* loop);

    ///////////////////////////////////////////////////////////////////////////
    // Marking of loops.
    ///////////////////////////////////////////////////////////////////////////

    bool markDivergentLoops(const LoopInfo& loopInfo);
    bool markDivergentLoop(Loop* loop, const LoopInfo& loopInfo);
    bool isLoopDivergent(Loop* loop, const LoopInfo& loopInfo);

	void markNestedDivergentTopLevelLoops();
	void markNestedDivergentTopLevelLoops(Loop* loop);

    ///////////////////////////////////////////////////////////////////////////
    // Index & Aliasing Analysis.
    ///////////////////////////////////////////////////////////////////////////

    unsigned mVectorizationFactor;

    struct IndexAlignedInfo
    {
        IndexAlignedInfo() : indexInfo(NULL), alignInfo(NULL) {}
        IndexAlignedInfo(const char* ii, const char*ai) : indexInfo(ii), alignInfo(ai) {}
        const char* indexInfo;
        const char* alignInfo;
    };

    void analyzeConsecutiveAlignedInfo(Function* scalarFn);
    void markIndexAlignValueAndOps(Value*                   value,
                                   SmallPtrSet<Value*, 64>& markedValues,
                                   const char**             indexInfo,
                                   const char**             alignInfo);
    void getIndexAlignedInfo(Value*       value,
                             const char** indexInfo,
                             const char** alignInfo) const;
    const char* deriveIndexInfo(Instruction* inst,
                                const std::vector<const char*>& iiVec) const;
    const char* deriveAlignmentInfo(Instruction* inst,
                                    const std::vector<const char*>& aiVec) const;
    const char* deriveIndexInfo(const Constant* c) const;
    const char* deriveAlignedInformation(const Constant* c) const;

    ///////////////////////////////////////////////////////////////////////////
    // Splitting Analysis.
    ///////////////////////////////////////////////////////////////////////////

    void analyzeSplitInfo(Function* scalarFn);

    bool requiresScalarResults(const Instruction& inst) const;
    bool mayRequireGuards(const Instruction& inst) const;

    bool requiresSplit(const Instruction& inst) const;

    bool requiresSplit(const LoadInst& load) const;
    bool requiresSplit(const StoreInst& store) const;
    bool requiresSplit(const GetElementPtrInst& gep) const;
    bool requiresSplit(const PHINode& phi) const;
    bool requiresSplit(const SelectInst& select) const;
    bool requiresSplit(const CallInst& call) const;

    bool hasVaryingIndex(const GetElementPtrInst& gep) const;

    bool requiresSpecialHandling(const Instruction& inst) const;
    bool hasSequentialOperand(const Instruction& inst) const;
    bool operandRequiresSequentialExec(const Value& value) const;

    ///////////////////////////////////////////////////////////////////////////
    // Mask Analysis.
    ///////////////////////////////////////////////////////////////////////////

	void analyzeMaskInfo(Function& scalarFn);
	void markAsMask(Instruction* inst);

	void checkLoop(const Loop* L);
	void checkEquivalentToNewAnalysis(Function& F);
};

//} // unnamed namespace

namespace RV {
bool VerifyVectorizationAnalysis(const Function& f);
}


// Forward declaration of initializer and public interface.
namespace llvm {
void initializeVectorizationAnalysisWrapperPass(PassRegistry&);
FunctionPass* createVectorizationAnalysisPass();
}


#endif	/* _VECTORIZATIONANALYSIS_H */
