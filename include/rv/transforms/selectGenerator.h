//===- selectGenerator.h -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//

#ifndef _SELECTGENERATOR_H
#define	_SELECTGENERATOR_H

#include <llvm/Pass.h>
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/SmallPtrSet.h>
#include <llvm/ADT/SmallVector.h>
#include <rv/vectorizationInfo.h>

class MaskAnalysis;
class LoopLiveValueAnalysis;

namespace llvm {
class Value;
class Instruction;
class Module;
class Function;
class SelectInst;
class PHINode;
class Loop;
class LoopInfo;
}

using namespace llvm;

namespace rv {
  class VectorizationInfo;
  class RVInfo;
}

using namespace rv;

//namespace {

class SelectGeneratorWrapper : public FunctionPass
{
public:
	static char ID __attribute__((unused)); // Pass identification, replacement for typeid.

	SelectGeneratorWrapper();

	virtual void releaseMemory   ();
	virtual void getAnalysisUsage(AnalysisUsage &AU) const;
	virtual bool doInitialization(Module& M);
	virtual bool doFinalization  (Module& M);
	virtual bool runOnFunction   (Function& F);
	virtual void print           (raw_ostream& O, const Module* M) const;
};

class SelectGenerator
{
public:
	SelectGenerator(const rv::RVInfo& rvinfo,
					const LoopInfo& loopinfo,
					const MaskAnalysis& maskAnalysis,
					LoopLiveValueAnalysis& loopLiveValueAnalysis,
					VectorizationInfo& vectorizationInfo);

	~SelectGenerator();

    bool generate(Function& F);

private:
    const rv::RVInfo&           mInfo;
    const LoopInfo&          mLoopInfo;
    const MaskAnalysis&      mMaskAnalysis;
    LoopLiveValueAnalysis&   mLoopLiveValueAnalysis;
	VectorizationInfo&       mvecInfo;

    void generatePhiSelects(Function* f);
    Value* generateSelectFromPhi(PHINode* phi);

    void generateLoopSelects(Function* f);

    void generateMultipleExitLoopSelects(Loop*                        loop,
                                         SmallPtrSet<SelectInst*, 8>& selectSet);
    bool hasLiveValueResult(const Loop* loop, const Instruction* liveValue) const;

    void replaceDirectParentLoopUsesOfWith(Value* oldValue, Value* newValue, Loop* loop);
    void replaceNonLoopUsesOfWith(Value* oldValue, Value* newValue, Loop* loop);
    bool isContainedInSomeParentLoop(BasicBlock* block, Loop* loop);
};

//} // unnamed namespace


// Forward declaration of initializer and public interface.
namespace llvm {
void initializeSelectGeneratorWrapperPass(PassRegistry&);
FunctionPass* createSelectGeneratorPass();
}


#endif	/* _SELECTGENERATOR_H */
