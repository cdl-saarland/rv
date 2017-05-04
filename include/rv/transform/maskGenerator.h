//===- maskGenerator.h ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//

#ifndef _MASKGENERATOR_H
#define	_MASKGENERATOR_H

#include <llvm/Pass.h>

#include "rv/analysis/maskAnalysis.h"


using namespace llvm; // FIXME no no


namespace rv {


class MaskGenerator
{
public:
    MaskGenerator(VectorizationInfo&, MaskAnalysis&, const LoopInfo&);
	~MaskGenerator();

    bool generate(Function& F);

private:
    VectorizationInfo& 	mvecInfo;
    MaskAnalysis&   	mMaskAnalysis;
    const LoopInfo& 	mLoopInfo;

    Type * boolTy;
    Value * mConstBoolTrue;
    Value * mConstBoolFalse;

    void markMaskOperation(Instruction& maskOp);
    void materializeMasks(Function& f);
    bool entryMaskIsUsed(const BasicBlock& block) const;
    Value* materializeMask(MaskPtr maskPtr);

    Value* createNeg(Value* operand, Instruction* insertBefore, const Twine& name);
    Value* createAnd(Value* operand0, Value* operand1, Instruction* insertBefore, const Twine& name);
    Value* createOr (Value* operand0, Value* operand1, Instruction* insertBefore, const Twine& name);
    Value* createSelect(Value*       operand0,
                        Value*       operand1,
                        Value*       operand2,
                        Instruction* insertPoint,
                        const Twine& name);
    Value* createPhi(Mask& mask, const Twine& name);

    bool hasNonUniformPhi(const BasicBlock& block) const;
    bool isHeaderOfDivergentLoop(const BasicBlock& block) const;

    void materializeLoopExitMasks(Loop* loop);
    void materializeCombinedLoopExitMasks(Loop* loop);

	void fillVecInfoWithPredicates(Function& F);
};

}

class MaskGeneratorWrapper : public FunctionPass
{
public:
	static char ID __attribute__((unused)); // Pass identification, replacement for typeid.

	MaskGeneratorWrapper();

	virtual void releaseMemory   ();
	virtual void getAnalysisUsage(AnalysisUsage &AU) const;
	virtual bool doInitialization(Module& M);
	virtual bool doFinalization  (Module& M);
	virtual bool runOnFunction   (Function& F);
	virtual void print           (raw_ostream& O, const Module* M) const;
};


// Forward declaration of initializer and public interface.
namespace llvm {
  void initializeMaskGeneratorWrapperPass(PassRegistry&);
  FunctionPass* createMaskGeneratorPass();
}


#endif	/* _MASKGENERATOR_H */
