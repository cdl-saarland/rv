//===- loopExitCanonicalizer.h ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//

#ifndef _LOOPEXITCANONICALIZER_H
#define	_LOOPEXITCANONICALIZER_H

#include <llvm/Pass.h>

namespace llvm {
class LoopInfo;
class Loop;
}

namespace rv {
  class RVInfo;
}

using namespace llvm;


//namespace {

class LoopExitCanonicalizerWrapper : public FunctionPass
{
public:
	static char ID; // Pass identification, replacement for typeid.

	LoopExitCanonicalizerWrapper();

	virtual void releaseMemory   ();
	virtual void getAnalysisUsage(AnalysisUsage &AU) const;
	virtual bool doInitialization(Module& M);
	virtual bool doFinalization  (Module& M);
	virtual bool runOnFunction   (Function& F);
	virtual void print           (raw_ostream& O, const Module* M) const;
};

class LoopExitCanonicalizer
{
public:
	LoopExitCanonicalizer(LoopInfo& loopInfo);
	~LoopExitCanonicalizer();

	bool canonicalize(Function& F);

private:
    LoopInfo&      mLoopInfo;

    void canonicalizeLoop(Loop* loop) const;
    BasicBlock* createIntermediateBlock(BasicBlock* source,
                                        BasicBlock* target) const;
    void adjustPhis(BasicBlock* source,
                    BasicBlock* target,
                    BasicBlock* newTarget) const;
    void replaceTarget(BasicBlock* source,
                       BasicBlock* target,
                       BasicBlock* newTarget) const;
};

//} // unnamed namespace


// Forward declaration of initializer and public interface.
namespace llvm {
void initializeLoopExitCanonicalizerWrapperPass(PassRegistry&);
FunctionPass* createLoopExitCanonicalizerPass();
}


#endif	/* _LOOPEXITCANONICALIZER_H */
