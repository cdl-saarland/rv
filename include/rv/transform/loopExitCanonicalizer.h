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


//namespace {

class LoopExitCanonicalizerWrapper : public llvm::FunctionPass
{
public:
	static char ID; // Pass identification, replacement for typeid.

	LoopExitCanonicalizerWrapper();

	virtual void releaseMemory   ();
	virtual void getAnalysisUsage(llvm::AnalysisUsage &AU) const;
	virtual bool doInitialization(llvm::Module& M);
	virtual bool doFinalization  (llvm::Module& M);
	virtual bool runOnFunction   (llvm::Function& F);
	virtual void print           (llvm::raw_ostream& O, const llvm::Module* M) const;
};

class LoopExitCanonicalizer
{
public:
	LoopExitCanonicalizer(llvm::LoopInfo& loopInfo);
	~LoopExitCanonicalizer();

	bool canonicalize(llvm::Function& F);

private:
	  llvm::LoopInfo&      mLoopInfo;

    void canonicalizeLoop(llvm::Loop* loop) const;
	  llvm::BasicBlock* createIntermediateBlock(llvm::BasicBlock* source,
																							llvm::BasicBlock* target) const;
    void adjustPhis(llvm::BasicBlock* source,
										llvm::BasicBlock* target,
										llvm::BasicBlock* newTarget) const;
    void replaceTarget(llvm::BasicBlock* source,
											 llvm::BasicBlock* target,
											 llvm::BasicBlock* newTarget) const;
};

//} // unnamed namespace


// Forward declaration of initializer and public interface.
namespace llvm {
void initializeLoopExitCanonicalizerWrapperPass(PassRegistry&);
FunctionPass* createLoopExitCanonicalizerPass();
}


#endif	/* _LOOPEXITCANONICALIZER_H */
