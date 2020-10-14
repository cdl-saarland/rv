//===- rv/transform/loopExitCanonicalizer.h - exit : exiting == 1:1  --*- C++
//-*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#ifndef _LOOPEXITCANONICALIZER_H
#define _LOOPEXITCANONICALIZER_H

#include "llvm/Pass.h"
#include "llvm/IR/BasicBlock.h"

namespace llvm {
class LoopInfo;
class Loop;
class BasicBlock;
} // namespace llvm

namespace rv {
class RVInfo;
}

// namespace {

class LoopExitCanonicalizerWrapper : public llvm::FunctionPass {
public:
  static char ID; // Pass identification, replacement for typeid.

  LoopExitCanonicalizerWrapper();

  void releaseMemory() override;
  void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;
  bool doInitialization(llvm::Module &M) override;
  bool doFinalization(llvm::Module &M) override;
  bool runOnFunction(llvm::Function &F) override;
  void print(llvm::raw_ostream &O, const llvm::Module *M) const override;
};

class LoopExitCanonicalizer {
public:
  LoopExitCanonicalizer(llvm::LoopInfo &loopInfo);
  ~LoopExitCanonicalizer();

  bool canonicalize(llvm::Function &F);

private:
  llvm::LoopInfo &mLoopInfo;

  void canonicalizeLoop(llvm::Loop *loop) const;
  llvm::BasicBlock *createIntermediateBlock(llvm::BasicBlock *source,
                                            llvm::BasicBlock *target) const;
  void adjustPhis(llvm::BasicBlock *source, llvm::BasicBlock *target,
                  llvm::BasicBlock *newTarget) const;
  void replaceTarget(llvm::BasicBlock *source, llvm::BasicBlock *target,
                     llvm::BasicBlock *newTarget) const;
};

//} // unnamed namespace

// Forward declaration of initializer and public interface.
namespace llvm {
void initializeLoopExitCanonicalizerWrapperPass(PassRegistry &);
FunctionPass *createLoopExitCanonicalizerPass();
} // namespace llvm

#endif /* _LOOPEXITCANONICALIZER_H */
