//===- rv/passes/loopExitCanonicalizer.h - exit : exiting == 1:1  --*- C++
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

#include "llvm/IR/BasicBlock.h"

namespace llvm {

class LoopInfo;
class Loop;

} // namespace llvm

class LoopExitCanonicalizer {
public:
  LoopExitCanonicalizer(llvm::LoopInfo &loopInfo);
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

#endif /* _LOOPEXITCANONICALIZER_H */
