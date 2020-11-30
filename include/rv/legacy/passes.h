//===- rv/legacy_passes.h - create RV passes --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef RV_LEGACY_PASSES_H
#define RV_LEGACY_PASSES_H

#include "rv/config.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Pass.h"

namespace rv {

// add all passes of RV to the pass pipeline PM.
void addRVPasses(llvm::legacy::PassManagerBase &PM);

// fine-grained pass adding
// RV-based loop vectorizer pass
llvm::ModulePass *createAutoMathWrapperPass();
llvm::FunctionPass *createLoopVectorizerPass();
llvm::ModulePass *createAutoMathPass();
llvm::ModulePass *createWFVPass();
llvm::FunctionPass *createIRPolisherWrapperPass(Config config = Config());

// Controlled Node Splitting (Irreducible loop normalization)
llvm::FunctionPass *createLowerRVIntrinsicsPass();

// add normalization passes required by RV (BEFORE)
void addPreparatoryPasses(llvm::legacy::PassManagerBase &PM);

// add RV's outer loop vectorizer and required passes to @PM
void addOuterLoopVectorizer(llvm::legacy::PassManagerBase &PM);

// add the math function auto-vectorizer
void addAutoMathPass(llvm::legacy::PassManagerBase &PM);

// add RV's whole function and required passes to @PM
void addWholeFunctionVectorizer(llvm::legacy::PassManagerBase &PM);

// add cleanup passes to run after RV (AFTER)
void addCleanupPasses(llvm::legacy::PassManagerBase &PM);

// insert a pass that
void addLowerBuiltinsPass(llvm::legacy::PassManagerBase &PM);

} // namespace rv

#endif // RV_LEGACY_PASSES
