//===- rv/legacy/passes.h - create RV passes --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef RV_LEGACY_PASSES_H
#define RV_LEGACY_PASSES_H

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Pass.h"

namespace rv {

/// Predefined pipelines.
// Add all passes of RV to the pass pipeline PM.
void addRVLegacyPasses(llvm::legacy::PassManagerBase &);

// Add normalization passes required by RV (BEFORE)
void addPreparatoryLegacyPasses(llvm::legacy::PassManagerBase &);

// Add cleanup passes to run after RV (AFTER)
void addCleanupLegacyPasses(llvm::legacy::PassManagerBase &);

// Legacy passes' createX functions.
llvm::FunctionPass *createIRPolisherLegacyPass();
llvm::FunctionPass *createLoopVectorizerLegacyPass();
llvm::FunctionPass *createLowerRVIntrinsicsLegacyPass();
llvm::FunctionPass *createLoopExitCanonicalizerLegacyPass();
llvm::ModulePass *createAutoMathLegacyPass();
llvm::ModulePass *createWFVLegacyPass();

} // namespace rv

#endif // RV_LEGACY_PASSES
