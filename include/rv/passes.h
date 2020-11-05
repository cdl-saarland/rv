//===- rv/LinkAllPasses.h - create RV passes --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef RV_PASSES_H
#define RV_PASSES_H

#include "llvm/Pass.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/PassManager.h"
#include "rv/config.h"

namespace rv {


  // add all passes of RV to the pass pipeline PM.
  void addRVPasses(llvm::legacy::PassManagerBase & PM);

  // add all passes of RV to the pass pipeline MPM.
  void addRVPasses(llvm::ModulePassManager & MPM);

// fine-grained pass adding
  // RV-based loop vectorizer pass
  llvm::FunctionPass *createLoopVectorizerPass();

  // Whole-Function Vectorizer pass
  llvm::ModulePass *createWFVPass();

  // vector IR polisher
  llvm::FunctionPass *createIRPolisherWrapperPass(Config config = Config());

  // Controlled Node Splitting (Irreducible loop normalization)
  llvm::FunctionPass *createLowerRVIntrinsicsPass();

  // add normalization passes required by RV (BEFORE)
  void addPreparatoryPasses(llvm::legacy::PassManagerBase & PM);

  // add RV's outer loop vectorizer and required passes to @PM
  void addOuterLoopVectorizer(llvm::legacy::PassManagerBase & PM);

  // add RV's whole function and required passes to @PM
  void addWholeFunctionVectorizer(llvm::legacy::PassManagerBase & PM);

  // add cleanup passes to run after RV (AFTER)
  void addCleanupPasses(llvm::legacy::PassManagerBase & PM);

  // insert a pass that
  void addLowerBuiltinsPass(llvm::legacy::PassManagerBase & PM);

  ///// New PM Registration /////

  // add normalization passes required by RV (BEFORE)
  void addPreparatoryPasses(llvm::FunctionPassManager & FPM);
  void addPreparatoryPasses(llvm::ModulePassManager & MPM);

  // add cleanup passes to run after RV (AFTER)
  void addCleanupPasses(llvm::ModulePassManager & MPM);

  // add RV's outer loop vectorizer and required passes.
  void addOuterLoopVectorizer(llvm::FunctionPassManager & FPM);
  void addOuterLoopVectorizer(llvm::ModulePassManager & MPM);

  // add RV's whole function and required passes.
  void addWholeFunctionVectorizer(llvm::ModulePassManager & MPM);

  // insert a pass that
  void addLowerBuiltinsPass(llvm::FunctionPassManager & FPM);
  void addLowerBuiltinsPass(llvm::ModulePassManager & MPM);

} // namespace rv


#endif
