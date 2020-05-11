//===- src/passes.cpp - create RV passes --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "rv/passes.h"

#include "rv/transform/loopExitCanonicalizer.h"
#include "llvm/Transforms/AggressiveInstCombine/AggressiveInstCombine.h"
#include "llvm/Transforms/IPO/AlwaysInliner.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Utils.h"

using namespace llvm;

namespace rv {

void addPreparatoryPasses(legacy::PassManagerBase &PM) {
  PM.add(createLoopSimplifyPass());
  PM.add(createLCSSAPass());
  PM.add(createLoopExitCanonicalizerPass()); // required for divLoopTrans
}

void addCleanupPasses(legacy::PassManagerBase &PM) {
  // post rv cleanup
  PM.add(createAlwaysInlinerLegacyPass());
  PM.add(createAggressiveInstCombinerPass());
  PM.add(createAggressiveDCEPass());
}

void addOuterLoopVectorizer(legacy::PassManagerBase &PM) {
  PM.add(rv::createLoopVectorizerPass());
}

void addAutoMathPass(llvm::legacy::PassManagerBase &PM) {
  PM.add(rv::createAutoMathPass());
}

void addWholeFunctionVectorizer(llvm::legacy::PassManagerBase &PM) {
  PM.add(rv::createWFVPass());
}

void addLowerBuiltinsPass(legacy::PassManagerBase &PM) {
  PM.add(rv::createLowerRVIntrinsicsPass());
}

void addRVPasses(llvm::legacy::PassManagerBase &PM) {
  // normalize loops
  addPreparatoryPasses(PM);

  // supplement vector math functions for select targets using RV's resolver API
  addAutoMathPass(PM);

  // vectorize scalar functions that have VectorABI attributes
  addWholeFunctionVectorizer(PM);

  // vectorize annotated loops
  addOuterLoopVectorizer(PM);

  // DCE, instcombine, ..
  addCleanupPasses(PM);
}

} // namespace rv
