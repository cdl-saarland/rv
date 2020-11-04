//===- src/passes.cpp - create RV passes --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "rv/passes.h"

#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Utils.h"
#include "llvm/Transforms/AggressiveInstCombine/AggressiveInstCombine.h"
#include "llvm/Transforms/IPO/AlwaysInliner.h"
#include "rv/transform/loopExitCanonicalizer.h"
#include "rv/transform/WFVPass.h"
#include "rv/transform/LoopVectorizer.h"
#include "rv/transform/lowerRVIntrinsics.h"

#include <llvm/Transforms/Scalar/ADCE.h>
#include <llvm/Transforms/Utils/LCSSA.h>
#include <llvm/Transforms/Utils/LoopSimplify.h>

using namespace llvm;

namespace rv {



void
addPreparatoryPasses(legacy::PassManagerBase & PM) {
   PM.add(createLoopSimplifyPass());
   PM.add(createLCSSAPass());
   PM.add(createLoopExitCanonicalizerPass()); // required for divLoopTrans
}

void
addCleanupPasses(legacy::PassManagerBase & PM) {
   // post rv cleanup
   PM.add(createAlwaysInlinerLegacyPass());
   PM.add(createAggressiveInstCombinerPass());
   PM.add(createAggressiveDCEPass());
}

void
addOuterLoopVectorizer(legacy::PassManagerBase & PM) {
   PM.add(rv::createLoopVectorizerPass());
}


void
addWholeFunctionVectorizer(legacy::PassManagerBase & PM) {
  PM.add(rv::createWFVPass());
}

void
addLowerBuiltinsPass(legacy::PassManagerBase & PM) {
   PM.add(rv::createLowerRVIntrinsicsPass());
}

void
addRVPasses(legacy::PassManagerBase & PM) {
  // normalize loops
  addPreparatoryPasses(PM);

  // vectorize scalar functions that have VectorABI attributes
  addWholeFunctionVectorizer(PM);

  // vectorize annotated loops
  addOuterLoopVectorizer(PM);

  // DCE, instcombine, ..
  addCleanupPasses(PM);
}

///// New PM Registration /////
void
addPreparatoryPasses(FunctionPassManager & FPM) {
  FPM.addPass(LoopSimplifyPass());
  FPM.addPass(LCSSAPass());
  FPM.addPass(rv::LoopExitCanonicalizerWrapperPass()); // required for divLoopTrans
}

void
addPreparatoryPasses(ModulePassManager & MPM) {
  llvm::FunctionPassManager FPM;
  addPreparatoryPasses(FPM);
  MPM.addPass(llvm::createModuleToFunctionPassAdaptor(std::move(FPM)));
}

void
addCleanupPasses(ModulePassManager & MPM) {
  // post rv cleanup
  MPM.addPass(AlwaysInlinerPass());
  llvm::FunctionPassManager FPM;
  FPM.addPass(AggressiveInstCombinePass());
  FPM.addPass(ADCEPass());
  MPM.addPass(llvm::createModuleToFunctionPassAdaptor(std::move(FPM)));
}

void
addOuterLoopVectorizer(FunctionPassManager & FPM) {
  FPM.addPass(rv::LoopVectorizerWrapperPass());
}

void
addOuterLoopVectorizer(ModulePassManager & MPM) {
  llvm::FunctionPassManager FPM;
  addOuterLoopVectorizer(FPM);
  MPM.addPass(llvm::createModuleToFunctionPassAdaptor(std::move(FPM)));
}

void
addWholeFunctionVectorizer(ModulePassManager & MPM) {
  MPM.addPass(rv::WFVWrapperPass());
}

void
addLowerBuiltinsPass(FunctionPassManager & FPM) {
  FPM.addPass(rv::LowerRVIntrinsicsWrapperPass());
}

void
addLowerBuiltinsPass(ModulePassManager & MPM) {
  llvm::FunctionPassManager FPM;
  addLowerBuiltinsPass(FPM);
  MPM.addPass(llvm::createModuleToFunctionPassAdaptor(std::move(FPM)));
}

void
addRVPasses(ModulePassManager & MPM) {
  // normalize loops
  addPreparatoryPasses(MPM);

  // vectorize scalar functions that have VectorABI attributes
  addWholeFunctionVectorizer(MPM);

  // vectorize annotated loops
  addOuterLoopVectorizer(MPM);

  // DCE, instcombine, ..
  addCleanupPasses(MPM);
}

}
