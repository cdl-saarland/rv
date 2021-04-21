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

#include "rv/transform/LoopVectorizer.h"
#include "rv/transform/WFVPass.h"
#include "rv/transform/loopExitCanonicalizer.h"
#include "rv/transform/lowerRVIntrinsics.h"
#include "rv/transform/AutoMathPass.h"
#include "rv/transform/OMPDeclutter.h"

#include "llvm/Transforms/Scalar/ADCE.h"
#include "llvm/Transforms/Utils/LCSSA.h"
#include "llvm/Transforms/Utils/LoopSimplify.h"

using namespace llvm;

namespace rv {

void addPreparatoryPasses(FunctionPassManager &FPM) {
  FPM.addPass(LoopSimplifyPass());
  FPM.addPass(LCSSAPass());
  FPM.addPass(
      LoopExitCanonicalizerWrapperPass()); // required for divLoopTrans
  FPM.addPass(OMPDeclutterWrapperPass());
}

void addPreparatoryPasses(ModulePassManager &MPM) {
  llvm::FunctionPassManager FPM;
  addPreparatoryPasses(FPM);
  MPM.addPass(llvm::createModuleToFunctionPassAdaptor(std::move(FPM)));
}

static void addCleanupPasses(FunctionPassManager &FPM) {
  // post rv cleanup
  FPM.addPass(AggressiveInstCombinePass());
  FPM.addPass(ADCEPass());
}

void addCleanupPasses(ModulePassManager &MPM) {
  // post rv cleanup
  MPM.addPass(AlwaysInlinerPass());
  llvm::FunctionPassManager FPM;
  addCleanupPasses(FPM);
  MPM.addPass(llvm::createModuleToFunctionPassAdaptor(std::move(FPM)));
}

void addOuterLoopVectorizer(FunctionPassManager &FPM) {
  FPM.addPass(rv::LoopVectorizerWrapperPass());
}

void addOuterLoopVectorizer(ModulePassManager &MPM) {
  llvm::FunctionPassManager FPM;
  addOuterLoopVectorizer(FPM);
  MPM.addPass(llvm::createModuleToFunctionPassAdaptor(std::move(FPM)));
}

void addAutoMathPass(llvm::ModulePassManager &MPM) {
  MPM.addPass(rv::AutoMathWrapperPass());
}

void addWholeFunctionVectorizer(ModulePassManager &MPM) {
  MPM.addPass(rv::WFVWrapperPass());
}

void addLowerBuiltinsPass(FunctionPassManager &FPM) {
  FPM.addPass(rv::LowerRVIntrinsicsWrapperPass());
}

void addLowerBuiltinsPass(ModulePassManager &MPM) {
  llvm::FunctionPassManager FPM;
  addLowerBuiltinsPass(FPM);
  MPM.addPass(llvm::createModuleToFunctionPassAdaptor(std::move(FPM)));
}

void addRVPasses(ModulePassManager &MPM) {
  // normalize loops
  addPreparatoryPasses(MPM);

  // supplement vector math functions for select targets using RV's resolver API
  addAutoMathPass(MPM);

  // vectorize scalar functions that have VectorABI attributes
  addWholeFunctionVectorizer(MPM);

  // vectorize annotated loops
  addOuterLoopVectorizer(MPM);

  // DCE, instcombine, ..
  addCleanupPasses(MPM);
}

} // namespace rv
