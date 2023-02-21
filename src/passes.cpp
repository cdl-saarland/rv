//===- src/passes.cpp - create RV passes --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "rv/passes.h"

#include "llvm/Transforms/AggressiveInstCombine/AggressiveInstCombine.h"
#include "llvm/Transforms/IPO/AlwaysInliner.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Utils.h"

#include "rv/passes/AutoMathPass.h"
#include "rv/passes/LoopVectorizer.h"
#include "rv/passes/WFVPass.h"
#include "rv/passes/loopExitCanonicalizer.h"
#include "rv/passes/lowerRVIntrinsics.h"

#include "llvm/Transforms/Scalar/ADCE.h"
#include "llvm/Transforms/Utils/LCSSA.h"
#include "llvm/Transforms/Utils/LoopSimplify.h"

#include "llvm/Transforms/Scalar/LICM.h"

#include "report.h"

using namespace llvm;

namespace rv {

void addPreparatoryPasses(FunctionPassManager &FPM) {
  FPM.addPass(LoopSimplifyPass());
  FPM.addPass(LCSSAPass());
  FPM.addPass(
      LoopExitCanonicalizerWrapperPass()); // required for divLoopTrans
  // FPM.addPass(LICMPass());
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

void addRVPasses(ModulePassManager &MPM) {
  // normalize loops
  addPreparatoryPasses(MPM);

  // supplement vector math functions for select targets using RV's resolver API
  MPM.addPass(AutoMathWrapperPass());

  // vectorize scalar functions that have VectorABI attributes
  MPM.addPass(WFVWrapperPass());

  // vectorize annotated loops
  llvm::FunctionPassManager FPM;
  FPM.addPass(LoopVectorizerWrapperPass());
  MPM.addPass(llvm::createModuleToFunctionPassAdaptor(std::move(FPM)));

  // DCE, instcombine, ..
  addCleanupPasses(MPM);
}

} // namespace rv
