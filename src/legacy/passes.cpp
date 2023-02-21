//===- legacy/passes.cpp - create (legacy) RV passes --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "rv/legacy/passes.h"

#include "llvm/Transforms/AggressiveInstCombine/AggressiveInstCombine.h"
#include "llvm/Transforms/IPO/AlwaysInliner.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Utils.h"

#include "rv/passes/loopExitCanonicalizer.h"
#include "rv/passes/LoopVectorizer.h"
#include "rv/passes/WFVPass.h"
#include "rv/passes/loopExitCanonicalizer.h"
#include "rv/passes/lowerRVIntrinsics.h"

#include "llvm/Transforms/Scalar/ADCE.h"
#include "llvm/Transforms/Utils/LCSSA.h"
#include "llvm/Transforms/Utils/LoopSimplify.h"

#include "report.h"

using namespace llvm;

namespace rv {

void addPreparatoryLegacyPasses(legacy::PassManagerBase &PM) {
  PM.add(createLICMPass());
  PM.add(createPromoteMemoryToRegisterPass());
  PM.add(createLoopSimplifyPass());
  PM.add(createLCSSAPass());
  PM.add(createLoopExitCanonicalizerLegacyPass()); // required for divLoopTrans
}

void addCleanupLegacyPasses(legacy::PassManagerBase &PM) {
  // post rv cleanup
  PM.add(createAlwaysInlinerLegacyPass());
  PM.add(createAggressiveInstCombinerPass());
  PM.add(createAggressiveDCEPass());
}

void addRVLegacyPasses(legacy::PassManagerBase &PM) {
  // normalize loops
  addPreparatoryLegacyPasses(PM);

  // supplement vector math functions for select targets using RV's resolver API
  PM.add(createAutoMathLegacyPass());

  // vectorize scalar functions that have VectorABI attributes
  PM.add(createWFVLegacyPass());

  // vectorize annotated loops
  PM.add(createLoopVectorizerLegacyPass());

  // DCE, instcombine, ..
  addCleanupLegacyPasses(PM);
}

} // namespace rv
