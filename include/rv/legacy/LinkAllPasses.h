//===- rv/LinkAllPasses.h - pass boilerplate header --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//


#ifndef RV_LINKALLPASSES_H
#define RV_LINKALLPASSES_H

#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "rv/legacy/passes.h"

#include <cstdlib>

namespace llvm {
class Pass;
class PassRegistry;

void initializeAutoMathLegacyPassPass(PassRegistry&);
void initializeIRPolisherLegacyPassPass(PassRegistry&);
void initializeLoopVectorizerLegacyPassPass(PassRegistry&);
void initializeLowerRVIntrinsicsLegacyPassPass(PassRegistry&);
void initializeWFVLegacyPassPass(PassRegistry&);
void initializeLoopExitCanonicalizerLegacyPassPass(PassRegistry&);
} // namespace llvm

namespace {
struct RVForcePassLinking {
  RVForcePassLinking() {
    // We must reference the passes in such a way that compilers will not
    // delete it all as dead code, even with whole program optimization,
    // yet is effectively a NO-OP. As the compiler isn't smart enough
    // to know that getenv() never returns -1, this will do the job.
    if (std::getenv("bar") != (char *)-1)
      return;

    rv::createAutoMathLegacyPass();
    rv::createIRPolisherLegacyPass();
    rv::createLoopVectorizerLegacyPass();
    rv::createLowerRVIntrinsicsLegacyPass();
    rv::createWFVLegacyPass();
    rv::createLoopExitCanonicalizerLegacyPass();
  }
} RVForcePassLinking; // Force link by creating a global definition.

} // namespace

#endif
