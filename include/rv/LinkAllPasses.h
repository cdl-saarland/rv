//===- rv/LinkAllPasses.h -------------- Reference All Passes ---*- C++ -*-===//
//
//                      The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This header file pulls in all transformation and analysis passes for tools
// like opt and bugpoint that need this functionality.
//
//===----------------------------------------------------------------------===//

#ifndef RV_LINKALLPASSES_H
#define RV_LINKALLPASSES_H

#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "rv/passes.h"

#include <cstdlib>

namespace llvm {
class Pass;
class PassRegistry;

void initializeLoopVectorizerPass(PassRegistry&);
void initializeIRPolisherWrapperPass(PassRegistry&);
void initializeCNSPass(PassRegistry&);
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

    rv::createLoopVectorizerPass();
    rv::createIRPolisherWrapperPass();
    rv::createCNSPass();
  }
} RVForcePassLinking; // Force link by creating a global definition.

} // namespace

#endif
