//===---------- RV.cpp - Initialize the RV Passes -------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//===----------------------------------------------------------------------===//

#include "rv/LinkAllPasses.h"

#include "llvm/IR/LegacyPassManager.h"

namespace {

/// Initialize RV passes when library is loaded.
///
/// We use the constructor of a statically declared object to initialize the
/// different RV passes right after the RV library is loaded. This ensures
/// that the RV passes are available e.g. in the 'opt' tool.
class StaticInitializer {
public:
  StaticInitializer() {
    llvm::PassRegistry &Registry = *llvm::PassRegistry::getPassRegistry();
    llvm::initializeLoopVectorizerPass(Registry);
    llvm::initializeIRPolisherWrapperPass(Registry);
    llvm::initializeCNSPass(Registry);
  }
};
static StaticInitializer InitializeEverything;
} // end of anonymous namespace.
