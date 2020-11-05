//===- src/init.cpp - LLVM-pass init boilerplate --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
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
    llvm::initializeWFVPassPass(Registry);
  }
};
static StaticInitializer InitializeEverything;
} // end of anonymous namespace.
