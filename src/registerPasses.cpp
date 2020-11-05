//===- src/registerPasses.cpp - LLVM-pass registry boilerplate --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "rv/passes.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

using namespace llvm;

cl::OptionCategory rvCategory("RV Options", "Configure the Region Vectorizer");

static cl::opt<bool> rvLowerBuiltins("rv-lower",
                                     cl::desc("Lower RV specific builtins"),
                                     cl::init(false), cl::ZeroOrMore,
                                     cl::cat(rvCategory));

static cl::opt<bool>
    rvLoopVecEnabled("rv-loopvec",
                     cl::desc("Enable RV's outer-loop vectorizer."),
                     cl::init(false), cl::ZeroOrMore, cl::cat(rvCategory));

static cl::opt<bool>
    rvWFVEnabled("rv-wfv", cl::desc("Enable RV's whole-function vectorizer."),
                 cl::init(false), cl::ZeroOrMore, cl::cat(rvCategory));

static cl::opt<bool> rvOnlyPolish("rv-polish",
                                  cl::desc("Only run RV's polish phase."),
                                  cl::init(false), cl::ZeroOrMore,
                                  cl::cat(rvCategory));

static cl::opt<bool> rvVectorizeEnabled(
    "rv",
    cl::desc("Enable Whole-Function and Outer-Loop Vectorization with RV "
             "(implies -rv-wfv and -rv-loopvec)."),
    cl::init(false), cl::ZeroOrMore, cl::cat(rvCategory));

static bool mayVectorize() {
  return rvWFVEnabled || rvLoopVecEnabled || rvVectorizeEnabled;
}
static bool shouldRunWFVPass() { return rvWFVEnabled || rvVectorizeEnabled; }
static bool shouldRunLoopVecPass() {
  return rvLoopVecEnabled || rvVectorizeEnabled;
}
static bool shouldLowerBuiltins() { return rvLowerBuiltins; }

///// Legacy PM pass registration /////
static void registerRVPasses(const llvm::PassManagerBuilder &Builder,
                             llvm::legacy::PassManagerBase &PM) {
  if (rvOnlyPolish) {
    PM.add(rv::createIRPolisherWrapperPass());
    return;
  }

  if (mayVectorize()) {
    rv::addPreparatoryPasses(PM);
  }

  if (shouldRunWFVPass()) {
    rv::addWholeFunctionVectorizer(PM);
  }
  if (shouldRunLoopVecPass()) {
    rv::addOuterLoopVectorizer(PM);
  }

  if (mayVectorize()) {
    rv::addCleanupPasses(PM);
  }
}

static void registerLateRVPasses(const llvm::PassManagerBuilder &Builder,
                                 llvm::legacy::PassManagerBase &PM) {
  if (shouldLowerBuiltins()) {
    rv::addLowerBuiltinsPass(PM);
  }
}

static llvm::RegisterStandardPasses
    RegisterRV_MidPipeline(llvm::PassManagerBuilder::EP_VectorizerStart,
                           registerRVPasses);

static llvm::RegisterStandardPasses
    RegisterRV_Late(llvm::PassManagerBuilder::EP_ScalarOptimizerLate,
                    registerLateRVPasses);
