//===- src/registerPasses.cpp - LLVM-pass registry boilerplate --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "rv/registerPasses.h"
#include "rv/legacy/passes.h"
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

static cl::opt<bool> rvPrep("rv-prep",
                                  cl::desc("Run IR canonicalizations."),
                                  cl::init(false), cl::ZeroOrMore,
                                  cl::cat(rvCategory));

static cl::opt<bool> rvVectorizeEnabled(
    "rv",
    cl::desc("Enable Whole-Function and Outer-Loop Vectorization with RV "
             "(implies -rv-wfv and -rv-loopvec)."),
    cl::init(false), cl::ZeroOrMore, cl::cat(rvCategory));

static cl::opt<bool>
    rvAutoVectorizeMath("rv-math",
                        cl::desc("Use RV to auto-vectorize libm calls."),
                        cl::init(true), cl::ZeroOrMore, cl::cat(rvCategory));

static bool mayVectorize() {
  return rvWFVEnabled || rvLoopVecEnabled || rvVectorizeEnabled;
}
static bool shouldRunWFVPass() { return rvWFVEnabled || rvVectorizeEnabled; }
static bool shouldRunLoopVecPass() {
  return rvLoopVecEnabled || rvVectorizeEnabled;
}
static bool shouldLowerBuiltins() { return rvLowerBuiltins; }
static bool shouldAutoVectorizeMath() { return rvAutoVectorizeMath; }

///// Legacy PM pass registration /////
static void registerRVPasses(const llvm::PassManagerBuilder &Builder,
                             llvm::legacy::PassManagerBase &PM) {
  if (rvOnlyPolish) {
    PM.add(rv::createIRPolisherWrapperPass());
    return;
  }

  if (rvPrep || mayVectorize()) {
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

static void registerLastRVPasses(const llvm::PassManagerBuilder &Builder,
                                 llvm::legacy::PassManagerBase &PM) {
  if (shouldAutoVectorizeMath()) {
    rv::addAutoMathPass(PM);
  }
}

static llvm::RegisterStandardPasses
    RegisterRV_MidPipeline(llvm::PassManagerBuilder::EP_VectorizerStart,
                           registerRVPasses);

static llvm::RegisterStandardPasses
    RegisterRV_Late(llvm::PassManagerBuilder::EP_ScalarOptimizerLate,
                    registerLateRVPasses);

static llvm::RegisterStandardPasses
    RegisterRV_Last(llvm::PassManagerBuilder::EP_OptimizerLast,
                    registerLastRVPasses);

///// New PM setup /////

void addFunctionPasses(ModulePassManager &MPM,
                       std::function<void(llvm::FunctionPassManager &)> Adder) {
  llvm::FunctionPassManager FPM;
  Adder(FPM);
  MPM.addPass(llvm::createModuleToFunctionPassAdaptor(std::move(FPM)));
}

static bool
buildDefaultRVPipeline(StringRef, ModulePassManager &MPM,
                       ArrayRef<PassBuilder::PipelineElement> Elems) {
  // normalize loops
  rv::addPreparatoryPasses(MPM);

  // vectorize scalar functions that have VectorABI attributes
  rv::addWholeFunctionVectorizer(MPM);

  // vectorize annotated loops
  addFunctionPasses(MPM, [](auto &FPM) { rv::addOuterLoopVectorizer(FPM); });

  // DCE, instcombine, ..
  rv::addCleanupPasses(MPM);

  return true; // FIXME
}

namespace rv {
void addConfiguredPasses(PassBuilder &PB) {
  PB.registerPipelineParsingCallback(buildDefaultRVPipeline);
}
} // namespace rv
