//===------ registerPasses.cpp - Add RV's Passes to default passes  --===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "rv/passes.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"

using namespace llvm;

cl::OptionCategory rvCategory("RV Options",
                                 "Configure the Region Vectorizer");


static cl::opt<bool>
    rvLoopVecEnabled("rv-loopvec", cl::desc("Enable RV's outer-loop vectorizer."),
                 cl::init(false), cl::ZeroOrMore, cl::cat(rvCategory));

static cl::opt<bool>
    rvWFVEnabled("rv-wfv", cl::desc("Enable RV's whole-function vectorizer."),
                 cl::init(false), cl::ZeroOrMore, cl::cat(rvCategory));

static cl::opt<bool>
    rvOnlyPolish("rv-polish", cl::desc("Only run RV's polish phase."),
                 cl::init(false), cl::ZeroOrMore, cl::cat(rvCategory));

static cl::opt<bool>
    rvVectorizeEnabled("rv", cl::desc("Enable Whole-Function and Outer-Loop Vectorization with RV (implies -rv-wfv and -rv-loopvec)."),
                 cl::init(false), cl::ZeroOrMore, cl::cat(rvCategory));

static cl::opt<bool>
    rvOnlyCNS("rv-cns", cl::desc("Only run RV's Irreducible Loop Normalizer."),
                 cl::init(false), cl::ZeroOrMore, cl::cat(rvCategory));

static bool mayVectorize() { return rvWFVEnabled || rvLoopVecEnabled || rvVectorizeEnabled; }
static bool shouldRunWFVPass() { return rvWFVEnabled || rvVectorizeEnabled; }
static bool shouldRunLoopVecPass() { return rvLoopVecEnabled || rvVectorizeEnabled; }

static void
registerRVPasses(const llvm::PassManagerBuilder &Builder,
                                       llvm::legacy::PassManagerBase &PM) {
  if (rvOnlyPolish) {
    PM.add(rv::createIRPolisherWrapperPass());
    return;
  }

  if (rvOnlyCNS) {
    PM.add(rv::createCNSPass());
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


static llvm::RegisterStandardPasses RegisterRVOptimizerScalarLate(
    llvm::PassManagerBuilder::EP_VectorizerStart,
    registerRVPasses);


