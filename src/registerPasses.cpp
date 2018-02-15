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
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/IPO/AlwaysInliner.h"
#include "rv/transform/loopExitCanonicalizer.h"

using namespace llvm;

cl::OptionCategory rvCategory("RV Options",
                                 "Configure the rv-based loop vectorizer");


static cl::opt<bool>
    rvLoopVecEnabled("rv-loopvec", cl::desc("Enable RV's outer-loop vectorizer"),
                 cl::init(false), cl::ZeroOrMore, cl::cat(rvCategory));

static cl::opt<bool>
    rvOnlyPolish("rv-polish", cl::desc("Only run RV's polish phase"),
                 cl::init(false), cl::ZeroOrMore, cl::cat(rvCategory));


static cl::opt<bool>
    rvOnlyCNS("rv-cns", cl::desc("Only run RV's Irreducible Loop Normalizer"),
                 cl::init(false), cl::ZeroOrMore, cl::cat(rvCategory));

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

  if (rvLoopVecEnabled) {
    rv::addOuterLoopVectorizer(PM);
  }
}


static llvm::RegisterStandardPasses RegisterRVOptimizerScalarLate(
    llvm::PassManagerBuilder::EP_VectorizerStart,
    registerRVPasses);


