//===------ registerPasses.cpp - Add RV's Passes to default passes  --===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file composes the individual LLVM-IR passes provided by Polly to a
// functional polyhedral optimizer. The polyhedral optimizer is automatically
// made available to LLVM based compilers by loading the Polly shared library
// into such a compiler.
//
// The Polly optimizer is made available by executing a static constructor that
// registers the individual Polly passes in the LLVM pass manager builder. The
// passes are registered such that the default behaviour of the compiler is not
// changed, but that the flag '-polly' provided at optimization level '-O3'
// enables additional polyhedral optimizations.
//===----------------------------------------------------------------------===//

#include "rv/passes.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "llvm/Transforms/Scalar.h"


using namespace llvm;

cl::OptionCategory rvCategory("RV Options",
                                 "Configure the rv-based loop vectorizer");


static cl::opt<bool>
    rvLoopVecEnabled("rv-loopvec", cl::desc("Enable RV's outer-loop vectorizer"),
                 cl::init(false), cl::ZeroOrMore, cl::cat(rvCategory));

static bool
shouldEnableRV() {
  return rvLoopVecEnabled;
}

static void
registerRVPasses(const llvm::PassManagerBuilder &Builder,
                                       llvm::legacy::PassManagerBase &PM) {
  if (!shouldEnableRV()) {
    return;
  }

  PM.add(rv::createLoopVectorizerPass());
  PM.add(createAggressiveDCEPass());
}


static llvm::RegisterStandardPasses RegisterPollyOptimizerScalarLate(
    llvm::PassManagerBuilder::EP_VectorizerStart,
    registerRVPasses);


