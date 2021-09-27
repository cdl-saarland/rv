//===- lowerRVIntrinsics.cpp - lower RV Builtins  ----------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//
//===----------------------------------------------------------------------===//

#include "rv/passes/lowerRVIntrinsics.h"
#include "rv/legacy/LinkAllPasses.h"

#include "report.h"
#include "rv/rv.h"
#include <map>

using namespace rv;
using namespace llvm;

///// Old PM Pass /////

LowerRVIntrinsicsLegacyPass::LowerRVIntrinsicsLegacyPass()
    : llvm::FunctionPass(ID) {}

bool LowerRVIntrinsicsLegacyPass::runOnFunction(llvm::Function &F) {
  return rv::lowerIntrinsics(F);
}

/// Register all analyses and transformation required.
void LowerRVIntrinsicsLegacyPass::getAnalysisUsage(AnalysisUsage &AU) const {}

char LowerRVIntrinsicsLegacyPass::ID = 0;

FunctionPass *rv::createLowerRVIntrinsicsLegacyPass() {
  return new LowerRVIntrinsicsLegacyPass();
}

INITIALIZE_PASS_BEGIN(LowerRVIntrinsicsLegacyPass, "rv-lower-intrinsics",
                      "RV - Lower intrinsics", false, false)
INITIALIZE_PASS_END(LowerRVIntrinsicsLegacyPass, "rv-lower-intrinsics",
                    "RV - Lower Intrinsics", false, false)

///// New PM Pass /////

llvm::PreservedAnalyses
LowerRVIntrinsicsWrapperPass::run(llvm::Function &F,
                                  llvm::FunctionAnalysisManager &FAM) {
  if (rv::lowerIntrinsics(F))
    return llvm::PreservedAnalyses::none();
  else
    return llvm::PreservedAnalyses::all();
}
