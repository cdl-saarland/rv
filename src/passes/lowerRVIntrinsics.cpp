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

#include "report.h"
#include "rv/rv.h"
#include <map>

using namespace rv;
using namespace llvm;

///// New PM Pass /////

llvm::PreservedAnalyses
LowerRVIntrinsicsWrapperPass::run(llvm::Function &F,
                                  llvm::FunctionAnalysisManager &FAM) {
  if (rv::lowerIntrinsics(F))
    return llvm::PreservedAnalyses::none();
  else
    return llvm::PreservedAnalyses::all();
}
