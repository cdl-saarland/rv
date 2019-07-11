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

#include "rv/transform/lowerRVIntrinsics.h"
#include "rv/LinkAllPasses.h"

#include "rv/rv.h"
#include "report.h"
#include <map>

using namespace rv;
using namespace llvm;

// typedef DomTreeNodeBase<BasicBlock*> DomTreeNode;
bool LowerRVIntrinsics::runOnFunction(Function &F) {
  return rv::lowerIntrinsics(F);
}

void LowerRVIntrinsics::getAnalysisUsage(AnalysisUsage &AU) const { }

char LowerRVIntrinsics::ID = 0;

FunctionPass *rv::createLowerRVIntrinsicsPass() { return new LowerRVIntrinsics(); }

INITIALIZE_PASS_BEGIN(LowerRVIntrinsics, "rv-lower-intrinsics",
                      "RV - Lower intrinsics", false, false)
INITIALIZE_PASS_END(LowerRVIntrinsics, "rv-lower-intrinsics", "RV - Lower Intrinsics",
                    false, false)
