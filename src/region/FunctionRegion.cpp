//===- FunctionRegion.cpp -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @author kloessner, simon
//

#include <llvm/IR/Dominators.h>
#include "rv/region/FunctionRegion.h"

using namespace llvm;

namespace rv {

void
FunctionRegion::getEndingBlocks(llvm::SmallPtrSet<BasicBlock*, 2>& endingBlocks) const
{
  for (auto & BB : F) {
    if (BB.getTerminator()->getNumSuccessors() == 0) endingBlocks.insert(&BB);
  }
}

std::string
FunctionRegion::str() const {
  return ("FunctionRegion (" + F.getName() + ")").str();
}

} // namespace rv

