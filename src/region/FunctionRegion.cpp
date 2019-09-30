//===- src/region/FunctionRegion.cpp - WFV region --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

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

