//===- src/region/LoopRegion.cpp - outer-loop region --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include <llvm/IR/Dominators.h>
#include "rv/region/LoopRegion.h"

using namespace llvm;

namespace rv {

LoopRegion::LoopRegion(Loop & _loop)
: loop(_loop)
{}

LoopRegion::~LoopRegion() {}

bool
LoopRegion::contains(const BasicBlock* BB) const
{
    return loop.contains(BB);
}

BasicBlock&
LoopRegion::getRegionEntry() const
{
    return *loop.getHeader();
}

void
LoopRegion::getEndingBlocks(llvm::SmallPtrSet<BasicBlock*, 2>& endingBlocks) const
{
    SmallVector<BasicBlock*, 2> endingBlocksVector;
    loop.getExitBlocks(endingBlocksVector);

    for (auto& endingBB : endingBlocksVector)
    {
        endingBlocks.insert(endingBB);
    }
}

std::string
LoopRegion::str() const {
  auto loopHeaderName = loop.getHeader()->getName();
  return ("LoopRegion (header " + loopHeaderName + ")").str();
}

} // namespace rv

