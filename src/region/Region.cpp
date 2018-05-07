//===- Region.cpp -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @author kloessner

#include "rv/region/Region.h"

#include "rv/region/RegionImpl.h"
#include <llvm/IR/Function.h>
#include <llvm/ADT/PostOrderIterator.h>

using namespace llvm;

namespace rv {

class RegionImpl;

Region::Region(RegionImpl& Impl) : mImpl(Impl)
{}

bool
Region::contains(const BasicBlock* BB) const {
  if (extraBlocks.count(BB)) return true;
  else return mImpl.contains(BB);
}

BasicBlock&
Region::getRegionEntry() const
{
    return mImpl.getRegionEntry();
}

std::string
Region::str() const { return mImpl.str(); }

void
Region::getEndingBlocks(llvm::SmallPtrSet<BasicBlock*, 2>& endingBlocks) const
{
    mImpl.getEndingBlocks(endingBlocks);
}

bool
Region::isVectorLoop() const { return mImpl.isVectorLoop(); }

void
Region::for_blocks(std::function<bool(const BasicBlock& block)> userFunc) const {
  mImpl.for_blocks(userFunc);
  for (auto * block : extraBlocks) userFunc(*block);
}

void
Region::for_blocks_rpo(std::function<bool(const BasicBlock& block)> userFunc) const {
  const Function & F = *getRegionEntry().getParent();
  ReversePostOrderTraversal<const Function*> RPOT(&F);

  for (auto * BB : RPOT) {
    if (mImpl.contains(BB) || extraBlocks.count(BB)) userFunc(*BB);
  }
}

} // namespace rv
