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

} // namespace rv
