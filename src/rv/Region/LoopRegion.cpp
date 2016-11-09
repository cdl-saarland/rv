//===- LoopRegion.cpp -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @author kloessner, simon
//

#include <llvm/IR/Dominators.h>
#include "rv/Region/LoopRegion.h"

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


}
