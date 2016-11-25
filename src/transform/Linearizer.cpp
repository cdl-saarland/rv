//===- Linearizer.cpp ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @authors simon
//

#include "rv/transform/Linearizer.h"

#include "rv/Region/Region.h"
#include "rv/vectorizationInfo.h"

#include <llvm/IR/Instructions.h>
#include <llvm/Analysis/LoopInfo.h>

#if 1
#define IF_DEBUG_LIN if (defined(_DEBUG))
#else
#define IF_DEBUG_LIN if (false)
#endif

namespace rv {


void
Linearizer::addToBlockIndex(BasicBlock & block) {
  blockIndex[&block] = blocks.size();
  blocks.push_back(&block);
}

using namespace rv;
using namespace llvm;

void
Linearizer::buildBlockIndex(DomTreeNode & domNode) {
  auto * block = domNode.getBlock();
  addToBlockIndex(*block);
  for (auto * childNode : domNode) {
    if (region && ! region->contains(childNode->getBlock())) continue;
    buildBlockIndex(*childNode);
  }
}

void
Linearizer::buildBlockIndex() {
  BlockIndex blockIndex;
  auto & entry = region ? region->getRegionEntry() : vecInfo.getScalarFunction().getEntryBlock();
  buildBlockIndex(*dt.getNode(&entry));

}


bool
Linearizer::needsFolding(TerminatorInst & termInst) {
  assert(!isa<SwitchInst>(termInst) && "switches unsupported at the moment");

  if (isa<ReturnInst>(termInst) || isa<UnreachableInst>(termInst)) return false;

  auto & branch = cast<BranchInst>(termInst);
  if (!branch.isConditional()) return false;
  return !vecInfo.getVectorShape(*branch.getCondition()).isUniform();
}

void
Linearizer::run() {
//
  buildBlockIndex();

}


}
