//===- rv/transform/singleReturnTrans.cpp - merge all return blocks --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#include "rv/transform/singleReturnTrans.h"

#include <llvm/IR/Instructions.h>
#include <llvm/IR/IRBuilder.h>
#include <rv/region/Region.h>

using namespace llvm;

namespace rv {

bool
SingleReturnTrans::run(Region & region) {
  if (region.getFunction().getReturnType()->isVoidTy()) return false;
  auto & retTy = *region.getFunction().getReturnType();

  // collect all returning blocks in the region
  std::vector<std::pair<BasicBlock*, Value*>> returnedValues;
  region.for_blocks([&](const BasicBlock & block) {
      auto * retInst = dyn_cast<ReturnInst>(block.getTerminator());
      if (!retInst) return true;
      returnedValues.emplace_back(const_cast<BasicBlock*>(&block), retInst->getReturnValue());
      return true;
  });

  if (returnedValues.size() == 1) return false;

  auto * singleRetBlock = BasicBlock::Create(region.getFunction().getContext(), "joined_return", &region.getFunction());
  IRBuilder<> builder(singleRetBlock);
  auto & phi = *builder.CreatePHI(&retTy, returnedValues.size(), "ret_vals");
  builder.CreateRet(&phi);

  for (auto & retPair : returnedValues) {
    phi.addIncoming(retPair.second, retPair.first);

    auto & oldTerm = *retPair.first->getTerminator();
    BranchInst::Create(singleRetBlock, &oldTerm);
    oldTerm.eraseFromParent();
  }
  return true;
}

} // namespace rv
