//===- rv/region/FunctionRegion.h - WFV region --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//


#ifndef RV_FUNCTIONREGION_H_
#define RV_FUNCTIONREGION_H_

#include <llvm/IR/Function.h>
#include <llvm/IR/BasicBlock.h>

#include "rv/region/RegionImpl.h"

namespace rv {

// this region object captures the entire CFG of a function
class FunctionRegion final : public RegionImpl
{
private:
  llvm::Function & F;

public:
  FunctionRegion(llvm::Function& _F) : F(_F) {};
  ~FunctionRegion() {}

  bool contains(const llvm::BasicBlock* BB) const override { return BB->getParent() == &F; }
  llvm::BasicBlock& getRegionEntry() const override { return F.getEntryBlock(); }
  void getEndingBlocks(llvm::SmallPtrSet<llvm::BasicBlock*, 2>& endingBlocks) const override;
  std::string str() const override;
  bool isVectorLoop() const override { return false; }
};

} // namespace rv

#endif // RV_FUNCTIONREGION_H_
