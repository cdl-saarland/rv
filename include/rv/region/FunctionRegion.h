//===- FunctionRegion.h -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//

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

  bool contains(const llvm::BasicBlock* BB) const override { return true; }
  llvm::BasicBlock& getRegionEntry() const override { return F.getEntryBlock(); }
  void getEndingBlocks(llvm::SmallPtrSet<llvm::BasicBlock*, 2>& endingBlocks) const override;
  std::string str() const override;
  bool isVectorLoop() const override { return false; }
};

} // namespace rv

#endif // RV_FUNCTIONREGION_H_
