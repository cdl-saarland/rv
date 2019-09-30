//===- rv/region/LoopRegion.h - outer-loop region --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef RV_LOOPREGION_H_
#define RV_LOOPREGION_H_

#include <llvm/IR/BasicBlock.h>
#include <llvm/Analysis/LoopInfo.h>

#include "rv/region/RegionImpl.h"

namespace rv {

// This implementation realizes regions
// with a single point of entry and exit
// All block dominated by the entry and postdominated
// by the exit are contained in this region
// The region represented this way has control flow
// possibly diverge after the entry but reconverge
// at the exit
class LoopRegion final : public RegionImpl
{
private:
    llvm::Loop& loop;

public:
    LoopRegion(llvm::Loop&);
    ~LoopRegion();

    bool contains(const llvm::BasicBlock* BB) const override;
    llvm::BasicBlock& getRegionEntry() const override;
    void getEndingBlocks(llvm::SmallPtrSet<llvm::BasicBlock*, 2>& endingBlocks) const override;
    std::string str() const override;
    bool isVectorLoop() const override { return true; }
};

} // namespace rv

#endif // RV_LOOPREGION_H_
