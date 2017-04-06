//===- Region.h -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//

#ifndef RV_LOOPREGION_H_
#define RV_LOOPREGION_H_

#include <llvm/IR/BasicBlock.h>
#include <llvm/Analysis/LoopInfo.h>

#include "rv/region/RegionImpl.h"

using llvm::Loop;
using llvm::SmallVector;

namespace rv {

// This implementation realizes regions
// with a single point of entry and exit
// All block dominated by the entry and postdominated
// by the exit are contained in this region
// The region represented this way has control flow
// possibly diverge after the entry but reconverge
// at the exit
class LoopRegion : public RegionImpl
{
private:
    Loop& loop;

public:
    LoopRegion(Loop&);
    ~LoopRegion();

    bool contains(const BasicBlock* BB) const override;
    BasicBlock& getRegionEntry() const override;
    void getEndingBlocks(SmallPtrSet<BasicBlock*, 2>& endingBlocks) const override;
    std::string str() const override;
};

} // namespace rv

#endif // RV_LOOPREGION_H_
