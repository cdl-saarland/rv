//===- Region.h -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//

#ifndef RV_REGION_H
#define RV_REGION_H

#include "RegionImpl.h"
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/SmallPtrSet.h>

namespace llvm {
class BasicBlock;
class raw_ostream;
}

namespace rv {

class Region {
    RegionImpl& mImpl;
    llvm::SmallPtrSet<const llvm::BasicBlock*,32> extraBlocks;

public:
    Region(RegionImpl& mImpl);
    bool contains(const llvm::BasicBlock* BB) const;

    llvm::BasicBlock& getRegionEntry() const;
    void getEndingBlocks(llvm::SmallPtrSet<llvm::BasicBlock*, 2>& endingBlocks) const;
    void print(llvm::raw_ostream & ) const {}
    std::string str() const;

    void add(const llvm::BasicBlock & extra) {
      extraBlocks.insert(&extra);
    }

    // iteratively apply @userFunc to all blocks in the region
    // stop if @userFunc returns false or all blocks have been prosessed, otw carry on
    void for_blocks(std::function<bool(const llvm::BasicBlock& block)> userFunc) const;
};

}

#endif //RV_REGION_H
