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

using namespace llvm;

namespace rv {

class Region {
    RegionImpl& mImpl;
    SmallPtrSet<const BasicBlock*,32> extraBlocks;

public:
    Region(RegionImpl& mImpl);
    bool contains(const BasicBlock* BB) const;

    BasicBlock& getRegionEntry() const;
    void getEndingBlocks(SmallPtrSet<BasicBlock*, 2>& endingBlocks) const;
    void print(llvm::raw_ostream & ) const {}
    std::string str() const;

    void add(const llvm::BasicBlock & extra) {
      extraBlocks.insert(&extra);
    }
};

}

#endif //RV_REGION_H
