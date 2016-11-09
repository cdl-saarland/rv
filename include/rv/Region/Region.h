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

namespace llvm {
class BasicBlock;
class raw_ostream;
}

namespace rv {

using llvm::BasicBlock;

class Region {
    RegionImpl& mImpl;
public:
    Region(RegionImpl& mImpl);
    bool contains(const BasicBlock* BB) const;
    BasicBlock& getRegionEntry() const;
    void print(llvm::raw_ostream & out) const {}
};

}

#endif //RV_REGION_H
