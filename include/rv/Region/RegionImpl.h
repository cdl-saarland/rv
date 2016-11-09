//
// Created by thorsten on 18.08.16.
//

#ifndef RV_REGIONIMPL_H
#define RV_REGIONIMPL_H

namespace llvm {
class BasicBlock;
}

using llvm::BasicBlock;

namespace rv {

class RegionImpl {
public:
    virtual ~RegionImpl() {}

    virtual bool contains(const BasicBlock* BB) const = 0;
    virtual BasicBlock& getRegionEntry() const = 0;
};

}

#endif // RV_REGIONIMPL_H
