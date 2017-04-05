//
// Created by thorsten on 18.08.16.
//

#ifndef RV_REGIONIMPL_H
#define RV_REGIONIMPL_H

#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/CFG.h"

#include <stack>
#include <set>

using namespace llvm;

namespace rv {

class RegionImpl {
public:
    virtual ~RegionImpl() {}

    virtual bool contains(const BasicBlock* BB) const = 0;
    virtual BasicBlock& getRegionEntry() const = 0;

    virtual std::string str() const = 0;

    virtual void getEndingBlocks(SmallPtrSet<BasicBlock*, 2>& endingBlocks) const
    {
        assert (endingBlocks.empty());

        std::stack<BasicBlock*> blockStack;
        blockStack.push(&this->getRegionEntry());

        std::set<BasicBlock*> visitedBlocks;

        while (!blockStack.empty())
        {
            // Pop the next block
            BasicBlock* block = blockStack.top(); blockStack.pop();

            // Make sure we haven't seen it already
            if (visitedBlocks.count(block)) continue;
            visitedBlocks.insert(block);

            // If a successor is outside the region, the region ends here.
            // Successors inside the region need to be processed recursively
            for (BasicBlock* successor : successors(block))
            {
                if (this->contains(successor))
                {
                    blockStack.push(successor);
                }
                else
                {
                    endingBlocks.insert(successor);
                }
            }
        }
    }
};

} // namespace rv

#endif // RV_REGIONIMPL_H
