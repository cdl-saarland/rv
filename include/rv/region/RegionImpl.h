//===- rv/region/RegionImpl.h - abstract region class --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef RV_REGIONIMPL_H
#define RV_REGIONIMPL_H

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/CFG.h"

#include <stack>
#include <set>

namespace rv {

class RegionImpl {
public:
    virtual ~RegionImpl() {}

    virtual bool contains(const llvm::BasicBlock* BB) const = 0;
    virtual llvm::BasicBlock& getRegionEntry() const = 0;

    virtual std::string str() const = 0;

    virtual void for_blocks(std::function<bool(const llvm::BasicBlock& block)> userFunc) const {
      auto * func = getRegionEntry().getParent();
      for (const auto & BB : *func) {
        if (contains(&BB)) {
          bool carryOn = userFunc(BB);
          if (!carryOn) break;
        }
      }
    }

    virtual void getEndingBlocks(llvm::SmallPtrSet<llvm::BasicBlock*, 2>& endingBlocks) const
    {
        assert (endingBlocks.empty());

        std::stack<llvm::BasicBlock*> blockStack;
        blockStack.push(&this->getRegionEntry());

        std::set<llvm::BasicBlock*> visitedBlocks;

        while (!blockStack.empty())
        {
            // Pop the next block
            llvm::BasicBlock* block = blockStack.top(); blockStack.pop();

            // Make sure we haven't seen it already
            if (visitedBlocks.count(block)) continue;
            visitedBlocks.insert(block);

            // If a successor is outside the region, the region ends here.
            // Successors inside the region need to be processed recursively
            for (llvm::BasicBlock* successor : successors(block))
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

    virtual bool isVectorLoop() const = 0;
};

} // namespace rv

#endif // RV_REGIONIMPL_H
