/*
 * llvmDomination.h
 *
 *  Created on: 06.03.2010
 */

#ifndef LLVMDOMINATION_HPP_
#define LLVMDOMINATION_HPP_

#include <llvm/IR/Dominators.h>
#include "CommonTypes.h"

namespace rv {

typedef std::vector<llvm::DomTreeNode *> DomTreeNodeVector;
typedef std::set<llvm::BasicBlock*> BlockSet;

/*
 * computes the intersection of all Dominance Frontiers of all elements in
 * @nodes
 */
/*template<typename IT>
BlockSet computeCommonDomFront(llvm::DominanceFrontiert & domFront, IT begin, IT
end)
{
        BlockSet result;
        bool wasMerged = false;

        for(IT itNode = begin; itNode != end; ++itNode)
        {
                llvm::BasicBlock * node = *itNode;
                BlockSet nodeDomFront = domFront[node];

                if (wasMerged) {
                        BlockSet tmp;
                        std::set_intersection(result.begin(), result.end(),
nodeDomFront.begin(), nodeDomFront.end(), tmp.begin());
                        result.swap(tmp);
                } else {
                        result.swap(nodeDomFront);
                }
        }

        return result;
} */

/*
 * collect all blocks that are reachable and post-dominated from @entryBlock
 */
BlockSet getSelfDominatedBlocks(llvm::BasicBlock *entryBlock,
                                llvm::PostDominatorTree &postDomTree);

bool dominatesAll(llvm::DominatorTree &domTree, llvm::DomTreeNode *node,
                  const BlockSet &blocks);

llvm::DomTreeNode *findImmediateDominator(llvm::DominatorTree &domTree,
                                          const BlockSet &blocks);

BlockSet computeDominatedRegion(llvm::DominatorTree &domTree,
                                llvm::BasicBlock *header, BlockSet exits);
}

#endif /* LLVMDOMINATION_HPP_ */
