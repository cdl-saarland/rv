//===- ABAAnalysis.cpp -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @authors kloessner
//

#include "rv/analysis/ABAAnalysis.h"

#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/Dominators.h>

#include "rvConfig.h"

char ABAAnalysisWrapper::ID = 0;

ABAAnalysisWrapper::ABAAnalysisWrapper()
        : FunctionPass(ID)
{
}

void
ABAAnalysisWrapper::getAnalysisUsage(AnalysisUsage& Info) const
{
    Info.addRequired<DominatorTreeWrapperPass>();
    Info.addRequired<PostDominatorTree>();
    Info.addRequired<LoopInfoWrapperPass>();
    Info.addRequired<VectorizationInfoProxyPass>();

    Info.setPreservesAll();
}

bool
ABAAnalysisWrapper::runOnFunction(Function& F)
{
    auto& vecInfo = getAnalysis<VectorizationInfoProxyPass>().getInfo();
    auto& platInfo = getAnalysis<VectorizationInfoProxyPass>().getPlatformInfo();
    const LoopInfo& loopInfo             = getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
    const PostDominatorTree& postDomTree = getAnalysis<PostDominatorTree>();
    const DominatorTree& domTree         = getAnalysis<DominatorTreeWrapperPass>().getDomTree();

    rv::ABAAnalysis Analysis(platInfo,
                         vecInfo,
                         loopInfo,
                         postDomTree,
                         domTree);

    Analysis.analyze(F);

    return false;
}



namespace rv {

ABAAnalysis::ABAAnalysis(PlatformInfo & platInfo,
                         VectorizationInfo& vecInfo,
                         const LoopInfo& loopInfo,
                         const PostDominatorTree& postDomTree,
                         const DominatorTree& domTree)
        : mVecinfo(vecInfo),
          mFuncinfo(platInfo.getFunctionMappings()),
          mLoopInfo(loopInfo),
          mPostDomTree(postDomTree),
          mDomTree(domTree),
          mRegion(mVecinfo.getRegion())
{
}

void
ABAAnalysis::analyze(Function& F)
{
    markABABlocks(F);
    markABAONBlocks(F);
}

bool
ABAAnalysis::isInDivergentLoop(const BasicBlock& block)
{
    // Search through all the loop that contain the block
    for (Loop* loop = mLoopInfo.getLoopFor(&block);
         loop && loop->contains(&block);
         loop = loop->getParentLoop())
    {
        if (mVecinfo.isDivergentLoop(loop)) return true;
    }

    return false;
}

void
ABAAnalysis::markABABlocks(Function& F)
{
    if (mFuncinfo.count(&F) &&
        mFuncinfo[&F]->maskPos != -1)
    {
        IF_DEBUG {
          outs() << "  Function has mask argument, no blocks can be "
                  << "ALWAYS_BY_ALL!\n";
        }
        return;
    }

    // Start at entry of region if present, otherwise at the entry block of the function
    BasicBlock* potentialABA = mRegion ? &mRegion->getRegionEntry() : &F.getEntryBlock();

    // Entry is ABA
    mVecinfo.markAlwaysByAll(potentialABA);

    // Run down the post dominator tree, starting at the entry block.
    // Every thread will certainly pass through potentialABA, but
    // if potentialABA is in a divergent loop, it is not ABA (just "by all").
    while (potentialABA && (!mRegion || mRegion->contains(potentialABA)))
    {
        DomTreeNode* dtn = mPostDomTree.getNode(potentialABA);

        // If there is no post dominator, stop.
        if (!dtn || !dtn->getIDom()) break;

        // Set the post dominator as the current block.
        potentialABA = dtn->getIDom()->getBlock();

        if (!potentialABA) break;

        // If the block is not part of a divergent loop, mark it ABA.
        if (!isInDivergentLoop(*potentialABA))
            mVecinfo.markAlwaysByAll(potentialABA);
    }
}

void
ABAAnalysis::markABAONBlocks(Function& F)
{
    if (mFuncinfo.count(&F) && mFuncinfo[&F]->maskPos != -1)
    {
        IF_DEBUG {
           outs() << "  Function has mask argument, no blocks can be "
                  << "ALWAYS_BY_ALL!\n";
        }
        return;
    }

    // There might be dependencies between ABAON blocks, so we have to make
    // sure we do not mark blocks as ABA_FALSE too early.
    // Blocks that post dominate ABAON blocks have to be ABAON as well,
    // unless they are in divergent loops (just like ABA analysis).
    // Do a top-down DFS and only mark blocks of which we have seen all predecessors.
    SmallVector<BasicBlock*, 8> stack;
    SmallPtrSet<BasicBlock*, 16> visitedBlocks;

    BasicBlock* entry = mRegion ? &mRegion->getRegionEntry() : &F.getEntryBlock();
    stack.push_back(entry);
    bool isFollowedPostDom = false;

    while (!stack.empty())
    {
        BasicBlock* block = stack.pop_back_val();

        // If we have already seen this block, stop recursion.
        if (visitedBlocks.count(block)) continue;
#if 0
        // Mark user-specified uniform blocks as ABAON
        if (uniformBlocks.count(block)) {
            changed |= markValueAs(block, WFV::WFV_METADATA_ALWAYS_BY_ALL_OR_NONE);
            continue;
        }
#endif
        // If we have not yet seen all predecessors, stop recursion for this path,
        // unless we are in a loop header and the predecessor is the latch, or we
        // are following a post dominator.
        // In case of a loop header, mark it depending on the loop's DIVERGENT mark,
        // and continue as if we had seen all predecessors.
        // In case of a post dominator, mark it ABAON and continue as if we had seen
        // all predecessors.
        Loop* loop = mLoopInfo.getLoopFor(block);
        const bool isLoopHeader = loop && loop->getHeader() == block;

        auto VisitedPredicate = [&visitedBlocks](BasicBlock* predBB)
        {
            return visitedBlocks.count(predBB);
        };

        bool allSeen = all_of(predecessors(block), VisitedPredicate);

        if (!allSeen && !isLoopHeader && !isFollowedPostDom) continue;

        // Otherwise, mark current block as seen, and reset postdom flag.
        visitedBlocks.insert(block);
        isFollowedPostDom = false;

        // Push all successors onto the stack for DFS traversal.
        for (BasicBlock* succBB : successors(block))
        {
            // If the successor is already outside the region, we are not interested in it
            if (!mRegion || mRegion->contains(succBB))
            {
                stack.push_back(succBB);
            }
        }

        // If the block is marked already, skip it.
        if (mVecinfo.isAlwaysByAll(block) || mVecinfo.isNotAlwaysByAll(block)) continue;

        // If the block has a predecessor with OP_VARYING branch, it is ABA_FALSE.
        bool marked = false;
        bool hasABAFalsePred = false;

        // Predecessors of the region entry shall not be considered
        if (!mRegion || block != &mRegion->getRegionEntry())
        {
            for (BasicBlock* predBB : predecessors(block))
            {
                const TerminatorInst& terminator = *predBB->getTerminator();

                // If only a region is analyzed, the terminator may not have a shape, or it may,
                // depending on what assumptions the user wrote into mVecInfo.
                // If it does not, we assume must assume it is varying, thus the block is ABA_FALSE.
                if (!mVecinfo.hasKnownShape(terminator) ||
                    !mVecinfo.getVectorShape(terminator).isUniform())
                {
                    mVecinfo.markNotAlwaysByAll(block);
                    marked = true;
                    break;
                }

                if (mVecinfo.isNotAlwaysByAll(predBB))
                    hasABAFalsePred = true;
            }

            if (marked) continue;
        }

        // If we are in a divergent loop, the block is ABA_FALSE.
        if (loop && mVecinfo.isDivergentLoop(loop))
        {
            mVecinfo.markNotAlwaysByAll(block);
            continue;
        }

        // Otherwise, it is ABAON
        // - if it post dominates an ABAON block and is dominated by it, or
        // - if it has no ABA_FALSE predecessor,
        // and ABA_FALSE otherwise.
        if (!hasABAFalsePred)
        {
            mVecinfo.markAlwaysByAllOrNone(block);
        }
        else
        {
            if (isABAONSESEExit(*block))
                mVecinfo.markAlwaysByAllOrNone(block);
            else
                mVecinfo.markNotAlwaysByAll(block);

            continue;
        }

        // We just marked the block ABAON, so if there is a block that post-dominates the current
        // block and is dominated by the current block, we now mark it ABAON as well unless it is
        // in a divergent loop.
        DomTreeNode* dtn = mPostDomTree.getNode(block);
        if (!dtn || !dtn->getIDom()) continue;

        BasicBlock* postDominatedBB = dtn->getIDom()->getBlock();

        Loop* pdLoop = mLoopInfo.getLoopFor(postDominatedBB);
        if (pdLoop && mVecinfo.isDivergentLoop(pdLoop)) continue;

        if (mVecinfo.isAlwaysByAll(postDominatedBB) || mVecinfo.isNotAlwaysByAll(postDominatedBB))
        {
            continue;
        }

        dtn = mDomTree.getNode(postDominatedBB);
        if (!dtn || !dtn->getIDom()) continue;

        if (dtn->getIDom()->getBlock() != block) continue;

        IF_DEBUG {
          outs() <<
                  "  marking post dominator '" <<
                  postDominatedBB->getName() <<
                  "' as ABAON...\n";
        }

        mVecinfo.markAlwaysByAllOrNone(postDominatedBB);

        stack.push_back(postDominatedBB);
        isFollowedPostDom = true;
    }
}

bool ABAAnalysis::isABAONSESEExit(BasicBlock& block)
{
    DomTreeNode* dtn = mDomTree.getNode(&block);
    if (dtn && dtn->getIDom())
    {
        BasicBlock* dominatorBB = dtn->getIDom()->getBlock();
        if (mVecinfo.isAlwaysByAllOrNone(dominatorBB))
        {
            dtn = mPostDomTree.getNode(dominatorBB);
            if (dtn && dtn->getIDom())
            {
                BasicBlock* postDominatorBB = dtn->getIDom()->getBlock();

                IF_DEBUG {
                  if (postDominatorBB == &block)
                             outs() <<
                                    "  block '" <<
                                    block.getName() <<
                                    "' is exit of ABAON SESE region.\n";
                }

                return postDominatorBB == &block;
            }
        }
    }

    return false;
}


} // namespace rv



FunctionPass*
createABAAnalysisPass() {
    return new ABAAnalysisWrapper();
}
