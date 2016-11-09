//===- sccAnalyzer.cpp  -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @author karrenberg
//

#include "sccAnalyzer.h"
#include "rvConfig.h"
#include "utils/rvTools.h"

#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/Instructions.h>

#ifdef RV_DO_NOT_ALLOW_CRITICAL_EDGES
#   include <llvm/Transforms/Utils/BasicBlockUtils.h> // isCriticalEdge()
#endif

#include <stdexcept>
#include <rv/rvInfoProxyPass.h>

using namespace llvm;


char SCCAnalyzer::ID = 0;
INITIALIZE_PASS_BEGIN(SCCAnalyzer, "sccAnalyzer", "SCCAnalyzer", false, true)
INITIALIZE_PASS_DEPENDENCY(RVInfoProxyPass)
INITIALIZE_PASS_END(SCCAnalyzer, "sccAnalyzer", "SCCAnalyzer", false, true)

// Public interface to the SCCAnalyzer pass
FunctionPass*
llvm::createSCCAnalyzerPass()
{
	return new SCCAnalyzer();
}


SCCAnalyzer::SCCAnalyzer()
    : FunctionPass(ID)
{
    initializeSCCAnalyzerPass(*PassRegistry::getPassRegistry());
}

SCCAnalyzer::~SCCAnalyzer()
{
    mInfo = nullptr; // Deleted by PassManager.
}

void
SCCAnalyzer::releaseMemory()
{
}

void
SCCAnalyzer::getAnalysisUsage(AnalysisUsage &AU) const
{
    AU.addRequired<RVInfoProxyPass>();

    AU.setPreservesAll();
}

bool
SCCAnalyzer::doInitialization(Module& M)
{
    // The return value presumably signals whether the module was changed or not.
    // There is no documentation on this in LLVM.
    return false;
}

bool
SCCAnalyzer::doFinalization(Module& M)
{
    // The return value presumably signals whether the module was changed or not.
    // There is no documentation on this in LLVM.
    return false;
}

bool
SCCAnalyzer::runOnFunction(Function& F)
{
    mInfo = &getAnalysis<RVInfoProxyPass>().getInfo();

    // If an error occurred in one of the previous phases, abort.

    try {
        createSCCs(F);
    }
    catch (std::logic_error& error)
    {
        errs() << "\nException occurred during SCCAnalyzer: "
                << error.what() << "\n";
    }
    catch (...)
    {
        errs() << "\nINTERNAL ERROR: Unexpected exception occurred during "
                << "SCCAnalyzer!\n";
    }

    // Function was not changed.
    return false;
}

void
SCCAnalyzer::print(raw_ostream& O, const Module* M) const
{
}

const SmallPtrSet<SCCAnalyzer::SCC*, 16>&
SCCAnalyzer::getSCCs() const
{
    return mSCCSet;
}

void
SCCAnalyzer::createSCCs(const Function& f)
{
    SmallVector<SCCEdge*, 4> ignoreEdges;

    SCC topLevelRegion(&f.getEntryBlock());
    for (auto &BB : f) topLevelRegion.insert(&BB);

    createSCCs(topLevelRegion, mSCCSet, ignoreEdges);

    DEBUG_RV( outs() << "SCCs:\n"; );
    DEBUG_RV( for (const auto &scc : mSCCSet) { outs() << "*"; scc->print(1); } );
}

void
SCCAnalyzer::createSCCs(const SCC&                parentRegion,
                        SmallPtrSet<SCC*, 16>&    sccSet,
                        SmallVector<SCCEdge*, 4>& ignoreEdges)
{
    unsigned                              index = 0;
    std::deque<SCCNode*>                  stack;
    DenseMap<const BasicBlock*, SCCNode*> nodeMap;

    // Start with headers.
    for (unsigned i=0, e=parentRegion.getNumHeaders(); i<e; ++i)
    {
        const BasicBlock* header = parentRegion.getHeader(i);
        if (nodeMap.count(header)) continue;

        findSCC(new SCCNode(header), index, stack, nodeMap, sccSet, ignoreEdges);
    }

    // Now look if there are remaining blocks to be traversed.
    for (const auto &BB : parentRegion)
    {
        if (nodeMap.count(BB)) continue;

        findSCC(new SCCNode(BB), index, stack, nodeMap, sccSet, ignoreEdges);
    }
    assert (stack.empty());

    // Remove SCCs with only one block.
    SmallVector<SCC*, 16> eraseVec;
    for (auto scc : sccSet)
    {
        assert (!scc->empty());
        if (scc->size() == 1) eraseVec.push_back(scc);
    }

    for (auto scc : eraseVec)
    {
        sccSet.erase(scc);
        delete scc;
    }

    // Cleanup.
    for (auto &it : nodeMap)
    {
        delete it.second;
    }

    DEBUG_RV( outs() << "\nSCCs at level " << ignoreEdges.size() << ":\n"; );
    DEBUG_RV(
        for (auto scc : sccSet)
        {
            scc->print();
        }
    );
    DEBUG_RV( outs() << "\n"; );

    // Recurse: Create nested SCCs.
    SmallPtrSet<SCC*, 16> nestedSCCSet;
    for (auto scc : sccSet)
    {
        // One nested SCC per header.
        for (unsigned i=0, e=scc->getNumHeaders(); i<e; ++i)
        {
            // We have to ignore the back edge to this header to prevent
            // this same SCC to be found again and again.
            addNextIgnoreEdge(*scc, i, ignoreEdges);

            createSCCs(*scc, nestedSCCSet, ignoreEdges);

            for (auto childSCC : nestedSCCSet)
            {
                scc->addChild(childSCC);
            }
            nestedSCCSet.clear();

            // Remove ignoreEdge of this nested SCC from stack again.
            SCCEdge* ignoreEdge = ignoreEdges.pop_back_val();
            delete ignoreEdge;
        }
    }
}

void
SCCAnalyzer::findSCC(SCCNode*                               node,
                     unsigned&                              nextIndex,
                     std::deque<SCCNode*>&                  stack,
                     DenseMap<const BasicBlock*, SCCNode*>& nodeMap,
                     SmallPtrSet<SCC*, 16>&                 sccSet,
                     const SmallVector<SCCEdge*, 4>&        ignoreEdges)
{
    assert (node);
    assert (node->mIndex == 0); // "uninitialized".
    DEBUG_RV( outs() << "findSCC(" << node->mBlock->getName() << "(" << nextIndex << "))\n"; );

    // Set the depth index for node to the smallest unused index.
    node->mIndex = nextIndex;
    node->mMin   = nextIndex++;

    stack.push_back(node);
    nodeMap[node->mBlock] = node;

    const BasicBlock*     block      = node->mBlock;
    const TerminatorInst* terminator = block->getTerminator();
    for (unsigned i=0, e=terminator->getNumSuccessors(); i<e; ++i)
    {
        const BasicBlock* succBB = terminator->getSuccessor(i);

        bool isIgnored = false;
        for (const auto &sccEdge : ignoreEdges)
        {
            if (sccEdge->mSource != block) continue;
            if (sccEdge->mTarget != succBB) continue;
            isIgnored = true;
            break;
        }
        if (isIgnored) continue;

        SCCNode* succNode = nodeMap[succBB];

        if (!succNode)
        {
            // Successor has not yet been visited -> recurse.
            succNode = new SCCNode(succBB);
            findSCC(succNode, nextIndex, stack, nodeMap, sccSet, ignoreEdges);
            node->mMin = std::min(node->mMin, succNode->mMin);
            continue;
        }

        // Search the stack (iteration is in order).
        bool succNodeOnStack = false;
        for (auto &snode : stack)
        {
            if (snode == succNode)
            {
                succNodeOnStack = true;
                break;
            }
        }

        if (!succNodeOnStack)
        {
            DEBUG_RV( outs() << "  successor " << succBB->getName() << " is not in stack.\n"; );
            continue;
        }

        DEBUG_RV( outs() << "  successor " << succBB->getName(); );
        DEBUG_RV( outs() << " is in stack -> in current SCC.\n"; );

        // Successor is in stack and hence in the current SCC.
        node->mMin = std::min(node->mMin, succNode->mIndex);
    }

    // If 'node' is a root node, pop the stack and generate an SCC.
    if (node->mMin == node->mIndex)
    {
        SCC* scc = new SCC();

        SCCNode* snode = nullptr;
        do
        {
            snode = stack.back();
            stack.pop_back();
            scc->insert(snode->mBlock);
        }
        while (snode != node);

        // Find other headers.
        for (const auto &BB : *scc)
        {
            for (const_pred_iterator P=pred_begin(BB), PE=pred_end(BB); P!=PE; ++P)
            {
                const BasicBlock* predBB = *P;

                // Ignore entries from within the SCC.
                if (scc->contains(predBB)) continue;

                // Ignore "entries" from unreachable code.
                if (&BB->getParent()->getEntryBlock() != predBB &&
                    pred_begin(predBB) == pred_end(predBB)) continue;

                // This is a header.
                scc->insertHeader(BB);
                break;
            }
        }
        assert (scc->size() == 1 || scc->getNumHeaders() > 0);

        sccSet.insert(scc);
        DEBUG_RV( outs() << "  new SCC: "; scc->print(); );
    }
}

void
SCCAnalyzer::addNextIgnoreEdge(const SCC&                scc,
                               const unsigned            headerIndex,
                               SmallVector<SCCEdge*, 4>& ignoreEdges)
{
    const BasicBlock* header = scc.getHeader(headerIndex);

    const BasicBlock* entryBB = nullptr;
    for (const_pred_iterator P=pred_begin(header), PE=pred_end(header); P!=PE; ++P)
    {
        const BasicBlock* predBB = *P;
        if (!scc.contains(predBB)) continue;
        entryBB = predBB;
        break;
    }
    assert (entryBB);

    SCCEdge* newEdge = new SCCEdge();
    newEdge->mSource = entryBB;
    newEdge->mTarget = header;
    ignoreEdges.push_back(newEdge);
    DEBUG_RV( outs() << "ignored edge for next iteration: '" << entryBB->getName(); );
    DEBUG_RV( outs() << "' -> '" << header->getName() << "\n"; );
}


SCCAnalyzer::SCC::SCC()
{}

SCCAnalyzer::SCC::SCC(const BasicBlock* header)
{
    assert (header);
    insertHeader(header);
}

unsigned
SCCAnalyzer::SCC::size() const
{
    return mBlocks.size();
}

bool
SCCAnalyzer::SCC::empty() const
{
    return mBlocks.empty();
}

bool
SCCAnalyzer::SCC::isHeader(const BasicBlock* block) const
{
    assert (block);
    if (!contains(block)) return false;
    for (unsigned i=0, e=getNumHeaders(); i<e; ++i)
    {
        if (block == getHeader(i)) return true;
    }
    return false;
}

bool
SCCAnalyzer::SCC::contains(const BasicBlock* block) const
{
    return mBlocks.count(block);
}

void
SCCAnalyzer::SCC::insert(const BasicBlock* block)
{
    assert (block);
    mBlocks.insert(block);
}

void
SCCAnalyzer::SCC::insertHeader(const BasicBlock* block)
{
    assert (block);
    mBlocks.insert(block);
    mHeaders.push_back(block);
}

unsigned
SCCAnalyzer::SCC::getNumHeaders() const
{
    return mHeaders.size();
}

const BasicBlock*
SCCAnalyzer::SCC::getHeader(unsigned index) const
{
    assert (index < mHeaders.size());
    return mHeaders[index];
}

void
SCCAnalyzer::SCC::addChild(SCCAnalyzer::SCC* child)
{
    mChildren.push_back(child);
}

unsigned
SCCAnalyzer::SCC::getNumChildren() const
{
    return mChildren.size();
}

SCCAnalyzer::SCC*
SCCAnalyzer::SCC::getChild(unsigned index) const
{
    assert (index < mChildren.size());
    return mChildren[index];
}

void
SCCAnalyzer::SCC::print(unsigned nestingLevel) const
{
    for (unsigned l=0, le=nestingLevel; l<le; ++l) outs() << "  ";

    for (unsigned i=0, e=getNumHeaders(); i<e; ++i)
    {
        outs() << " *" << getHeader(i)->getName();
    }
    for (const auto &BB : mBlocks)
    {
        if (isHeader(BB)) continue;
        outs() << " " << BB->getName();
    }
    outs() << "\n";

    for (unsigned i=0, e=getNumChildren(); i<e; ++i)
    {
        getChild(i)->print(++nestingLevel);
    }
}

SCCAnalyzer::SCC::iterator
SCCAnalyzer::SCC::begin()
{
    return mBlocks.begin();
}

SCCAnalyzer::SCC::const_iterator
SCCAnalyzer::SCC::begin() const
{
    return mBlocks.begin();
}

SCCAnalyzer::SCC::iterator
SCCAnalyzer::SCC::end()
{
    return mBlocks.end();
}

SCCAnalyzer::SCC::const_iterator
SCCAnalyzer::SCC::end() const
{
    return mBlocks.end();
}
