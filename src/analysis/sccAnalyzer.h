//===- sccAnalyzer.h  -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//


#ifndef _SCCANALYZER_H
#define	_SCCANALYZER_H

#include "rv/rvInfo.h"

#include <deque>

#include <llvm/Pass.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/SmallPtrSet.h>
#include <llvm/ADT/DenseMap.h>

namespace llvm {
class Module;
class Function;
class BasicBlock;
}

using namespace llvm;


namespace {

struct SCCNode
{
    const BasicBlock* mBlock;
    unsigned mIndex;
    unsigned mMin;

    SCCNode(const BasicBlock* block) : mBlock(block), mIndex(0), mMin(0) {}
};

struct SCCEdge
{
    const BasicBlock* mSource;
    const BasicBlock* mTarget;
};

}


class SCCAnalyzer : public FunctionPass
{
public:
    static char ID __attribute__((unused)); // Pass identification, replacement for typeid.

    SCCAnalyzer();
	~SCCAnalyzer();

    virtual void releaseMemory   ();
	virtual void getAnalysisUsage(AnalysisUsage &AU) const;
    virtual bool doInitialization(Module& M);
    virtual bool doFinalization  (Module& M);
	virtual bool runOnFunction   (Function& F);
    virtual void print           (raw_ostream& O, const Module* M) const;

    struct SCC
    {
    private:
        typedef SmallVector<const BasicBlock*, 2> HeaderVecType;
        HeaderVecType mHeaders;

        typedef SmallPtrSet<const BasicBlock*, 4> BlockSetType;
        BlockSetType mBlocks;

        typedef SmallVector<SCC*, 2> ChildSCCVecType;
        ChildSCCVecType mChildren;

    public:
        SCC();
        SCC(const BasicBlock* header);

        unsigned size() const;
        bool empty() const;
        bool isHeader(const BasicBlock* block) const;
        bool contains(const BasicBlock* block) const;
        void insert(const BasicBlock* block);
        void insertHeader(const BasicBlock* block);
        unsigned getNumHeaders() const;
        const BasicBlock* getHeader(unsigned index) const;
        void addChild(SCC* child);
        unsigned getNumChildren() const;
        SCC* getChild(unsigned index) const;
        void print(unsigned nestingLevel=0) const;

        typedef BlockSetType::iterator       iterator;
        typedef BlockSetType::const_iterator const_iterator;

        iterator       begin();
        const_iterator begin() const;
        iterator       end();
        const_iterator end() const;
    };

    const SmallPtrSet<SCC*, 16>& getSCCs() const;

private:
    const rv::RVInfo*        mInfo;
    SmallPtrSet<SCC*, 16> mSCCSet;

    void createSCCs(const Function& f);

    void createSCCs(const SCC&                parentRegion,
                    SmallPtrSet<SCC*, 16>&    sccSet,
                    SmallVector<SCCEdge*, 4>& ignoreEdges);

    void findSCC(SCCNode*                               node,
                 unsigned&                              nextIndex,
                 std::deque<SCCNode*>&                  stack,
                 DenseMap<const BasicBlock*, SCCNode*>& nodeMap,
                 SmallPtrSet<SCC*, 16>&                 sccSet,
                 const SmallVector<SCCEdge*, 4>&        ignoreEdges);

    void addNextIgnoreEdge(const SCC&                scc,
                           const unsigned            headerIndex,
                           SmallVector<SCCEdge*, 4>& ignoreEdges);
};

//} // unnamed namespace


// Forward declaration of initializer and public interface.
namespace llvm {
void initializeSCCAnalyzerPass(PassRegistry&);
FunctionPass* createSCCAnalyzerPass();
}


#endif	/* _SCCANALYZER_H */
