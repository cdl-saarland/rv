//===- ABAAnalysis.h -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//


#ifndef RV_ABAANALYSIS_H
#define RV_ABAANALYSIS_H

#include <llvm/Pass.h>

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Use.h"
#include "llvm/IR/Value.h"
#include "llvm/Pass.h"
#include "llvm/Support/GenericDomTree.h"
#include "llvm/Support/raw_ostream.h"

#include "rv/analysis/DFG.h"
#include "rv/vectorizationInfo.h"
#include "rv/VectorizationInfoProxyPass.h"
#include "rv/region/Region.h"
#include "rv/PlatformInfo.h"

namespace llvm {
  class LoopInfo;
  struct PostDominatorTree;
  class DominatorTree;
}

class ABAAnalysisWrapper : public llvm::FunctionPass {
    static char ID;

public:
    ABAAnalysisWrapper();

    void getAnalysisUsage(AnalysisUsage& Info) const override;
    bool runOnFunction(Function& F) override;
};


namespace rv {

/*
 * Uses the VectorShape results to determine basic blocks which
 * are always executed by all threads(ABA), or either by all or none
 * of the threads(ABAON)
 */
class ABAAnalysis {
public:
    ABAAnalysis(PlatformInfo & platInfo,
                VectorizationInfo& vecInfo,
                const LoopInfo& loopInfo,
                const PostDominatorTree& postDomTree,
                const DominatorTree& domTree);
    ABAAnalysis(const ABAAnalysis&) = delete;
    ABAAnalysis& operator=(ABAAnalysis) = delete;

    void analyze(Function& F);

private:
    VectorizationInfo&           mVecinfo;
    VectorFuncMap&               mFuncinfo;
    const LoopInfo&              mLoopInfo;
    const PostDominatorTree&     mPostDomTree;
    const DominatorTree&         mDomTree;
    const Region*                mRegion;

    // Always-by-all/Always-by-all-or-none analysis
    void markABABlocks(Function& F);
    void markABAONBlocks(Function& F);

    bool isInDivergentLoop(const BasicBlock& block);
    bool isABAONSESEExit(BasicBlock& block);

    void checkEquivalentToOldAnalysis(Function& F);
};

FunctionPass* createABAAnalysisPass();

}

#endif // RV_ABAANALYSIS_H

