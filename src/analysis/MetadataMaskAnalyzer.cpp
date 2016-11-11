//===- MetadataMaskAnalyzer.h -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @author kloessner
//

#include "rv/analysis/MetadataMaskAnalyzer.h"
#include <rvConfig.h>

namespace rv
{

MetadataMaskAnalyzer::MetadataMaskAnalyzer(rv::VectorizationInfo& vecInfo)
        : mVecInfo(vecInfo)
{ }

void
MetadataMaskAnalyzer::markMasks(Function& F)
{
    DEBUG_VA( outs() << "\nMarking mask operations...\n"; );

    for (auto& BB : F)
    {
        for (auto& I : BB)
        {
            if (BranchInst* br = dyn_cast<BranchInst>(&I))
            {
                if (br->isConditional())
                {
                    if (!isa<Instruction>(br->getCondition())) continue;
                    markAsMask(cast<Instruction>(br->getCondition()));
                }
            }
            else if (SelectInst* sel = dyn_cast<SelectInst>(&I))
            {
                // TODO: HERE! Also mark arguments as masks?
                if (!isa<Instruction>(sel->getCondition())) continue;
                markAsMask(cast<Instruction>(sel->getCondition()));
            }
        }
    }
}

void
MetadataMaskAnalyzer::markAsMask(Instruction* inst)
{
    assert (inst);

    if (mVecInfo.isMetadataMask(inst)) return;
    if (!inst->getType()->isIntegerTy(1)) return;

    mVecInfo.markMetadataMask(inst);

    // Stop at compare instructions
    if (isa<CmpInst>(inst)) return;

    // If this is no compare instruction, go backwards and mark operands as MASK.
    for (auto& operand : inst->operands())
    {
        if (!isa<Instruction>(operand)) continue;

        Instruction* opInst = cast<Instruction>(operand);
        markAsMask(opInst);
    }
}


}
