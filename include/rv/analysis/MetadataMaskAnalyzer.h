//===- MetadataMaskAnalyzer.h -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//

#ifndef RV_METADATAMASKANALYZER_H
#define RV_METADATAMASKANALYZER_H

#include <llvm/IR/Instructions.h>

#include <rv/vectorizationInfo.h>

using namespace llvm;

namespace rv
{

class MetadataMaskAnalyzer
{
public:
    MetadataMaskAnalyzer(VectorizationInfo& vecInfo);

    void markMasks(Function& F);
private:
    VectorizationInfo& mVecInfo;

    void markAsMask(Instruction* inst);
};

}

#endif // RV_METADATAMASKANALYZER_H
