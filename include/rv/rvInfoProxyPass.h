//===- rvInfoProxyPass.h ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//

#ifndef RV_WFVINFOPROXYPASS_H
#define RV_WFVINFOPROXYPASS_H

#include <llvm/Pass.h>

namespace rv {
  class RVInfo;
}

namespace llvm {
void initializeRVInfoProxyPassPass(PassRegistry&);
ImmutablePass* createRVInfoProxyPass(rv::RVInfo* rvInfo);

}

class RVInfoProxyPass : public llvm::ImmutablePass {
public:
    static char ID;

    RVInfoProxyPass();
    RVInfoProxyPass(rv::RVInfo *rvInfo);

    rv::RVInfo& getInfo() const;

    virtual void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;

    virtual bool runOnModule(llvm::Module& M) override;

    virtual void releaseMemory() override;

private:
    rv::RVInfo *mRVInfo;
};

#endif //RV_RVVINFOPROXYPASS_H
