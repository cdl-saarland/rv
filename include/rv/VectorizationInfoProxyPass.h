//===- VectorizationInfoProxyPass.h -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//

#ifndef RV_VECTORIZATIONINFOPROXYPASS_H
#define RV_VECTORIZATIONINFOPROXYPASS_H

#include <llvm/Pass.h>
#include <rv/vectorizationInfo.h>

namespace llvm {
void initializeVectorizationInfoProxyPassPass(PassRegistry&);
}

namespace rv {
  class VectorizationInfo;
}


// proxy pass for querying DivergenceInfo objects inside analysis passes
class VectorizationInfoProxyPass : public llvm::ImmutablePass {
  public:
    static char ID;

  private:
    rv::VectorizationInfo * vectorizationInfo;

  public:
    VectorizationInfoProxyPass();
    VectorizationInfoProxyPass(rv::VectorizationInfo * _vi);
    rv::VectorizationInfo & getInfo() const;
};

#endif //RV_VECTORIZATIONINFOPROXYPASS_H
