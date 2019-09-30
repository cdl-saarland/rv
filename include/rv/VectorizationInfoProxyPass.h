//===- rv/VectorizationInfoProxyPass.h - vecInfo proxy pass (FIXME deprecated) --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
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
  class PlatformInfo;
}


// proxy pass for querying DivergenceInfo objects inside analysis passes
class VectorizationInfoProxyPass : public llvm::ImmutablePass {
  public:
    static char ID;

  private:
    rv::VectorizationInfo * vecInfo;
    rv::PlatformInfo * platInfo;

  public:
    VectorizationInfoProxyPass();
    VectorizationInfoProxyPass(rv::PlatformInfo & _platInfo, rv::VectorizationInfo & _vecInfo);
    rv::VectorizationInfo & getInfo() const;
    rv::PlatformInfo & getPlatformInfo() const;
};

#endif //RV_VECTORIZATIONINFOPROXYPASS_H
