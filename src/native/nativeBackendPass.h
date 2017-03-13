//===- nativebackendPass.h -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//

#ifndef SRC_NATIVE_NATIVEBACKENDPASS_H_
#define SRC_NATIVE_NATIVEBACKENDPASS_H_

#include <llvm/Pass.h>

namespace rv {

class ReductionAnalysis;

// wrapper pass for Montadas NaTIVE backend
  class NativeBackendPass : public llvm::FunctionPass {
    VectorizationInfo *vi;
    PlatformInfo *pi;
    DominatorTree const *domTree;
    ReductionAnalysis * reda;

  public:
    static char ID;

    NativeBackendPass();
    NativeBackendPass(VectorizationInfo *vi, PlatformInfo *pi, DominatorTree const *domTree, ReductionAnalysis * _reda);

    ~NativeBackendPass();

    void getAnalysisUsage(llvm::AnalysisUsage &AU) const;

    bool runOnFunction(llvm::Function &F);
  };


}


#endif /* SRC_NATIVE_NATIVEBACKENDPASS_H_ */
