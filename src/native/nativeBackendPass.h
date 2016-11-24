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

// wrapper pass for Montadas NaTIVE backend
  class NativeBackendPass : public llvm::FunctionPass {
  public:
    static char ID;

    NativeBackendPass();

    ~NativeBackendPass();

    void getAnalysisUsage(llvm::AnalysisUsage &AU) const;

    bool runOnFunction(llvm::Function &F);
  };


}


#endif /* SRC_NATIVE_NATIVEBACKENDPASS_H_ */
