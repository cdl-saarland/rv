//===- rv/transform/CNSPass.h - Regularizer pass  --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#ifndef RV_TRANSFORM_CNSPASS_H
#define RV_TRANSFORM_CNSPASS_H

#include <llvm/IR/PassManager.h>
#include "rv/passes.h"

namespace rv {

  struct CNSWrapperPass : llvm::PassInfoMixin<CNSWrapperPass> {
    private:
      std::shared_ptr<llvm::FunctionPass> cns;
    public:
      CNSWrapperPass() : cns(rv::createCNSPass()) {};

      llvm::PreservedAnalyses run(llvm::Function &F, llvm::FunctionAnalysisManager &AM) {
        if (cns->runOnFunction(F))
          return llvm::PreservedAnalyses::none();
        else
          return llvm::PreservedAnalyses::all();
      };
  };

} // namespace rv
#endif // RV_TRANSFORM_CNSPASS_H
