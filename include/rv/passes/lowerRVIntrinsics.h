//===- rv/passes/lowerRVIntrinsics.h - implement rv intrinsics for
//numThread==1  --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#include "llvm/IR/PassManager.h"
#include "llvm/Pass.h"

namespace rv {

///// New PM Impl /////
struct LowerRVIntrinsicsWrapperPass
    : llvm::PassInfoMixin<LowerRVIntrinsicsWrapperPass> {
public:
  LowerRVIntrinsicsWrapperPass() {}

  llvm::PreservedAnalyses run(llvm::Function &F,
                              llvm::FunctionAnalysisManager &FAM);
};

} // namespace rv
