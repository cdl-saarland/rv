//===- rv/passes/lowerRVIntrinsics.h - implement rv intrinsics for
//numThread==1  --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#include "rv/legacy/passes.h"
#include "llvm/IR/PassManager.h"
#include "llvm/Pass.h"

namespace rv {

//// Old PM Impl /////
class LowerRVIntrinsicsLegacyPass : public llvm::FunctionPass {
public:
  static char ID;
  LowerRVIntrinsicsLegacyPass();

  bool runOnFunction(llvm::Function &F) override;

  /// Register all analyses and transformation required.
  void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;
};

///// New PM Impl /////
struct LowerRVIntrinsicsWrapperPass
    : llvm::PassInfoMixin<LowerRVIntrinsicsWrapperPass> {
public:
  LowerRVIntrinsicsWrapperPass() {}

  llvm::PreservedAnalyses run(llvm::Function &F,
                              llvm::FunctionAnalysisManager &FAM);
};

} // namespace rv
