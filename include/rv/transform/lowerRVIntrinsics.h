//===- rv/transform/lowerRVIntrinsics.h - implement rv intrinsics for numThread==1  --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#include "llvm/Pass.h"
#include "llvm/IR/PassManager.h"
#include "rv/passes.h"

namespace rv {
class LowerRVIntrinsics : public llvm::FunctionPass {
public:
  static char ID;
  LowerRVIntrinsics() : llvm::FunctionPass(ID)
  {}

  bool runOnFunction(llvm::Function &F) override;

  /// Register all analyses and transformation required.
  void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;
};

struct LowerRVIntrinsicsWrapperPass : llvm::PassInfoMixin<LowerRVIntrinsicsWrapperPass> {
  private:
    std::shared_ptr<llvm::FunctionPass> lower;
  public:
    LowerRVIntrinsicsWrapperPass() : lower(rv::createLowerRVIntrinsicsPass()) {};

    llvm::PreservedAnalyses run (llvm::Function &F, llvm::FunctionAnalysisManager &FAM) {
      if (lower->runOnFunction(F))
        return llvm::PreservedAnalyses::none();
      else
        return llvm::PreservedAnalyses::all();
    }
};

} // namespace rv
