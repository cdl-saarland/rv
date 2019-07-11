//===- transform/lowerRVIntrinsics.h - lower intrinsics  ----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//
//===----------------------------------------------------------------------===//

#include "llvm/Pass.h"

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

} // namespace rv
