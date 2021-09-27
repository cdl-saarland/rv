//===- rv/passes/OMPDeclutter.h - Remove clutter in the IR --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef RV_TRANSFORM_OMPDECLUTTER_H
#define RV_TRANSFORM_OMPDECLUTTER_H

#include <map>
#include <vector>

#include "llvm/Pass.h"
#include "llvm/IR/PassManager.h"

namespace rv {

class RVInfo;

struct OMPDeclutterWrapperPass : llvm::PassInfoMixin<OMPDeclutterWrapperPass> {
public:
  OMPDeclutterWrapperPass();
  static llvm::StringRef name() { return "rv::OMPDeclutterWrapperPass"; }
  llvm::PreservedAnalyses run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM);
};

class OMPDeclutterLegacyPass : public llvm::FunctionPass {
public:
  static char ID;
  OMPDeclutterLegacyPass();

  bool runOnFunction(llvm::Function &F) override;

  /// Register all analyses and transformation required.
  void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;
};

} // namespace rv

#endif // RV_TRANSFORM_OMPDECLUTTER_H
