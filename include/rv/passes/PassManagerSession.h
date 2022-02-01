//===- rv/passes/PassManagerSession.h - auto-vectorize math  --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#ifndef RV_PASSES_PASSMANAGERSESSION_H
#define RV_PASSES_PASSMANAGERSESSION_H

#include "llvm/IR/PassManager.h"
#include "llvm/Analysis/LoopAnalysisManager.h"
#include "llvm/Analysis/CGSCCPassManager.h"

namespace rv {

class PassManagerSession {
public:
  PassManagerSession();

  llvm::FunctionAnalysisManager FAM;
  llvm::LoopAnalysisManager LAM;
  llvm::CGSCCAnalysisManager CGAM;
  llvm::ModuleAnalysisManager MAM;
};

}

#endif // RV_PASSES_PASSMANAGERSESSION_H
