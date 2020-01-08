//===- rv/transform/lowerDivergentSwitches.h - if-cascades for divergent switches  --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#ifndef RV_TRANSFORM_LOWERDIVERGENTSWITCHES_H
#define RV_TRANSFORM_LOWERDIVERGENTSWITCHES_H

#include <llvm/IR/Instruction.h>
#include "llvm/IR/PassManager.h"

namespace llvm {
  class SwitchInst;
  class LoopInfo;
  class BasicBlock;
}

namespace rv {

class VectorizationInfo;

class LowerDivergentSwitches {
  VectorizationInfo & vecInfo;
  llvm::FunctionAnalysisManager & FAM;
  llvm::LoopInfo & LI;

  void lowerSwitch(llvm::SwitchInst & swInst);
  void replaceIncoming(llvm::BasicBlock & phiBlock, llvm::BasicBlock & oldIncoming, llvm::BasicBlock & newIncoming);

public:
  LowerDivergentSwitches(VectorizationInfo & _vecInfo, llvm::FunctionAnalysisManager & FAM);
  bool run();
};

} // namespace rv


#endif // RV_TRANSFORM_LOWERDIVERGENTSWITCHES_H
