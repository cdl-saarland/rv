//===- rv/transform/CoherentIfTransform.h - Insertiong of all-true code paths --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#ifndef RV_TRANSFORM_CIF_H
#define RV_TRANSFORM_CIF_H

#include <llvm/IR/Value.h>
#include <llvm/Transforms/Utils/ValueMapper.h>
#include "llvm/IR/PassManager.h"

#include "rv/PlatformInfo.h"
#include "rv/shape/vectorShape.h"

namespace llvm {
  class AllocaInst;
  class DataLayout;
  class LoopInfo;
  class DominatorTree;
  class PostDominatorTree;
  class BranchProbabilityInfo;
}

namespace rv {

class MaskExpander;
class VectorizationInfo;

class CoherentIFTransform {
  VectorizationInfo & vecInfo;
  PlatformInfo & platInfo;
  MaskExpander & maskEx;
  llvm::DominatorTree & domTree;
  llvm::PostDominatorTree & postDomTree;
  llvm::LoopInfo & loopInfo;
  llvm::BranchProbabilityInfo * pbInfo;

public:
  CoherentIFTransform(VectorizationInfo & _vecInfo, PlatformInfo & _platInfo, MaskExpander & _maskEx, llvm::FunctionAnalysisManager &FAM);

  bool run();
};


} // namespace rv

#endif// RV_TRANSFORM_CIF_H
