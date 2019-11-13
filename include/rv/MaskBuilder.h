//===- rv/vectorizationInfo.h - vectorizer IR using an overlay object --*- C++
//-*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Utility class for manipulating \p Mask objects.
//
//===----------------------------------------------------------------------===//

#ifndef INCLUDE_RV_MASKBUILDER_H
#define INCLUDE_RV_MASKBUILDER_H

#include "rv/Mask.h"
#include "rv/vectorizationInfo.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/ADT/Twine.h"

namespace rv {

class MaskBuilder {
  VectorizationInfo &VecInfo;

public:
  MaskBuilder(VectorizationInfo &VecInfo) : VecInfo(VecInfo) {}

  // Fold the ActiveVectorLength of M into its predicate.
  Mask FoldAVL(llvm::IRBuilder<> &Builder, Mask M, llvm::Twine Name="");

  // Perform logic operation on the input masks.
  Mask CreateOr(llvm::IRBuilder<> &Builder, Mask A, Mask B, llvm::Twine Name="");
  Mask CreateAnd(llvm::IRBuilder<> &Builder, Mask A, Mask B, llvm::Twine Name="");
  Mask CreateNot(llvm::IRBuilder<> &Builder, Mask M, llvm::Twine Name="");
};

} // namespace rv

#endif // INCLUDE_RV_MASKBUILDER_H
