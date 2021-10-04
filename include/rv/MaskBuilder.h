//===- rv/vectorizationInfo.h - vectorizer IR using an overlay object --*- C++
//-*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// Utility class for manipulating \p Mask objects and setting thei vshapes on
// the fly.
//
//===----------------------------------------------------------------------===//

#ifndef INCLUDE_RV_MASKBUILDER_H
#define INCLUDE_RV_MASKBUILDER_H

#include "rv/Mask.h"
#include "rv/vectorizationInfo.h"
#include "llvm/ADT/Twine.h"
#include "llvm/IR/IRBuilder.h"

namespace rv {

class MaskBuilder {
  llvm::Value &CreatePredicateAnd(llvm::IRBuilder<> &Builder, llvm::Value &lhs,
                                  llvm::Value &rhs,
                                  const llvm::Twine &name = "");

protected:
  virtual llvm::Value &AddMaskVal(llvm::Value &Op) = 0;

  // implementation aide
  template <typename T> T &AddMaskOp(T &I) {
    return llvm::cast<T>(AddMaskVal(llvm::cast<llvm::Value>(I)));
  }
public:
  // initialize for P-LLVM context.
  MaskBuilder() {}
  virtual ~MaskBuilder() {}

  // Fold the ActiveVectorLength of M into its predicate.
  virtual Mask FoldAVL(llvm::IRBuilder<> &Builder, Mask M, llvm::Twine Name);

  // Perform logic operation on the input masks.
  Mask CreateOr(llvm::IRBuilder<> &Builder, Mask A, Mask B,
                llvm::Twine Name = "");
  Mask CreateAnd(llvm::IRBuilder<> &Builder, Mask A, Mask B,
                 llvm::Twine Name = "");
  Mask CreateNot(llvm::IRBuilder<> &Builder, Mask M, llvm::Twine Name = "");

  // Create a masked select
  //   \p ContextAVL  The EVL up to which lanes are defined.
  llvm::Value *CreateSelect(llvm::IRBuilder<> &builder, Mask CondMask,
                            llvm::Value *OnTrueVal,
                            llvm::Value *OnFalseVal,
                            llvm::Value *ContextEVL = nullptr,
                            llvm::Twine Name = "");
};

// MaskBuilder for the P-LLVM context
//   This instance updates mask info and marks mark operations as total.
class ScalarMaskBuilder final : public MaskBuilder {
  VectorizationInfo &VecInfo;
  // assign a shape for \p I from its operands in VecInfo
  llvm::Value &ComputeInstShape(llvm::Value &I);

protected:
  llvm::Value &AddMaskVal(llvm::Value &Op) override;

public:
  ScalarMaskBuilder(VectorizationInfo &VecInfo)
      : MaskBuilder(), VecInfo(VecInfo) {}

  Mask FoldAVL(llvm::IRBuilder<> &Builder, Mask M, llvm::Twine Name = "") override;
};

class VectorMaskBuilder final : public MaskBuilder {
  // TODO: Support SVE.
  unsigned VectorWidth;
protected:
  llvm::Value &AddMaskVal(llvm::Value &Op) override { return Op; }

public:
  VectorMaskBuilder(unsigned VectorWidth) : MaskBuilder(), VectorWidth(VectorWidth) {}

  Mask FoldAVL(llvm::IRBuilder<> &Builder, Mask M, llvm::Twine Name = "") override;
};

} // namespace rv

#endif // INCLUDE_RV_MASKBUILDER_H
