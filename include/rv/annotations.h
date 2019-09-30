//===- rv/annotations.h - getter/sterr for IR augmentations --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef RV_ANNOTATIONS_H
#define RV_ANNOTATIONS_H

#include "rv/analysis/reductions.h"

namespace llvm {
  class Function;
  class PHINode;
}

namespace rv {
  void MarkAsCriticalSection(llvm::Function & func);
  bool IsCriticalSection(const llvm::Function & func);

  void SetReductionHint(llvm::PHINode & loopHeaderPhi, RedKind redKind);
  RedKind ReadReductionHint(const llvm::PHINode & loopHeaderPhi);
}

#endif
