//===- rv/analysis/reductions.h - supported reductions --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef RV_ANALYSIS_REDUCTIONS_H
#define RV_ANALYSIS_REDUCTIONS_H

#include <stdint.h>
#include "llvm/ADT/StringRef.h"

namespace llvm {
  class Constant;
  class Instruction;
  class Type;
}

namespace rv {

enum class RedKind : int64_t {
  Enum_Begin = -1,
  Top = -1, // not a recognized reduction
  Bot = 0, // not yet analyzed or recurrence
  Add = 1,
  Mul = 2,
  And = 3,
  Or = 4,
  SMax = 5,
  UMax = 6,
  SMin = 7,
  UMin = 8,
  FMin = 9,
  FMax = 10,

  Enum_End = 11
};

// join operator
RedKind JoinKinds(RedKind A, RedKind B);

llvm::StringRef to_string(RedKind red);
bool from_string(llvm::StringRef redKindText, RedKind & oRedKind);

// get the neutral element for this reduction kind and data type
llvm::Constant& GetNeutralElement(RedKind redKind, llvm::Type & chainType);

// try to infer the reduction kind of the operator implemented by inst
RedKind InferInstRedKind(llvm::Instruction & inst);

}

#endif // RV_ANALYSIS_REDUCTIONS_H
