//===- src/analysis/reductions.cpp - supported reductions --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "rv/analysis/reductions.h"

#include "llvm/IR/Instructions.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Constants.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/StringRef.h"

#include <string.h>

using namespace llvm;

#ifdef RV_DEBUG
#define IF_DEBUG_RED if (true)
#else
#define IF_DEBUG_RED if (false)
#endif


namespace rv {

RedKind JoinKinds(RedKind A, RedKind B) {
  // default bottom rules
  if (A == RedKind::Bot) return B;
  if (B == RedKind::Bot) return A;

  // fallback rule
  if (A != B) return RedKind::Top;
  return A; // A == B
}


StringRef
to_string(RedKind red) {
  switch (red) {
    default:
      llvm_unreachable("unrecognized reduction");
    case RedKind::Bot: return "Bot";
    case RedKind::Top: return "Top";
    case RedKind::Add: return "Add";
    case RedKind::Mul: return "Mul";
    case RedKind::And: return "And";
    case RedKind::Or: return "Or";
    case RedKind::SMax: return "SMax";
    case RedKind::UMax: return "UMax";
    case RedKind::SMin: return "SMin";
    case RedKind::UMin: return "UMin";
    case RedKind::FMin: return "FMin";
    case RedKind::FMax: return "FMax";
  }
}

bool
from_string(StringRef redKindText, RedKind & oRedKind) {
  for (int64_t itRed = (int64_t) RedKind::Enum_Begin;
      itRed < (int64_t) RedKind::Enum_End;
      ++itRed) {
    RedKind kind = (RedKind) itRed;
    if (redKindText == to_string(kind)) {
        oRedKind = kind;
        return true;
    }
  }
  return false;
}

Constant&
GetNeutralElement_fp(RedKind redKind, Type & chainTy) {
  bool isDouble = chainTy.isDoubleTy();
  const double maxDouble = std::numeric_limits<double>::max();
  const double minDouble = std::numeric_limits<double>::min();
  const double maxFloat = std::numeric_limits<float>::max();
  const double minFloat = std::numeric_limits<float>::min();

  switch(redKind) {
    default:
      llvm_unreachable("reduction unsupported for this type");
    case RedKind::Add:
      return *ConstantFP::get(&chainTy, 0.0);

    case RedKind::Mul:
      return *ConstantFP::get(&chainTy, 1.0);

    case RedKind::FMax:
      return *ConstantFP::get(&chainTy, isDouble ? minDouble : minFloat);

    case RedKind::FMin:
      return *ConstantFP::get(&chainTy, isDouble ? maxDouble : maxFloat);

  }
}

Constant&
GetNeutralElement_int(RedKind redKind, Type & chainTy) {
  uint32_t numBits = chainTy.getIntegerBitWidth();
  uint64_t highestBitSet = 1 << (numBits - 1);

  switch (redKind) {
  default:
    llvm_unreachable("unsupported integer reduction");
  case RedKind::Add:
    return *ConstantInt::getNullValue(&chainTy);
  case RedKind::Mul:
    return *ConstantInt::get(&chainTy, false, 1);
  case RedKind::And:
    return *ConstantInt::getAllOnesValue(&chainTy);
  case RedKind::Or:
    return *ConstantInt::getNullValue(&chainTy);
  case RedKind::UMax:
    return *ConstantInt::get(&chainTy, 0); // 00..00
  case RedKind::SMax:
    return *ConstantInt::get(&chainTy, highestBitSet, true); // 10..00
  case RedKind::UMin:
    return *ConstantInt::getAllOnesValue(&chainTy); // 1..11
  case RedKind::SMin:
    return *ConstantInt::get(&chainTy, 0 ^ highestBitSet); // 01..11
  }
}

Constant&
GetNeutralElement(RedKind redKind, Type & chainTy) {
  if (chainTy.isFloatingPointTy()) return GetNeutralElement_fp(redKind, chainTy);
  if (chainTy.isIntegerTy()) return GetNeutralElement_int(redKind, chainTy);
  llvm_unreachable("unsupported type for reduction");
}

RedKind
InferInstRedKind(Instruction & inst) {
  switch (inst.getOpcode()) {
  // actually operations folding a reduction input into the chian
    case Instruction::FAdd:
    case Instruction::Add:
      return RedKind::Add;

    case Instruction::FSub:
    case Instruction::Sub:
      return RedKind::Top;

    case Instruction::FMul:
    case Instruction::Mul:
      return RedKind::Mul;

    case Instruction::Or:
      return RedKind::Or;

    case Instruction::And:
      return RedKind::And;

  // preserving operations
    case Instruction::Select:
    case Instruction::PHI:
      return RedKind::Bot;

  // unkown -> unrecognized operation in reduction chain
    default:
      return RedKind::Top;
  }
  abort();
}


} // namespace rv
