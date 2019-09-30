//===- rv/vectorMapping.cpp - scalar to vector function mapping --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#include "rv/vectorMapping.h"

#include <llvm/IR/Function.h>
#include <llvm/Support/raw_ostream.h>

using namespace llvm;

namespace rv {

std::string
to_string(CallPredicateMode PredMode) {
  switch (PredMode) {
    default: llvm_unreachable("unrecognized call predicate");
    case CallPredicateMode::Unpredicated: return "Unpredicated";
    case CallPredicateMode::PredicateArg: return "PredicateArg";
    case CallPredicateMode::SafeWithoutPredicate: return "SafeWithoutPredicate";
  }
}

void VectorMapping::dump() const {
  print(errs());
}

void VectorMapping::print(llvm::raw_ostream &out) const {
  out << "VectorMapping {\n"
      << "\tscalarFn = " << (scalarFn ? scalarFn->getName() : "null") << "\n"
      << "\tvectorFn = " << (vectorFn ? vectorFn->getName() : "null") << "\n"
      << "\tvectorW  = " << vectorWidth << "\n"
      << "\tpredMode = " << to_string(predMode) << "\n"
      << "\tmaskPos  = " << maskPos << "\n"
      << "\tresultSh = " << resultShape.str() << "\n"
      << "\tparamShs: {\n";
  auto itScalarArg = scalarFn->arg_begin();

  if (vectorFn) {
    // proper vector function
    auto itVectorArg = vectorFn->arg_begin();

    int i = 0;
    for (VectorShape argShape : argShapes) {
      if (i == maskPos) {
        ++itVectorArg; // skip the vector mask
      }
      out << "\t\t(" << i << ") " << *itScalarArg << " -> " << *itVectorArg
          << " : " << argShape.str() << "\n";
      ++i;
      ++itScalarArg;
      ++itVectorArg;
    }

  } else {
    // no vector function available
    int i = 0;
    for (VectorShape argShape : argShapes) {
      out << "\t\t(" << i << ") " << *itScalarArg << " : " << argShape.str() << "\n";
      ++i;
      ++itScalarArg;
    }
  }

  out << "\t}\n";
  out << "}\n";
}

bool
VectorMapping::supportsPredicatedCall() const {
  return (maskPos >= 0) || (predMode == CallPredicateMode::SafeWithoutPredicate);
}

} /* namespace rv */
