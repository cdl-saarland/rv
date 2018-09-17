//===- vectorMapping.cpp -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @authors simon

#include "rv/vectorMapping.h"

#include <llvm/IR/Function.h>
#include <llvm/Support/raw_ostream.h>

using namespace llvm;

namespace rv {

void VectorMapping::dump() const {
  print(errs());
}

void VectorMapping::print(llvm::raw_ostream &out) const {
  out << "VectorMapping {\n"
      << "\tscalarFn = " << (scalarFn ? scalarFn->getName() : "null") << "\n"
      << "\tvectorFn = " << (vectorFn ? vectorFn->getName() : "null") << "\n"
      << "\tvectorW  = " << vectorWidth << "\n"
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

} /* namespace rv */
