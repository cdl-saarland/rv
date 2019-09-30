//===- rv/vectorMapping.h - scalar to vector function mapping --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#ifndef INCLUDE_RV_VECTORMAPPING_H_
#define INCLUDE_RV_VECTORMAPPING_H_

#include "rv/shape/vectorShape.h"
#include <llvm/ADT/SmallVector.h>
#include <initializer_list>

namespace llvm {
class Function;
class raw_ostream;
} // namespace llvm

namespace rv {

enum class CallPredicateMode {
  // Function does not support predication and can only be called in an unpredicated context.
  Unpredicated = 0,
  // Function has an explicit predicate argument.
  PredicateArg = 1,
  // Can be called with an invalid predicate.
  SafeWithoutPredicate = 2
};

std::string to_string(CallPredicateMode PredMode);

struct VectorMapping {
  llvm::Function *scalarFn;
  llvm::Function *vectorFn;
  unsigned vectorWidth; // == 0 implies that this mapping is sound for all
                        // vectorWidths (no varying shapes), otw this mapping is
                        // only applicable for the specified width
  int maskPos; // < 0 => no mask argument, otw position of mask argument
  VectorShapeVec argShapes;
  VectorShape resultShape;
  // If scalarFn == vectorFn the following vectorization rules apply
  // 1. scalarFn has side effects -> function call will be replicated for all
  // lanes at every call site
  // 2. resultShape == VARYING -> replicated
  // 3. the mapping contains at least one VARYING argument -> replicated
  // 4. the function is called with a VARYING parameter -> replicated for this
  // call
  // 5. the function is called with a parameter of vector shape T that was not
  // registered with a VectorMapping -> replicated for this call

  // example : get_thread_id()
  // -> resultShape == Consecutive, no side effects

  CallPredicateMode predMode;

  VectorMapping(llvm::Function *_scalarFn, llvm::Function *_vectorFn,
                unsigned _vectorWidth, int _maskPos, VectorShape _resultShape,
                std::initializer_list<VectorShape> argShapeList, CallPredicateMode _predMode)
      : scalarFn(_scalarFn), vectorFn(_vectorFn), vectorWidth(_vectorWidth),
        maskPos(_maskPos), argShapes(argShapeList), resultShape(_resultShape),
        predMode(_predMode) {}

  VectorMapping(llvm::Function *_scalarFn, llvm::Function *_vectorFn,
                unsigned _vectorWidth, int _maskPos, VectorShape _resultShape,
                const VectorShapeVec &_argShapeVec, CallPredicateMode _predMode)
      : scalarFn(_scalarFn), vectorFn(_vectorFn), vectorWidth(_vectorWidth),
        maskPos(_maskPos), argShapes(_argShapeVec), resultShape(_resultShape),
        predMode(_predMode) {}

  VectorMapping(llvm::Function *_scalarFn, llvm::Function *_vectorFn,
                unsigned _vectorWidth, CallPredicateMode _predMode)
      : scalarFn(_scalarFn), vectorFn(_vectorFn), vectorWidth(_vectorWidth),
        maskPos(-1), argShapes(), resultShape(), predMode(_predMode) {}

  VectorMapping()
      : scalarFn(nullptr), vectorFn(nullptr), vectorWidth(0), maskPos(-1),
        argShapes(), resultShape(),
        predMode(CallPredicateMode::Unpredicated) {}

  bool operator==(const VectorMapping &O) const {
    return (scalarFn == O.scalarFn) && (vectorFn == O.vectorFn) &&
           (vectorWidth == O.vectorWidth) && (maskPos == O.maskPos) &&
           (argShapes == O.argShapes) && (resultShape == O.resultShape);
  }

  void dump() const;
  void print(llvm::raw_ostream &out) const;

  bool supportsPredicatedCall() const;
};

} // namespace rv

#endif /* INCLUDE_RV_VECTORMAPPING_H_ */
