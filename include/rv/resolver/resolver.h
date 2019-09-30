//===- rv/resolver/resolver.h - function call resolver --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef RV_RESOLVER_H
#define RV_RESOLVER_H

#include "rv/vectorMapping.h"
#include "rv/shape/vectorShape.h"

#include <llvm/ADT/StringRef.h>
#include <memory>
#include <cstddef>

namespace llvm {
  class FunctionType;
  class Function;
  class Module;
  class raw_ostream;
}

namespace rv {

// represents an abstract notion of cost associated with a function.
struct
FunctionCost {
  size_t cost;
};


// Represents a way to vectorize a function.
//
// Provides access to a cost esimate, the result value shape and the vectorized function.
class
FunctionResolver {
protected:
  llvm::Module & targetModule;

public:
  virtual ~FunctionResolver();

  static VectorShape ComputeShape(const VectorShapeVec & argShapes);

  FunctionResolver(llvm::Module & _targetModule)
  : targetModule(_targetModule)
  {}

  // return a cost estimate for this function.
  virtual FunctionCost requestCostEstimate() { return FunctionCost{1}; }

  // materialized the vectorized function in the module @insertInto and returns a reference to it.
  virtual llvm::Function& requestVectorized() = 0;

  // how the vector function of \p requestVectorized() should be called in a predicated context.
  virtual CallPredicateMode getCallSitePredicateMode() = 0;

  // mask position (if any)
  virtual int getMaskPos() = 0;

  // result shape of function @funcName in target module @module.
  virtual VectorShape requestResultShape() = 0;
};

// abstract function resolver interface
class
ResolverService {
public:
  virtual ~ResolverService();
  virtual std::unique_ptr<FunctionResolver> resolve(llvm::StringRef funcName, llvm::FunctionType & scaFuncTy, const VectorShapeVec & argShapes, int vectorWidth, bool hasPredicate, llvm::Module & destModule) = 0;

  void dump() const;
  virtual void print(llvm::raw_ostream & out) const;
};


} // namespace rv


#endif // RV_RESOLVER_H
