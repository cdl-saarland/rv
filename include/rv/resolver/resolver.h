#ifndef RV_RESOLVER_H
#define RV_RESOLVER_H

#include <cstddef>
#include <llvm/ADT/StringRef.h>

#include "rv/vectorShape.h"
#include <memory>

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

  // result shape of function @funcName in target module @module.
  virtual VectorShape requestResultShape() = 0;
};

// abstract function resolver interface
class
ResolverService {
public:
  virtual ~ResolverService();
  virtual std::unique_ptr<FunctionResolver> resolve(llvm::StringRef funcName, llvm::FunctionType & scaFuncTy, const VectorShapeVec & argShapes, int vectorWidth, llvm::Module & destModule) = 0;

  void dump() const;
  virtual void print(llvm::raw_ostream & out) const;
};


} // namespace rv


#endif // RV_RESOLVER_H
