#include "rv/resolver/resolver.h"
#include "rv/vectorShape.h"
#include <llvm/Support/raw_ostream.h>

namespace rv {

ResolverService::~ResolverService()
{}

void
ResolverService::print(llvm::raw_ostream & out) const {
  out << "{ResolverService}\n";
}

void
ResolverService::dump() const {
  print(llvm::errs());
}

FunctionResolver::~FunctionResolver()
{}

// result shape of function @funcName in target module @module
VectorShape
FunctionResolver::ComputeShape(const VectorShapeVec & argShapes) {
  // TODO run VA
  for (const auto & argShape : argShapes) {
    if (!argShape.isUniform()) return VectorShape::varying();
  }
  return VectorShape::uni();
}


} // namespace rv
