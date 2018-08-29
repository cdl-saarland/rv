#include "rv/resolver/listResolver.h"
#include "utils/rvTools.h"
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>

#include "rvConfig.h"

using namespace llvm;

namespace rv {

ListResolver::~ListResolver()
{}

VectorMapping
ListResolver::inferMapping(llvm::Function &scalarFnc,
                                          llvm::Function &simdFnc,
                                          int maskPos) {

  // return shape
  rv::VectorShape resultShape;

  auto *scalarRetTy = scalarFnc.getReturnType();
  auto *simdRetTy = simdFnc.getReturnType();

  if (typesMatch(scalarRetTy, simdRetTy)) {
    resultShape = VectorShape::uni();
  } else {
    assert(simdRetTy->isVectorTy() && "return type mismatch");
    resultShape = VectorShape::varying();
  }

  // argument shapes
  rv::VectorShapeVec argShapes;

  auto itScalarArg = scalarFnc.arg_begin();
  auto itSimdArg = simdFnc.arg_begin();

  for (size_t i = 0; i < simdFnc.arg_size(); ++i) {
    // mask special case
    if (maskPos >= 0 && (i == (uint)maskPos)) {
      argShapes.push_back(VectorShape::varying());
      ++itSimdArg;
      continue;
    }

    // trailing additional argument case
    if (itScalarArg == scalarFnc.arg_end()) {
      IF_DEBUG errs() << "Unexpected additional argument (pos " << i
                      << ") in simd function " << simdFnc << "\n";
      argShapes.push_back(VectorShape::varying());
      ++itSimdArg;
      continue;
    }

    // default argument case
    if (typesMatch(itScalarArg->getType(), itSimdArg->getType())) {
      argShapes.push_back(VectorShape::uni()); // unaligned
    } else {
      argShapes.push_back(VectorShape::varying());
    }

    ++itScalarArg;
    ++itSimdArg;
  }

  assert(itScalarArg == scalarFnc.arg_end());
  assert(itSimdArg == simdFnc.arg_end());

  int vecWidth = 0; // FIXME
  return rv::VectorMapping(&scalarFnc, &simdFnc,
                               vecWidth, // if all arguments have shapes this
                                         // function is suitable for all
                                         // possible widths
                               maskPos, resultShape, argShapes);
}

// shape based mappings
bool
ListResolver::addMapping(rv::VectorMapping &&mapping) {
  auto it = funcMappings.find(mapping.scalarFn);
  VecMappingShortVec * vecMappings = nullptr;
  if (it == funcMappings.end()) {
    auto ItInserted = funcMappings.emplace(mapping.scalarFn, new VecMappingShortVec);
    vecMappings = &*ItInserted.first->second;
  } else {
    vecMappings = &*it->second;
  }

  // check if there is an equivalent mapping already
  for (auto & knownMapping : *vecMappings) {
    if (knownMapping == mapping) return false;
  }

  vecMappings->push_back(mapping);
  return true;
}


// query available vector mappings for a given vector call signature
void
ListResolver::ForAll_MappingsForCall(std::function<bool(const VectorMapping&)> MatchFunc, const llvm::Function & scalarFn, const VectorShapeVec & argShapes, uint vectorWidth, bool needsPredication) {
// register user shapes
  auto it = funcMappings.find(&scalarFn);
  if (it == funcMappings.end()) return;
  auto & allMappings = *it->second;

  for (auto & mapping : allMappings) {
    if (mapping.vectorWidth > 1 && (mapping.vectorWidth != vectorWidth)) continue;
    if (mapping.maskPos < 0 && needsPredication) continue;

    // check that all arg shapes are compatible with the shapes in the caller
    bool foundIncompatibleShape = false;
    for (int i = 0; i < (int) argShapes.size(); ++i) {
      if (!mapping.argShapes[i].contains(argShapes[i])) {
        foundIncompatibleShape = true;
        break;
      }
    }
    if (foundIncompatibleShape) continue;

    // valid match
    MatchFunc(mapping);
  }
}

class
MappedFunctionResolver : public FunctionResolver {
  const VectorMapping & mapping;

public:
  MappedFunctionResolver(const VectorMapping & mapping)
  : FunctionResolver(*mapping.scalarFn->getParent())
  , mapping(mapping)
  {}

  // materialized the vectorized function in the module @insertInto and returns a reference to it.
  llvm::Function& requestVectorized() {
    return *mapping.vectorFn;
  }

  // result shape of function @funcName in target module @module.
  VectorShape requestResultShape() {
    return mapping.resultShape;
  }
};


// result shape of function @funcName in target module @module.
std::unique_ptr<FunctionResolver>
ListResolver::resolve(llvm::StringRef funcName, llvm::FunctionType & scaFuncTy, const VectorShapeVec & argShapes, int vectorWidth, llvm::Module & destModule) {
  IF_DEBUG { errs() << "ListResolverService: " << funcName << " for width " << vectorWidth << "\n"; }

  // scalar function not available
  auto * scaFunc = destModule.getFunction(funcName);
  if (!scaFunc) {
    IF_DEBUG { errs() << "\tListR: not in module!\n"; }
    return nullptr;
  }

  // signature mismatch
  if (!typesMatch(scaFunc->getFunctionType(), &scaFuncTy)) {
    IF_DEBUG { errs() << "\tListR: type mismatch!\n"; }
    return nullptr;
  }

  bool needsPredication = false; // FIXME

  // look-up mapping
  VectorShape bestResultShape = VectorShape::varying();
  const VectorMapping * bestMapping = nullptr;
  ForAll_MappingsForCall([&](const VectorMapping & mapping) {
      IF_DEBUG {
        errs() << "ListR: match ";
        mapping.print(errs());
      }

      // have we found a better mapping?
      if (mapping.resultShape.morePreciseThan(bestResultShape)) {
         bestResultShape = mapping.resultShape;
         bestMapping = &mapping;
      }

      // keep looking for a better mapping
      return true;
  }, *scaFunc, argShapes, vectorWidth, needsPredication);

  if (!bestMapping) return nullptr;
  return std::unique_ptr<FunctionResolver>(new MappedFunctionResolver(*bestMapping));
}

void
ListResolver::print(raw_ostream & out) const {
  out << "ListR {\n";
  for (auto & it : funcMappings) {
    for (auto & itMapping : *it.second) {
      itMapping.print(out);
    }
  }
  out << "}\n";
}

/// Forget one specific mapping.
void
ListResolver::forgetMapping(const VectorMapping & mapping) {
  auto itFunc = funcMappings.find(mapping.scalarFn);
  if (itFunc == funcMappings.end()) {
    return;
  }

  VecMappingShortVec & vecMappings = *itFunc->second;

  // scan for that mapping
  auto it = vecMappings.begin();
  auto itEnd = vecMappings.end();
  for (;it != itEnd; ++it) {
    if (!(*it == mapping)) continue;

    // found it!
    vecMappings.erase(it);
    break;
  }
}

void
ListResolver::forgetAllMappingsFor(const Function & scaFunc) {
  auto It = funcMappings.find(&scaFunc);
  if (It == funcMappings.end()) return;
  funcMappings.erase(It);
}


} // namespace rv
