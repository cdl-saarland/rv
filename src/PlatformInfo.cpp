//
// Created by thorsten on 06.10.16.
//

#include "rv/PlatformInfo.h"
#include "rv/sleefLibrary.h"

#include "utils/rvTools.h"

#include "rvConfig.h"

using namespace llvm;

namespace rv {

PlatformInfo::PlatformInfo(Module &_mod, TargetTransformInfo *TTI,
                           TargetLibraryInfo *TLI)
    : mod(_mod), mTTI(TTI), mTLI(TLI) {}

PlatformInfo::~PlatformInfo() {
  for (auto it : funcMappings) {
    delete it.second;
  }
}

void PlatformInfo::addMapping(const Function *function,
                              const rv::VectorMapping *mapping) {
  funcMappings[function] = mapping;
}

void PlatformInfo::removeMappingIfPresent(const Function *function) {
  auto found = funcMappings.find(function);

  if (found != funcMappings.end())
    funcMappings.erase(found);
}

const rv::VectorMapping *
PlatformInfo::getMappingByFunction(const Function *function) const {
  auto found = funcMappings.find(function);

  if (found != funcMappings.end())
    return found->second;

  return nullptr;
}

void PlatformInfo::setTTI(TargetTransformInfo *TTI) { mTTI = TTI; }

void PlatformInfo::setTLI(TargetLibraryInfo *TLI) { mTLI = TLI; }

TargetTransformInfo *PlatformInfo::getTTI() { return mTTI; }

TargetLibraryInfo *PlatformInfo::getTLI() { return mTLI; }

void PlatformInfo::addVectorizableFunctions(ArrayRef<VecDesc> funcs, bool givePrecedence) {
  auto itInsert = givePrecedence ? commonVectorMappings.begin() : commonVectorMappings.end();
  commonVectorMappings.insert(itInsert, funcs.begin(), funcs.end());
}

bool PlatformInfo::isFunctionVectorizable(StringRef funcName,
                                          unsigned vectorWidth) {
  return !getVectorizedFunction(funcName, vectorWidth).empty();
}

StringRef PlatformInfo::getVectorizedFunction(StringRef funcName,
                                              unsigned vectorWidth,
                                              bool *isInTLI) {
  if (funcName.empty())
    return funcName;

  // query custom mappings with precedence
  std::string funcNameStr = funcName.str();
  for (const auto & vd : commonVectorMappings) {
     if (vd.scalarFnName == funcNameStr && vd.vectorWidth == vectorWidth) return vd.vectorFnName;
  };

  // query TLI
  StringRef tliFnName = mTLI->getVectorizedFunction(funcName, vectorWidth);
  if (!tliFnName.empty()) {
    if (isInTLI)
      *isInTLI = true;
    return tliFnName;
  }

  // no mapping
  return StringRef();
}

Function *PlatformInfo::requestVectorizedFunction(StringRef funcName,
                                                  unsigned vectorWidth,
                                                  Module *insertInto,
                                                  bool doublePrecision) {
  bool isInTLI = false;
  StringRef vecFuncName =
      getVectorizedFunction(funcName, vectorWidth, &isInTLI);
  if (vecFuncName.empty())
    return nullptr;

  if (isInTLI)
    return insertInto->getFunction(vecFuncName);
  else
    return requestSleefFunction(funcName, vecFuncName, insertInto,
                                doublePrecision);
}

bool PlatformInfo::addSIMDMapping(rv::VectorMapping &mapping) {
  if (funcMappings.count(mapping.scalarFn))
    return false;
  funcMappings[mapping.scalarFn] = new rv::VectorMapping(mapping);
  return true;
}

// This function should be called *before* run().
bool PlatformInfo::addSIMDMapping(const Function &scalarFunction,
                                  const Function &simdFunction,
                                  const int maskPosition,
                                  const bool mayHaveSideEffects) {
  assert(scalarFunction.getParent() == simdFunction.getParent());

  // Find out which arguments are UNIFORM and which are VARYING.
  SmallVector<bool, 4> uniformArgs;
  uniformArgs.reserve(scalarFunction.arg_size());

  Function::const_arg_iterator scalarA = scalarFunction.arg_begin();
  Function::const_arg_iterator simdA = simdFunction.arg_begin();

  for (Function::const_arg_iterator scalarE = scalarFunction.arg_end();
       scalarA != scalarE; ++scalarA, ++simdA) {
    Type *scalarType = scalarA->getType();
    Type *simdType = simdA->getType();
    const bool isUniform = typesMatch(scalarType, simdType);

    uniformArgs.push_back(isUniform);
  }

  funcMappings[&scalarFunction] =
      inferMapping(const_cast<Function &>(scalarFunction),
                   const_cast<Function &>(simdFunction), maskPosition);

  return true;
}

VectorMapping *PlatformInfo::inferMapping(llvm::Function &scalarFnc,
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
  return new rv::VectorMapping(&scalarFnc, &simdFnc,
                               vecWidth, // if all arguments have shapes this
                                         // function is suitable for all
                                         // possible widths
                               maskPos, resultShape, argShapes);
}

Function *PlatformInfo::requestVectorMaskReductionFunc(const std::string &name, size_t width) {
  std::string mangledName = name + "_v" + std::to_string(width);
  auto *redFunc = mod.getFunction(mangledName);
  if (redFunc)
    return redFunc;
  auto &context = mod.getContext();
  auto *boolTy = Type::getInt1Ty(context);
  auto *vecBoolTy = VectorType::get(boolTy, width);
  auto *funcTy = FunctionType::get(boolTy, vecBoolTy, false);
  redFunc = Function::Create(funcTy, GlobalValue::ExternalLinkage, mangledName, &mod);
  redFunc->setDoesNotAccessMemory();
  redFunc->setDoesNotThrow();
  redFunc->setConvergent();
  redFunc->setDoesNotRecurse();
  return redFunc; // TODO add SIMD mapping
}
Function *PlatformInfo::requestMaskReductionFunc(const std::string &name) {
  auto *redFunc = mod.getFunction(name);
  if (redFunc)
    return redFunc;
  auto &context = mod.getContext();
  auto *boolTy = Type::getInt1Ty(context);
  auto *funcTy = FunctionType::get(boolTy, boolTy, false);
  redFunc = Function::Create(funcTy, GlobalValue::ExternalLinkage, name, &mod);
  redFunc->setDoesNotAccessMemory();
  redFunc->setDoesNotThrow();
  redFunc->setConvergent();
  redFunc->setDoesNotRecurse();
  return redFunc; // TODO add SIMD mapping
}

size_t
PlatformInfo::getMaxVectorWidth() const {
  return getMaxVectorBits() / 8;
}

size_t
PlatformInfo::getMaxVectorBits() const {
  return mTTI->getRegisterBitWidth(true);
}

}
