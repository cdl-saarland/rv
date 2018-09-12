//
// Created by thorsten on 06.10.16.
//

#include "rv/PlatformInfo.h"
#include "rv/sleefLibrary.h"

#include "utils/rvTools.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "rv/utils.h"

#include "rvConfig.h"

using namespace llvm;

namespace rv {

// result shape of function @funcName in target module @module
VectorShape
FunctionResolver::ComputeShape(const VectorShapeVec & argShapes) {
  // TODO run VA
  for (const auto & argShape : argShapes) {
    if (!argShape.isUniform()) return VectorShape::varying();
  }
  return VectorShape::uni();
}

class TLIFuncResolver : public FunctionResolver {
  TargetLibraryInfo & TLI;
  llvm::StringRef funcName;
  llvm::FunctionType & scaFuncTy;
  int vectorWidth;

public:
  TLIFuncResolver(Module & _destModule, TargetLibraryInfo & _TLI, llvm::StringRef _funcName, llvm::FunctionType & _scaFuncTy, int _vectorWidth)
  : FunctionResolver(_destModule)
  , TLI(_TLI)
  , funcName(_funcName)
  , scaFuncTy(_scaFuncTy)
  , vectorWidth(_vectorWidth)
  {}

  VectorShape
  requestResultShape() { return VectorShape::varying(); }

  Function& requestVectorized() {
    // TODO actually emit a SIMD declaration for this function
    StringRef tliFnName = TLI.getVectorizedFunction(funcName, vectorWidth);
    return *targetModule.getFunction(tliFnName);
  }
};

class TLIResolverService : public ResolverService {
  TargetLibraryInfo & TLI;

public:
  TLIResolverService(TargetLibraryInfo & _TLI)
  : TLI(_TLI)
  {}

  std::unique_ptr<FunctionResolver>
  resolve(llvm::StringRef funcName, llvm::FunctionType & scaFuncTy, const VectorShapeVec & argShapes, int vectorWidth, llvm::Module & destModule) {
    StringRef tliFnName = TLI.getVectorizedFunction(funcName, vectorWidth);
    if (!tliFnName.empty()) {
      return std::make_unique<TLIFuncResolver>(destModule, TLI, funcName, scaFuncTy, vectorWidth);
    }
    return nullptr;
  }
};

void
PlatformInfo::registerDeclareSIMDFunction(Function & F) {
  auto attribSet = F.getAttributes().getFnAttributes();
  // parse SIMD signatures
  std::vector<VectorMapping> wfvJobs;
  for (auto attrib : attribSet) {
    if (!attrib.isStringAttribute()) continue;
    StringRef attribText = attrib.getKindAsString();

    if (attribText.size() < 2) continue;

    VectorMapping vecMapping;
    if (!parseVectorMapping(F, attribText, vecMapping, false)) continue;
    addMapping(vecMapping);
  }

}

PlatformInfo::PlatformInfo(Module &_mod, TargetTransformInfo *TTI,
                           TargetLibraryInfo *TLI)
    : mod(_mod), mTTI(TTI), mTLI(TLI)
{
  for (auto & F : mod) {
    registerDeclareSIMDFunction(F);
  }
}

PlatformInfo::~PlatformInfo() {
  for (auto it : funcMappings) {
    delete it.second;
  }
}

void PlatformInfo::setTTI(TargetTransformInfo *TTI) { mTTI = TTI; }

void PlatformInfo::setTLI(TargetLibraryInfo *TLI) { mTLI = TLI; }

TargetTransformInfo *PlatformInfo::getTTI() { return mTTI; }

TargetLibraryInfo *PlatformInfo::getTLI() { return mTLI; }

void
PlatformInfo::addResolverService(std::unique_ptr<ResolverService>&& newResolver, bool givePrecedence) {
  auto itInsert = givePrecedence ? resolverServices.begin() : resolverServices.end();
  resolverServices.insert(itInsert, std::move(newResolver));
}

std::unique_ptr<FunctionResolver>
PlatformInfo::getResolver(StringRef funcName,
                          FunctionType & scaFuncTy,
                          const VectorShapeVec & argShapes,
                          int vectorWidth) const {
  for (const auto & resolver : resolverServices) {
    std::unique_ptr<FunctionResolver> funcResolver = resolver->resolve(funcName, scaFuncTy, argShapes, vectorWidth, mod);
    if (funcResolver) return funcResolver;
  }
  return nullptr;
}




// shape based mappings
bool
PlatformInfo::addMapping(rv::VectorMapping &mapping) {
  auto it = funcMappings.find(mapping.scalarFn);
  VecMappingShortVec * vecMappings = nullptr;
  if (it == funcMappings.end()) {
    vecMappings = new VecMappingShortVec();
    funcMappings[mapping.scalarFn] = vecMappings;
  } else {
    vecMappings = it->second;
  }

  // check if there is an equivalent mapping already
  for (auto & knownMapping : *vecMappings) {
    if (knownMapping == mapping) return false;
  }

  vecMappings->push_back(mapping);
  return true;
}


// query available vector mappings for a given vector call signature
bool
PlatformInfo::getMappingsForCall(VecMappingShortVec & matchVec, const llvm::Function & scalarFn, const VectorShapeVec & argShapes, unsigned vectorWidth, bool needsPredication) {
// register user shapes
  auto it = funcMappings.find(&scalarFn);
  if (it == funcMappings.end()) return false;
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

    matchVec.push_back(mapping);
  }
  return matchVec.size() > 0;
}

VectorMapping
PlatformInfo::inferMapping(llvm::Function &scalarFnc,
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
    if (maskPos >= 0 && (i == (unsigned)maskPos)) {
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
