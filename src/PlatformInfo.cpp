//===- src/PlatformInfo.cpp - target info&function environment --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//


#include "rv/PlatformInfo.h"
#include "rv/resolver/listResolver.h"
#include "rv/intrinsics.h"

#include "utils/rvTools.h"
#include "rv/utils.h"

#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/TypeSize.h>
#include <llvm/ADT/Triple.h>

#include "rvConfig.h"

#include <sstream>

using namespace llvm;

#if 0
#define IF_DEBUG_PLAT if (true)
#else
#define IF_DEBUG_PLAT IF_DEBUG
#endif

namespace rv {

static bool
GetBuiltinMapping(Function & F, VectorMapping & KnownMapping) {
  if (F.getName() == "_ZdlPv") {
    // operator delete - never replicate for the same base ptr (despite its side effects)
    KnownMapping = VectorMapping(
      &F,
      &F,
      0, // no specific vector width
      -1, //
      VectorShape::uni(),
      {VectorShape::uni()},
      CallPredicateMode::Unpredicated
    );
    return true;
  }

  // TODO free
  return false;
}


void
PlatformInfo::registerDeclareSIMDFunction(Function & F) {
  auto attribSet = F.getAttributes().getFnAttrs();
  // parse SIMD signatures
  std::vector<VectorMapping> wfvJobs;
  for (auto attrib : attribSet) {
    if (!attrib.isStringAttribute()) continue;
    StringRef attribText = attrib.getKindAsString();

    if (attribText.size() < 2) continue;

    VectorMapping vecMapping;
    if (!parseVectorMapping(F, attribText, vecMapping, true)) continue;
    addMapping(std::move(vecMapping));
  }
}

void
PlatformInfo::addMapping(VectorMapping&& mapping) { listResolver->addMapping(std::move(mapping)); }

void
PlatformInfo::addIntrinsicMappings() {
  //
  for (Function & func : getModule()) {
    // Add mappings for known C++ ABI / stdlib functions (free)
    VectorMapping builtinMapping;
    if (GetBuiltinMapping(func, builtinMapping)) {
      addMapping(std::move(builtinMapping));
      continue;
    }

    // is this an RV intrinsic?
    RVIntrinsic id = GetIntrinsicID(func);
    if (id == RVIntrinsic::Unknown) continue;
    auto vecMapping = GetIntrinsicMapping(func, id);
    addMapping(std::move(vecMapping));
  }
}

PlatformInfo::PlatformInfo(Module &_mod, TargetTransformInfo *TTI,
                           TargetLibraryInfo *TLI)
: mod(_mod)
, mTTI(TTI)
, mTLI(TLI)
, resolverServices()
, listResolver(nullptr)
{
  resolverServices.push_back(std::unique_ptr<ResolverService>(new ListResolver(mod)));
  listResolver = static_cast<ListResolver*>(&*resolverServices[0]);

  // add Rv intrinsic mappings
  addIntrinsicMappings();

  // register OpenMP "pragma omp declare simd" functions
  for (auto & F : mod) {
    registerDeclareSIMDFunction(F);
  }
}

PlatformInfo::~PlatformInfo() {}

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
                          int vectorWidth,
                          bool hasPredicate) const {
  IF_DEBUG_PLAT {
    errs() << "Resolver query:\n"
           << "scaName:  " << funcName << "\n"
           << "width:    " << vectorWidth << "\n"
           << "pred:     " << hasPredicate << "\n"
           << "argShapes:";
    for (auto argShape : argShapes) errs() << ", " << argShape.str();
    errs() << "\n";
  }

  for (const auto & resolver : resolverServices) {
    std::unique_ptr<FunctionResolver> funcResolver = resolver->resolve(funcName, scaFuncTy, argShapes, vectorWidth, hasPredicate, mod);
    if (funcResolver) return funcResolver;
  }
  return nullptr;
}

llvm::Function &
PlatformInfo::requestRVIntrinsicFunc(RVIntrinsic rvIntrin) {
  auto * func = mod.getFunction(GetIntrinsicName(rvIntrin));
  if (func) return *func;

  // create a legal declaration
  func = &DeclareIntrinsic(rvIntrin, mod);

  // add VA mappings
  auto vecMapping = GetIntrinsicMapping(*func, rvIntrin);
  addMapping(vecMapping);
  return *func;
}


Function*
PlatformInfo::requestVectorMaskReductionFunc(const std::string &name, size_t width) {
  std::string mangledName = name + "_v" + std::to_string(width);
  auto *redFunc = mod.getFunction(mangledName);
  if (redFunc)
    return redFunc;
  auto &context = mod.getContext();
  auto *boolTy = Type::getInt1Ty(context);
  auto *vecBoolTy = FixedVectorType::get(boolTy, width);
  auto *funcTy = FunctionType::get(boolTy, vecBoolTy, false);
  redFunc = Function::Create(funcTy, GlobalValue::ExternalLinkage, mangledName, &mod);
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

size_t PlatformInfo::getMaxVectorBits() const {
  // Bypass TTI for VE since we want to enable RV vectorization while disabling
  // LLVM vectorization via TTI.
  llvm::Triple Triple(getModule().getTargetTriple());
  if (Triple.getArch() == Triple::ve)
    return 256 * 64;
  return mTTI->getRegisterBitWidth(TargetTransformInfo::RGK_FixedWidthVector);
}

void
PlatformInfo::dump() const {
  print(llvm::errs());
}


bool
PlatformInfo::forgetMapping(const VectorMapping & mapping) { return listResolver->forgetMapping(mapping); }

void
PlatformInfo::forgetAllMappingsFor(const Function & scaFunc) { listResolver->forgetAllMappingsFor(scaFunc); }

void
PlatformInfo::print(llvm::raw_ostream & out) const {
  out << "PlatformInfo {\n";
  out << "Resolvers: [\n";
  for (const auto & resService : resolverServices) {
    resService->print(out);
    out << "\n";
  }
  out << "] }\n";
}

std::string
PlatformInfo::createMangledVectorName(StringRef scalarName, const VectorShapeVec & argShapes, int vectorWidth, int maskPos) {
  // TODO use VectorABI mangling when aplicable

  std::stringstream ss;
  ss
    << scalarName.str()
    << "_v" << vectorWidth << "_";

  // mangle mask position (if defined)
  if (maskPos >= 0) {
    ss << "M" << maskPos;
  } else {
    ss << "N";
  }

  ss << "_";

  // attach arg shapes
  for (const auto & argShape : argShapes) {
    ss << argShape.serialize();
  }

  return ss.str();
}


llvm::Function &
PlatformInfo::requestIntrinsic(RVIntrinsic id, llvm::Type * DataTy) {
  std::string MangledName = GetIntrinsicName(id, DataTy);
  Function *F = mod.getFunction(MangledName);
  if (F) return *F;

  // declare and register with platInfo for this module
  F = &DeclareIntrinsic(id, mod, DataTy);
  auto vecMapping = GetIntrinsicMapping(*F, id);
  addMapping(std::move(vecMapping));
  return *F;
  
}

}
