//
// Created by thorsten on 06.10.16.
//

#include "rv/PlatformInfo.h"
#include "rv/resolver/listResolver.h"
#include "rv/intrinsics.h"

#include "utils/rvTools.h"
#include "rv/utils.h"

#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/Support/raw_ostream.h>

#include "rvConfig.h"

using namespace llvm;

namespace rv {

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
    addMapping(std::move(vecMapping));
  }
}

void
PlatformInfo::addMapping(VectorMapping&& mapping) { listResolver->addMapping(std::move(mapping)); }

void
PlatformInfo::addIntrinsicMappings() {
  for (Function & func : getModule()) {
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
  auto *vecBoolTy = VectorType::get(boolTy, width);
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

size_t
PlatformInfo::getMaxVectorBits() const {
  return mTTI->getRegisterBitWidth(true);
}

void
PlatformInfo::dump() const {
  print(llvm::errs());
}


void
PlatformInfo::forgetMapping(const VectorMapping & mapping) { listResolver->forgetMapping(mapping); }

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

}
