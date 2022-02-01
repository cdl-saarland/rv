//===- sleefLibrary.cpp -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#include <llvm/Support/SourceMgr.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Dominators.h>
#include <llvm/Analysis/PostDominators.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Analysis/ScalarEvolution.h>
#include <llvm/Analysis/MemoryDependenceAnalysis.h>
#include <llvm/Analysis/BranchProbabilityInfo.h>
#include <llvm/Passes/PassBuilder.h>
#include "llvm/Transforms/Utils/LCSSA.h"

#include "rv/PlatformInfo.h"
#include "utils/rvTools.h"
#include "utils/rvLinking.h"
#include "rvConfig.h"
#include "rv/rv.h"
#include "rv/utils.h"
#include "rv/region/FunctionRegion.h"
#include "rv/transform/singleReturnTrans.h"
#include "rv/passes/loopExitCanonicalizer.h"
#include "rv/passes/PassManagerSession.h"
#include "report.h"

#include <llvm/IR/Verifier.h>
#include <vector>
#include <sstream>

#if 1
#define IF_DEBUG_SLEEF IF_DEBUG
#else
#define IF_DEBUG_SLEEF if (true)
#endif


// used for on-demand mappings
//
using namespace llvm;

// vector-length agnostic
extern "C" {

#define EXTERNAL_GENBC_BUFFER(NAME) \
extern const unsigned char * NAME##_Buffer; \
extern const size_t NAME##_BufferLen;

#define EMPTY_GENBC_BUFFER(NAME) \
const unsigned char * NAME##_Buffer = nullptr; \
const size_t NAME##_BufferLen = 0;


#ifdef RV_ENABLE_SLEEF
EXTERNAL_GENBC_BUFFER(rempitab)
EXTERNAL_GENBC_BUFFER(vla_sp)
EXTERNAL_GENBC_BUFFER(vla_dp)
#else
EMPTY_GENBC_BUFFER(rempitab)
EMPTY_GENBC_BUFFER(vla_sp)
EMPTY_GENBC_BUFFER(vla_dp)
#endif

#ifdef RV_ENABLE_ADVSIMD
EXTERNAL_GENBC_BUFFER(advsimd_extras)
EXTERNAL_GENBC_BUFFER(advsimd_sp)
EXTERNAL_GENBC_BUFFER(advsimd_dp)
#else
EMPTY_GENBC_BUFFER(advsimd_extras)
EMPTY_GENBC_BUFFER(advsimd_sp)
EMPTY_GENBC_BUFFER(advsimd_dp)
#endif

#ifdef RV_ENABLE_X86
EXTERNAL_GENBC_BUFFER(avx512_extras)
EXTERNAL_GENBC_BUFFER(avx512_sp)
EXTERNAL_GENBC_BUFFER(avx512_dp)
EXTERNAL_GENBC_BUFFER(avx2_extras)
EXTERNAL_GENBC_BUFFER(avx2_sp)
EXTERNAL_GENBC_BUFFER(avx2_dp)
EXTERNAL_GENBC_BUFFER(avx_sp)
EXTERNAL_GENBC_BUFFER(avx_dp)
EXTERNAL_GENBC_BUFFER(sse_sp)
EXTERNAL_GENBC_BUFFER(sse_dp)

#else
EMPTY_GENBC_BUFFER(avx512_extras)
EMPTY_GENBC_BUFFER(avx512_sp)
EMPTY_GENBC_BUFFER(avx512_dp)
EMPTY_GENBC_BUFFER(avx2_extras)
EMPTY_GENBC_BUFFER(avx2_sp)
EMPTY_GENBC_BUFFER(avx2_dp)
EMPTY_GENBC_BUFFER(avx_sp)
EMPTY_GENBC_BUFFER(avx_dp)
EMPTY_GENBC_BUFFER(sse_sp)
EMPTY_GENBC_BUFFER(sse_dp)
#endif
} // extern "C"

#undef EMPTY_GENBC_BUFFER
#undef EXTERNAL_GENBC_BUFFER

namespace rv {


// internal structures for named mappings (without fancily shaped arguments)
struct PlainVecDesc {
  std::string scalarFnName;
  std::string vectorFnName;
  int vectorWidth;

  PlainVecDesc(std::string _scalarName, std::string _vectorName, int _width)
  : scalarFnName(_scalarName), vectorFnName(_vectorName), vectorWidth(_width)
  {}

  PlainVecDesc()
  : scalarFnName()
  , vectorFnName()
  , vectorWidth(0)
  {}
};
using PlainVecDescVector = std::vector<PlainVecDesc>;

using AddToListFuncType = std::function<void(const PlainVecDescVector&, bool)>;

enum SleefISA {
  SLEEF_VLA  = 0,
  SLEEF_SSE  = 1,
  SLEEF_AVX  = 2,
  SLEEF_AVX2 = 3,
  SLEEF_AVX512 = 4,
  SLEEF_ADVSIMD = 5,
  SLEEF_Enum_Entries = 6
};

inline int sleefModuleIndex(SleefISA isa, bool doublePrecision) {
  return int(isa) + (doublePrecision ? (int) SLEEF_Enum_Entries : 0);
}

static const size_t sleefModuleBufferLens[] = {
    vla_sp_BufferLen,
    sse_sp_BufferLen,
    avx_sp_BufferLen,
    avx2_sp_BufferLen,
    avx512_sp_BufferLen,
    advsimd_sp_BufferLen,

    vla_dp_BufferLen,
    sse_dp_BufferLen,
    avx_dp_BufferLen,
    avx2_dp_BufferLen,
    avx512_dp_BufferLen,
    advsimd_dp_BufferLen,
};

static const unsigned char** sleefModuleBuffers[] = {
    &vla_sp_Buffer,
    &sse_sp_Buffer,
    &avx_sp_Buffer,
    &avx2_sp_Buffer,
    &avx512_sp_Buffer,
    &advsimd_sp_Buffer,

    &vla_dp_Buffer,
    &sse_dp_Buffer,
    &avx_dp_Buffer,
    &avx2_dp_Buffer,
    &avx512_dp_Buffer,
    &advsimd_dp_Buffer,
};

static const size_t extraModuleBufferLens[] = {
    0, // VLA
    0, // SSE
    0, // AVX
    avx2_extras_BufferLen,
    avx512_extras_BufferLen,
    advsimd_extras_BufferLen,
};

static const unsigned char** extraModuleBuffers[] = {
    nullptr, // VLA
    nullptr, // SSE
    nullptr, // AVX
    &avx2_extras_Buffer,
    &avx512_extras_Buffer,
    &advsimd_extras_Buffer,
};



static Module *sleefModules[SLEEF_Enum_Entries * 2];
static Module *extraModules[SLEEF_Enum_Entries * 2];

static Module &requestSharedModule(LLVMContext &Ctx) {
  static Module *SharedModule = nullptr;
  if (!SharedModule)
    SharedModule =
        createModuleFromBuffer(reinterpret_cast<const char *>(&rempitab_Buffer),
                               rempitab_BufferLen, Ctx);
  assert(SharedModule);
  return *SharedModule;
}

const LinkerCallback SharedModuleLookup = [](GlobalValue& GV, Module& M) -> Value * {
  IF_DEBUG_SLEEF {
    errs() << "SharedModuleLookup: " << GV.getName() << "\n";
  }

  // Definition available
  if (!GV.isDeclaration())
    return &GV;

  // Ignore intrinsic decls
  auto *F = dyn_cast<Function>(&GV);
  if (F && F->getIntrinsicID() != Intrinsic::not_intrinsic)
    return nullptr;

  // Lookup symbol in shared 'rempitab' module
  auto &SharedMod = requestSharedModule(M.getContext());
  auto *SharedGV = SharedMod.getNamedValue(GV.getName());
  // N/a in the shared module -> back to the original source.
  if (!SharedGV)
    return nullptr;
  IF_DEBUG_SLEEF {
    errs() << "Pulling shared global: " << SharedGV->getName() << "\n";
  }
  // Don't recurse into lookup
  GlobalVariable * ClonedGV = cast<GlobalVariable>(&cloneGlobalIntoModule(*SharedGV, M, nullptr));
  // Uniquify the 'rempitab' constant globals.
  ClonedGV->setLinkage(GlobalVariable::LinkOnceODRLinkage);
  return ClonedGV;
};

static
void
InitSleefMappings(PlainVecDescVector & archMappings, int floatWidth, int doubleWidth) {
      PlainVecDescVector VecFuncs = {
          {"ilogbf", "xilogbf", floatWidth},
          {"fmaf", "xfmaf", floatWidth},
          {"fabsf", "xfabsf", floatWidth},
          {"copysignf", "xcopysignf", floatWidth},
          {"fmaxf", "xfmaxf", floatWidth},
          {"fminf", "xfminf", floatWidth},
          {"fdimf", "xfdimf", floatWidth},
          {"truncf", "xtruncf", floatWidth},
          {"floorf", "xfloorf", floatWidth},
          {"ceilf", "xceilf", floatWidth},
          {"roundf", "xroundf", floatWidth},
          {"rintf", "xrintf", floatWidth},
          {"nextafterf", "xnextafterf", floatWidth},
          {"frfrexpf", "xfrfrexpf",  floatWidth},
          {"expfrexpf", "xexpfrexpf", floatWidth},
          {"fmodf", "xfmodf", floatWidth},
          {"modff", "xmodff", floatWidth},

          {"ilogb", "xilogb", doubleWidth},
          {"fma", "xfma", doubleWidth},
          {"fabs", "xfabs", doubleWidth},
          {"copysign", "xcopysign", doubleWidth},
          {"fmax", "xfmax", doubleWidth},
          {"fmin", "xfmin", doubleWidth},
          {"fdim", "xfdim", doubleWidth},
          {"trunc", "xtrunc", doubleWidth},
          {"floor", "xfloor", doubleWidth},
          {"ceil", "xceil", doubleWidth},
          {"round", "xround", doubleWidth},
          {"rint", "xrint", doubleWidth},
          {"nextafter", "xnextafter", doubleWidth},
          {"frfrexp", "xfrfrexp", doubleWidth},
          {"expfrexp", "xexpfrexp", doubleWidth},
          {"fmod", "xfmod", doubleWidth},
          {"modf", "xmodf", floatWidth},

          {"llvm.floor.f32", "xfloorf", floatWidth},
          {"llvm.fabs.f32", "xfabsf", floatWidth},
          {"llvm.copysign.f32", "xcopysignf", floatWidth},
          {"llvm.minnum.f32", "xfminf", floatWidth},
          {"llvm.maxnum.f32", "xfmaxf", floatWidth},
          {"llvm.floor.f64", "xfloor", doubleWidth},
          {"llvm.fabs.f64", "xfabs", doubleWidth},
          {"llvm.copysign.f64", "xcopysign", doubleWidth},
          {"llvm.minnum.f64", "xfmin", doubleWidth},
          {"llvm.maxnum.f64", "xfmax", doubleWidth},

#if 0
        // TODO VLA random number generator
        // extras
          {"drand48", "vrand_extra_vla", 2},
          {"frand48", "vrand_extra_vla", 4}
#endif
#define ALSO_FINITE(IRNAME, SLEEFNAME, WIDTH) \
            {#IRNAME, #SLEEFNAME, WIDTH}, \
            {"__" #IRNAME "_finite", #SLEEFNAME, floatWidth}
#define ALSO_FINITE_ULP(IRNAME, ULP, SLEEFNAME, WIDTH) \
            {#IRNAME, #SLEEFNAME, WIDTH}, \
            {"__" #IRNAME "_" #ULP "_finite", #SLEEFNAME, floatWidth}
//            EXPORT CONST vfloat __atan2f_finite    (vfloat, vfloat) __attribute__((weak, alias(str_xatan2f_u1 )));
//            EXPORT CONST vfloat __fmodf_finite     (vfloat, vfloat) __attribute__((weak, alias(str_xfmodf     )));
//            EXPORT CONST vfloat __modff_finite      (vfloat, vfloat *) __attribute__((weak, alias(str_xmodff  )));
//            EXPORT CONST vfloat __hypotf_u05_finite(vfloat, vfloat) __attribute__((weak, alias(str_xhypotf_u05)));

            // TODO: Auto-generate this from some API header/description file.
            ALSO_FINITE(acosf,xacosf, floatWidth),
            ALSO_FINITE(acoshf,xacoshf, floatWidth),
            ALSO_FINITE(acoshf,xacoshf,floatWidth),
            ALSO_FINITE(asinf,xasinf, floatWidth),
            ALSO_FINITE(asinhf, xasinhf, floatWidth),
            ALSO_FINITE(atan2f,xatan2f, floatWidth),
            ALSO_FINITE(atanf,xatanf, floatWidth),
            ALSO_FINITE(atanhf,xatanhf,floatWidth),
            ALSO_FINITE(cbrtf,xcbrtf, floatWidth),
            ALSO_FINITE(cosf,xcosf, floatWidth),
            ALSO_FINITE(coshf, xcoshf, floatWidth),
            ALSO_FINITE(erfcf,xerfcf,    floatWidth),
            ALSO_FINITE(erff,xerff,       floatWidth),
            ALSO_FINITE(exp10f,xexp10f, floatWidth),
            ALSO_FINITE(exp2f,xexp2f, floatWidth),
            ALSO_FINITE(expf,xexpf,floatWidth),
            ALSO_FINITE(expm1f,xexpm1f, floatWidth),
            ALSO_FINITE(log10f,xlog10f,floatWidth),
            ALSO_FINITE(log1pf,xlog1pf,floatWidth),
            ALSO_FINITE(logf,xlogf, floatWidth),
            ALSO_FINITE(powf,xpowf,floatWidth),
            ALSO_FINITE(sinf,xsinf, floatWidth),
            ALSO_FINITE(sinhf,xsinhf, floatWidth),
            ALSO_FINITE(sqrtf,xsqrtf,floatWidth),
            ALSO_FINITE(tanf,xtanf, floatWidth),
            ALSO_FINITE(tanhf, xtanhf, floatWidth),
            ALSO_FINITE_ULP(hypotf,u05,xhypotf,floatWidth),
            ALSO_FINITE_ULP(lgammaf,u1,xlgammaf, floatWidth),
            ALSO_FINITE_ULP(tgammaf,u1,xtgammaf, floatWidth),
#undef ALSO_FINITE
#undef ALSO_FINITE_ULP

            {"sin", "xsin",   doubleWidth},
            {"cos", "xcos",   doubleWidth},
            {"tan", "xtan",   doubleWidth},
            {"asin", "xasin", doubleWidth},
            {"acos", "xacos", doubleWidth},
            {"atan", "xatan", doubleWidth},
            {"atan2", "xatan2", doubleWidth},
            {"log", "xlog", doubleWidth},
            {"cbrt", "xcbrt", doubleWidth},
            {"exp", "xexp", doubleWidth},
            {"pow", "xpow", doubleWidth},
            {"sinh", "xsinh", doubleWidth},
            {"cosh", "xcosh", doubleWidth},
            {"tanh", "xtanh", doubleWidth},
            {"asinh", "xasinh", doubleWidth},
            {"acosh", "xacosh", doubleWidth},
            {"atanh", "xatanh", doubleWidth},
            {"exp2", "xexp2",   doubleWidth},
            {"exp10", "xexp10", doubleWidth},
            {"expm1", "xexpm1", doubleWidth},
            {"log10", "xlog10", doubleWidth},
            {"log1p", "xlog1p", doubleWidth},
            {"sqrt", "xsqrt", doubleWidth},
            {"hypot", "xhypot", doubleWidth},
            {"lgamma", "xlgamma", doubleWidth},
            {"tgamma", "xtgamma", doubleWidth},
            {"erf", "xerf",     doubleWidth},
            {"erfc", "xerfc", doubleWidth},

            {"llvm.sin.f32", "xsinf", floatWidth},
            {"llvm.cos.f32", "xcosf", floatWidth},
            {"llvm.log.f32", "xlogf", floatWidth},
            {"llvm.exp.f32", "xexpf", floatWidth},
            {"llvm.pow.f32", "xpowf", floatWidth},
            {"llvm.sqrt.f32", "xsqrtf", floatWidth},
            {"llvm.exp2.f32", "xexp2f", floatWidth},
            {"llvm.log10.f32", "xlog10f", floatWidth},
            {"llvm.sin.f64", "xsin", doubleWidth},
            {"llvm.cos.f64", "xcos", doubleWidth},
            {"llvm.log.f64", "xlog", doubleWidth},
            {"llvm.exp.f64", "xexp", doubleWidth},
            {"llvm.pow.f64", "xpow", doubleWidth},
            {"llvm.sqrt.f64", "xsqrt", doubleWidth},
            {"llvm.exp2.f64", "xexp2", doubleWidth},
            {"llvm.log10.f64", "xlog10", doubleWidth}
        };
        archMappings.insert(archMappings.end(), VecFuncs.begin(), VecFuncs.end());
}


class SleefResolverService : public ResolverService {
  PlatformInfo & platInfo;

  struct ArchFunctionList {
    SleefISA isaIndex;
    std::string archSuffix;

    PlainVecDescVector commonVectorMappings;

    void addNamedMappings(const PlainVecDescVector & funcs, bool givePrecedence) {
      auto itInsert = givePrecedence ? commonVectorMappings.begin() : commonVectorMappings.end();
      commonVectorMappings.insert(itInsert, funcs.begin(), funcs.end());
    }

    ArchFunctionList(SleefISA _isaIndex, std::string _archSuffix)
    : isaIndex(_isaIndex)
    , archSuffix(_archSuffix)
    {}
  };

  std::vector<ArchFunctionList*> archLists;

  Config config;


public:
  void
  print(llvm::raw_ostream & out) const override {
    out << "SLEEFResolver:\n"
             << "\tarch order: ";

    bool later = false;
    for (const auto * archList : archLists) {
      if (later) { out << ","; }
      later = true;
      out << archList->archSuffix;
    }
  }

  SleefResolverService(PlatformInfo & _platInfo, const Config & _config)
  : platInfo(_platInfo)
  , config(_config)
  {
  // ARM
#ifdef RV_ENABLE_ADVSIMD
    if (config.useADVSIMD) {
      auto * advSimdArch = new ArchFunctionList(SleefISA::SLEEF_ADVSIMD, "advsimd");
      InitSleefMappings(advSimdArch->commonVectorMappings, 4, 2);
      archLists.push_back(advSimdArch);
    }
#endif

  // x86
#ifdef RV_ENABLE_X86
    if (config.useAVX512) {
      auto * avx512Arch = new ArchFunctionList(SleefISA::SLEEF_AVX512, "avx512");
      InitSleefMappings(avx512Arch->commonVectorMappings, 16, 8);
      archLists.push_back(avx512Arch);
    }
    if (config.useAVX2 || config.useAVX512) {
      auto * avx2Arch = new ArchFunctionList(SleefISA::SLEEF_AVX2, "avx2");
      InitSleefMappings(avx2Arch->commonVectorMappings, 8, 4);
      archLists.push_back(avx2Arch);
    }
    if (config.useAVX) {
      auto * avxArch = new ArchFunctionList(SleefISA::SLEEF_AVX, "avx");
      InitSleefMappings(avxArch->commonVectorMappings, 8, 4);
      archLists.push_back(avxArch);
    }
    if (config.useSSE || config.useAVX || config.useAVX2 || config.useAVX512) {
      auto * sseArch = new ArchFunctionList(SleefISA::SLEEF_SSE, "sse");
      InitSleefMappings(sseArch->commonVectorMappings, 4, 2);
      archLists.push_back(sseArch);
    }
#endif

  // generic
    // fall back to automatic vectorization of scalar implementations (baseline)
    auto * vlaArch = new ArchFunctionList(SleefISA::SLEEF_VLA, "vla");
    InitSleefMappings(vlaArch->commonVectorMappings, -1, -1);

    vlaArch->commonVectorMappings.emplace_back("ldexpf", "xldexpf", -1);
    vlaArch->commonVectorMappings.emplace_back("ldexp", "xldexp", -1);

    archLists.push_back(vlaArch);

  }

  ~SleefResolverService() {
    for (auto * archList : archLists) delete archList;
  }

  std::unique_ptr<FunctionResolver> resolve(llvm::StringRef funcName, llvm::FunctionType & scaFuncTy, const VectorShapeVec & argShapes, int vectorWidth, bool hasPredicate, llvm::Module & destModule) override;
};








// simply links-in the pre-vectorized SLEEF function
class SleefLookupResolver : public FunctionResolver {
  VectorShape resShape;
  Function & vecFunc;
  std::string destFuncName;

  public:
    SleefLookupResolver(Module & _targetModule, VectorShape resShape, Function & _vecFunc, std::string _destFuncName)
    : FunctionResolver(_targetModule)
    , resShape(resShape)
    , vecFunc(_vecFunc)
    , destFuncName(_destFuncName)
  {}

  CallPredicateMode getCallSitePredicateMode() override {
    // FIXME this is not entirely true for vector math
    return CallPredicateMode::SafeWithoutPredicate;
  }

  // mask position (if any)
  int getMaskPos() override {
    return -1; // FIXME vector math is unpredicated
  }

  llvm::Function&
  requestVectorized() override {
    auto * existingFunc = targetModule.getFunction(destFuncName);
    if (existingFunc) {
      return *existingFunc;
    }

    // Picks 'Sleef_rempitab' from the shared module.
    Function &clonedVecFunc = cloneFunctionIntoModule(
        vecFunc, targetModule, destFuncName, SharedModuleLookup);
    clonedVecFunc.setDoesNotRecurse(); // SLEEF math does not recurse
    return clonedVecFunc;
  }

  // result shape of function @funcName in target module @module
  VectorShape requestResultShape() override { return resShape; }
};


// on-the-fly vectorizing resolver
struct SleefVLAResolver : public FunctionResolver {
  VectorizerInterface vectorizer;
  std::unique_ptr<VectorizationInfo> vecInfo;

  Function & scaFunc;
  Function * clonedFunc;
  Function * vecFunc;
  VectorShapeVec argShapes;
  VectorShape resShape;
  int vectorWidth;

  std::string vecFuncName;

  SleefVLAResolver(PlatformInfo & platInfo, std::string baseName, Config config, Function & _scaFunc, const VectorShapeVec & _argShapes, int _vectorWidth)
  : FunctionResolver(platInfo.getModule())
  , vectorizer(platInfo, config)
  , vecInfo(nullptr)
  , scaFunc(_scaFunc)
  , clonedFunc(nullptr)
  , vecFunc(nullptr)
  , argShapes(_argShapes)
  , resShape(VectorShape::undef())
  , vectorWidth(_vectorWidth)
  , vecFuncName(platInfo.createMangledVectorName(baseName, argShapes, vectorWidth, -1))
  {
    IF_DEBUG_SLEEF { errs() << "VLA: " << vecFuncName << "\n"; }
  }

  CallPredicateMode getCallSitePredicateMode() override {
    // FIXME this is not entirely true for vector math
    return CallPredicateMode::SafeWithoutPredicate;
  }

  // mask position (if any)
  int getMaskPos() override {
    return -1; // FIXME vector math is unpredicated
  }

  // materialized the vectorized function in the module @insertInto and returns a reference to it
  llvm::Function& requestVectorized() override {
    if (vecFunc) return *vecFunc;
    vecFunc = targetModule.getFunction(vecFuncName);
    if (vecFunc) return *vecFunc;

    // FIXME this is a hacky workaround for sqrt
    if (scaFunc.getName().startswith("xsqrt")) {
      auto funcTy = scaFunc.getFunctionType();
      auto vecTy = FixedVectorType::get(funcTy->getReturnType(), vectorWidth);
      vecFunc = Intrinsic::getDeclaration(&targetModule, Intrinsic::sqrt, {vecTy});
      return *vecFunc;
    }

    requestResultShape();

    // prepare scalar copy for transforming
    clonedFunc = &cloneFunctionIntoModule(
        scaFunc, targetModule, vecFuncName + ".tmp", SharedModuleLookup);
    assert(clonedFunc);

    // create SIMD declaration
    const int maskPos = -1; // TODO add support for masking
    vecFunc = createVectorDeclaration(*clonedFunc, resShape, argShapes, vectorWidth, maskPos);
    vecFunc->setName(vecFuncName);

    // override with no-recurse flag (so we won't get guards in the vector code)
    vecFunc->copyAttributesFrom(&scaFunc);
    vecFunc->setDoesNotRecurse();

    // Use fastest possible CC.
    vecFunc->setCallingConv(CallingConv::Fast);
    vecFunc->setLinkage(GlobalValue::LinkOnceAnyLinkage);

    VectorMapping mapping(clonedFunc, vecFunc, vectorWidth, maskPos, resShape, argShapes, CallPredicateMode::SafeWithoutPredicate);
    vectorizer.getPlatformInfo().addMapping(mapping); // prevent recursive vectorization

    // set-up vecInfo
    FunctionRegion funcWrapper(*clonedFunc);
    Region funcRegion(funcWrapper);
    VectorizationInfo vecInfo(funcRegion, mapping);

    // unify returns (if necessary)
    SingleReturnTrans::run(funcRegion);

    // compute anlaysis results
    PassManagerSession PMS;

    // compute DT, PDT, LI
    PMS.FAM.getResult<DominatorTreeAnalysis>(*clonedFunc);
    PMS.FAM.getResult<PostDominatorTreeAnalysis>(*clonedFunc);
    PMS.FAM.getResult<LoopAnalysis>(*clonedFunc);
    PMS.FAM.getResult<ScalarEvolutionAnalysis>(*clonedFunc);
    PMS.FAM.getResult<MemoryDependenceAnalysis>(*clonedFunc);
    PMS.FAM.getResult<BranchProbabilityAnalysis>(*clonedFunc);

    // re-establish LCSSA
    FunctionPassManager FPM;
    FPM.addPass<LCSSAPass>(LCSSAPass());
    FPM.run(*clonedFunc, PMS.FAM);

    // normalize loop exits (TODO make divLoopTrans work without this)
    {
      LoopInfo &LI = PMS.FAM.getResult<LoopAnalysis>(*clonedFunc);
      LoopExitCanonicalizer canonicalizer(LI);
      canonicalizer.canonicalize(*clonedFunc);
      auto PA = PreservedAnalyses::all();
      PA.abandon<DominatorTreeAnalysis>();
      PA.abandon<PostDominatorTreeAnalysis>();
      // invalidate & recompute LI
      PA.abandon<LoopAnalysis>();
      PMS.FAM.invalidate(*clonedFunc, PA);
      PMS.FAM.getResult<LoopAnalysis>(*clonedFunc);
    }

    // run pipeline
    vectorizer.analyze(vecInfo, PMS.FAM);
    vectorizer.linearize(vecInfo, PMS.FAM);
    vectorizer.vectorize(vecInfo, PMS.FAM, nullptr);
    vectorizer.finalize();

    // discard temporary mapping
    vectorizer.getPlatformInfo().forgetAllMappingsFor(*clonedFunc);
    // can dispose of temporary function now
    clonedFunc->eraseFromParent();

    return *vecFunc;
  }

  // result shape of function @funcName in target module @module
  VectorShape requestResultShape() override {
    if (resShape.isDefined()) return resShape;

    // TODO run VA
    for (const auto & argShape : argShapes) {
      if (!argShape.isUniform()) {
        resShape = VectorShape::varying();
        return resShape;
      }
    }
    resShape = VectorShape::uni();
    return resShape;
  }

  // whether this resolver can provide a vectorized version ofthis function
  bool isVectorizable(const VectorShapeVec & argShapes, int vectorWidth) {
    return true;
  }
};

// used for shape-based call mappings
using VecMappingShortVec = llvm::SmallVector<VectorMapping, 4>;
using VectorFuncMap = std::map<const llvm::Function *, VecMappingShortVec*>;

// parse ULP error bound from mangled SLEEF name
static unsigned
ReadULPBound(StringRef sleefName) {
  auto itStart = sleefName.find_last_of("_u");
  if (itStart == StringRef::npos) return 0; // unspecified -> perfect rounding
  StringRef ulpPart = sleefName.substr(itStart + 1);

  // single digit ULP value
  unsigned ulpBound;
  if (ulpPart.size() == 1) {
    ulpBound = 10 * (ulpPart[0] - '0');

  // lsc is tenth of ULP
  } else {
    bool parseError = ulpPart.consumeInteger<unsigned>(10, ulpBound);
    (void) parseError; assert(!parseError);
  }

  return ulpBound;
}

static Function*
GetLeastPreciseImpl(Module & mod, const std::string & funcPrefix, const unsigned maxULPBound) {
  Function * currBest = nullptr;
  unsigned bestBound = 0;

  IF_DEBUG_SLEEF { errs() << "SLEEF: impl: " << funcPrefix << "\n"; }
  for (auto & func : mod) {
    if (func.isDeclaration()) continue;
    if (!func.getName().startswith(funcPrefix)) continue;

    // not a complete funcname match (ie "xlog" would otw match "xlog1p")
    if ((func.getName().size() > funcPrefix.size()) &&
        (func.getName()[funcPrefix.size()] != '_'))
    {
        continue;
    }

    IF_DEBUG_SLEEF { errs() << "\t candidate: " << func.getName() << "\n"; }

    unsigned funcBound = ReadULPBound(func.getName());
    // dismiss too imprecise functions
    if (funcBound > maxULPBound) {
      IF_DEBUG_SLEEF { errs() << "discard, ulp was: " << funcBound << "\n"; }
      continue;

    // accept functions with higher ULP error within maxUPLBound
    } else if (!currBest || (funcBound > bestBound)) {
      IF_DEBUG_SLEEF { errs() << "\tOK! " << func.getName() << " with ulp bound: " << funcBound << "\n"; }
      bestBound = funcBound;
      currBest = &func;
    }
  }

  return currBest;
}

std::unique_ptr<FunctionResolver>
SleefResolverService::resolve(llvm::StringRef funcName, llvm::FunctionType & scaFuncTy, const VectorShapeVec & argShapes, int vectorWidth, bool hasPredicate, llvm::Module & destModule) {
  IF_DEBUG_SLEEF { errs() << "SLEEFResolverService: " << funcName << " for width " << vectorWidth << "\n"; }

  (void) hasPredicate; // FIXME use predicated versions

  // Otw, start looking for a SIMD-ized implementation
  ArchFunctionList * archList = nullptr;
  PlainVecDesc funcDesc;
  for (auto * candList : archLists) {
    // query custom mappings with precedence
    std::string funcNameStr = funcName.str();
    for (const auto & vd : candList->commonVectorMappings) {
       if (vd.scalarFnName == funcNameStr &&
          ((vd.vectorWidth <= 0) || (vd.vectorWidth == vectorWidth)))
       {
         funcDesc = vd;
         archList = candList;
         break;
       }
    };

    if (archList) break;
  }
  IF_DEBUG_SLEEF { errs() << "\tsleef: n/a\n"; }
  if (!archList) return nullptr;

  // Make sure the shared module is available
  auto & Ctx = destModule.getContext();

  // decode bitwidth (for module lookup)
  bool doublePrecision = false;
  for (const auto * argTy : scaFuncTy.params()) {
    doublePrecision |= argTy->isDoubleTy();
  }


  // remove the trailing isa specifier (_avx2/_avx/_sse/..)
  SleefISA isa = archList->isaIndex;

  std::string sleefName = funcDesc.vectorFnName;

  // TODO factor out
  bool isExtraFunc = funcDesc.vectorFnName.find("_extra") != std::string::npos;
  if (isExtraFunc) {
    int modIdx = (int) isa;
    auto *& mod = extraModules[modIdx];
    if (!mod) mod = createModuleFromBuffer(reinterpret_cast<const char*>(extraModuleBuffers[modIdx]), extraModuleBufferLens[modIdx], Ctx);
    Function *vecFunc = mod->getFunction(sleefName);
    assert(vecFunc && "mapped extra function not found in module!");
    return std::make_unique<SleefLookupResolver>(destModule, /* RNG result */ VectorShape::varying(), *vecFunc, funcDesc.vectorFnName);
  }

  // Skip for fast builtin functions // FIXME should be in its own target-dependent resolver
  if (StringRef(sleefName).startswith("xsqrt")) {
    // "every" target has a sqrt instruction of sorts
    auto vecTy = FixedVectorType::get(scaFuncTy.getReturnType(), vectorWidth);
    auto vecFunc = Intrinsic::getDeclaration(&destModule, Intrinsic::sqrt, {vecTy});

    // FIXME abusing the SleefLookupResolver to retrieve an existing declaration...
    return std::make_unique<SleefLookupResolver>(destModule, VectorShape::varying(), *vecFunc, vecFunc->getName().str());
  }

  // Look in SLEEF module
  auto modIndex = sleefModuleIndex(isa, doublePrecision);
  llvm::Module*& mod = sleefModules[modIndex]; // TODO const Module
  if (!mod) {
    mod = createModuleFromBuffer(reinterpret_cast<const char*>(sleefModuleBuffers[modIndex]), sleefModuleBufferLens[modIndex], Ctx);

    IF_DEBUG {
      bool brokenMod = verifyModule(*mod, &errs());
      if (brokenMod) abort();
    }
  }

  if (isa == SLEEF_VLA) {
    // on-the-fly vectorization module
    Function *vlaFunc = GetLeastPreciseImpl(*mod, sleefName, config.maxULPErrorBound);
    if (!vlaFunc) {
      IF_DEBUG_SLEEF { errs() << "sleef: " << sleefName << " n/a with maxULPError: " << config.maxULPErrorBound << "\n"; }
      return nullptr;
    }

    return std::make_unique<SleefVLAResolver>(platInfo, vlaFunc->getName().str(), config, *vlaFunc, argShapes, vectorWidth);

  } else {
    // these are pure functions
    VectorShape resShape = VectorShape::uni();
    for (const auto argShape : argShapes) {
      if (!argShape.isUniform()) {
        resShape = VectorShape::varying();
        break;
      }
    }

    // we'll have to link in the function
    Function *vecFunc = GetLeastPreciseImpl(*mod, sleefName, config.maxULPErrorBound);
    if (!vecFunc) {
      IF_DEBUG_SLEEF { errs() << "sleef: " << sleefName << " n/a with maxULPError: " << config.maxULPErrorBound << "\n"; }
      return nullptr;
    }

    std::string vecFuncName = vecFunc->getName().str() + "_" + archList->archSuffix;
    return std::make_unique<SleefLookupResolver>(destModule, resShape, *vecFunc, vecFuncName);
  }
}



void
addSleefResolver(const Config & config, PlatformInfo & platInfo) {
#ifdef RV_ENABLE_SLEEF
  auto sleefRes = std::make_unique<SleefResolverService>(platInfo, config);
  platInfo.addResolverService(std::move(sleefRes), false); // give precedence to VectorABI
#else
  Report() << " build w/o SLEEF (tried to add SleefResolver)!\n";
#endif
}

} // namespace rv
