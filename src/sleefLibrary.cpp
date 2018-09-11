//===- sleefLibrary.cpp -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.

#include "rv/sleefLibrary.h"

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

#include "rv/PlatformInfo.h"
#include "utils/rvTools.h"
#include "rvConfig.h"
#include "rv/rv.h"
#include "rv/utils.h"
#include "rv/region/FunctionRegion.h"
#include "rv/transform/singleReturnTrans.h"
#include "rv/transform/loopExitCanonicalizer.h"
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

extern const unsigned char * vla_sp_Buffer;
extern const size_t vla_sp_BufferLen;

extern const unsigned char * vla_dp_Buffer;
extern const size_t vla_dp_BufferLen;

#ifdef RV_ENABLE_ADVSIMD
extern const unsigned char * advsimd_sp_Buffer;
extern const size_t advsimd_sp_BufferLen;

extern const unsigned char * advsimd_dp_Buffer;
extern const size_t advsimd_dp_BufferLen;

extern const unsigned char * advsimd_extras_Buffer;
extern const size_t advsimd_extras_BufferLen;

#else
const unsigned char * advsimd_sp_Buffer = nullptr;
const size_t advsimd_sp_BufferLen = 0;

const unsigned char * advsimd_dp_Buffer = nullptr;
const size_t advsimd_dp_BufferLen = 0;

const unsigned char * advsimd_extras_Buffer = nullptr;
const size_t advsimd_extras_BufferLen = 0;
#endif

#ifdef RV_ENABLE_X86
extern const unsigned char * avx512_sp_Buffer;
extern const size_t avx512_sp_BufferLen;

extern const unsigned char * avx2_sp_Buffer;
extern const size_t avx2_sp_BufferLen;

extern const unsigned char * avx_sp_Buffer;
extern const size_t avx_sp_BufferLen;

extern const unsigned char * sse_sp_Buffer;
extern const size_t sse_sp_BufferLen;

extern const unsigned char * avx512_dp_Buffer;
extern const size_t avx512_dp_BufferLen;

extern const unsigned char * avx2_dp_Buffer;
extern const size_t avx2_dp_BufferLen;

extern const unsigned char * avx_dp_Buffer;
extern const size_t avx_dp_BufferLen;

extern const unsigned char * sse_dp_Buffer;
extern const size_t sse_dp_BufferLen;

extern const unsigned char * avx2_extras_Buffer;
extern const size_t avx2_extras_BufferLen;

extern const unsigned char * avx512_extras_Buffer;
extern const size_t avx512_extras_BufferLen;

#else
const unsigned char * avx512_sp_Buffer = nullptr;
const size_t avx512_sp_BufferLen = 0;

const unsigned char * avx2_sp_Buffer = nullptr;
const size_t avx2_sp_BufferLen = 0;

const unsigned char * avx_sp_Buffer = nullptr;
const size_t avx_sp_BufferLen = 0;

const unsigned char * sse_sp_Buffer = nullptr;
const size_t sse_sp_BufferLen = 0;

const unsigned char * avx512_dp_Buffer = nullptr;
const size_t avx512_dp_BufferLen = 0;

const unsigned char * avx2_dp_Buffer = nullptr;
const size_t avx2_dp_BufferLen = 0;

const unsigned char * avx_dp_Buffer = nullptr;
const size_t avx_dp_BufferLen = 0;

const unsigned char * sse_dp_Buffer = nullptr;
const size_t sse_dp_BufferLen = 0;

const unsigned char * avx2_extras_Buffer = nullptr;
const size_t avx2_extras_BufferLen = 0;

const unsigned char * avx512_extras_Buffer = nullptr;
const size_t avx512_extras_BufferLen = 0;
#endif

#ifdef RV_ENABLE_CRT
extern const unsigned char * crt_Buffer;
extern const size_t crt_BufferLen;
#endif

} // extern "C"
namespace rv {

  // forward decls
  GlobalValue & cloneGlobalIntoModule(GlobalValue &gv, Module &cloneInto);
  Constant & cloneConstant(Constant& constVal, Module & cloneInto);
  Function &cloneFunctionIntoModule(Function &func, Module &cloneInto, StringRef name);




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

#ifdef RV_ENABLE_CRT
  static Module* scalarModule; // scalar implementations to be inlined
#endif

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

          {"llvm.fabs.f32", "xfabsf", floatWidth},
          {"llvm.copysign.f32", "xcopysignf", floatWidth},
          {"llvm.minnum.f32", "xfminf", floatWidth},
          {"llvm.maxnum.f32", "xfmaxf", floatWidth},
          {"llvm.fabs.f64", "xfabs_vla", doubleWidth},
          {"llvm.copysign.f64", "xcopysign", doubleWidth},
          {"llvm.minnum.f64", "xfmin", doubleWidth},
          {"llvm.maxnum.f64", "xfmax", doubleWidth},

#if 0
        // TODO VLA random number generator
        // extras
          {"drand48", "vrand_extra_vla", 2},
          {"frand48", "vrand_extra_vla", 4}
#endif
            {"sinf", "xsinf", floatWidth},
            {"cosf", "xcosf", floatWidth},
            {"tanf", "xtanf", floatWidth},
            {"asinf", "xasinf", floatWidth},
            {"acosf", "xacosf", floatWidth},
            {"atanf", "xatanf", floatWidth},
            {"atan2f", "xatan2f", floatWidth},
            {"logf", "xlogf", floatWidth},
            {"cbrtf", "xcbrtf", floatWidth},
            {"expf", "xexpf", floatWidth},
            {"powf", "xpowf", floatWidth},
            {"sinhf", "xsinhf", floatWidth},
            {"coshf", "xcoshf", floatWidth},
            {"tanhf", "xtanhf", floatWidth},
            {"asinhf", "xasinhf", floatWidth},
            {"acoshf", "xacoshf", floatWidth},
            {"atanhf", "xatanhf", floatWidth},
            {"exp2f", "xexp2f", floatWidth},
            {"exp10f", "xexp10f", floatWidth},
            {"expm1f", "xexpm1f", floatWidth},
            {"log10f", "xlog10f", floatWidth},
            {"log1pf", "xlog1pf", floatWidth},
            {"sqrtf", "xsqrtf", floatWidth},
            {"hypotf", "xhypotf",  floatWidth},
            {"lgammaf", "xlgammaf", floatWidth},
            {"tgammaf", "xtgammaf", floatWidth},
            {"erff", "xerff",       floatWidth},
            {"erfcf", "xerfcf",    floatWidth},

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
  const unsigned maxULPError;

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
  void reportConfig() const {
    Report() << "SLEEFResolver:\n"
             << "\tULP error bound is " << (maxULPError / 10) << '.' << (maxULPError % 10) << "\n"
             << "\tarch order: ";

    bool later = false;
    for (const auto * archList : archLists) {
      if (later) { ReportContinue() << ","; }
      later = true;
      ReportContinue() << archList->archSuffix;
    }
    ReportContinue() << "\n";
  }

  SleefResolverService(PlatformInfo & _platInfo, const Config & _config, unsigned _maxULPError)
  : platInfo(_platInfo)
  , maxULPError(_maxULPError)
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

  std::unique_ptr<FunctionResolver> resolve(llvm::StringRef funcName, llvm::FunctionType & scaFuncTy, const VectorShapeVec & argShapes, int vectorWidth, llvm::Module & destModule);
};









// existing vectorized function wrapper
class ExistingResolver : public FunctionResolver {
  Function & vecFunc;
  VectorShape retShape;

public:
  ExistingResolver(Module & destModule, Function & _vecFunc, VectorShape _retShape)
  : FunctionResolver(destModule)
  , vecFunc(_vecFunc)
  , retShape(_retShape)
  {}

  llvm::Function& requestVectorized() {
    return vecFunc;
  }

  // result shape of function @funcName in target module @module
  VectorShape requestResultShape() {
    return VectorShape::varying();
  }
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

  llvm::Function&
  requestVectorized() {
    auto * existingFunc = targetModule.getFunction(destFuncName);
    if (existingFunc) return *existingFunc;
    return cloneFunctionIntoModule(vecFunc, targetModule, destFuncName);
  }

  // result shape of function @funcName in target module @module
  VectorShape requestResultShape() { return resShape; }
};


static
std::string
MangleFunction(StringRef sleefName, const VectorShapeVec & argShapes, int vectorWidth) {
  std::stringstream ss;
  ss << sleefName.str() << "_v" << vectorWidth << "_";
  for (const auto & argShape : argShapes) {
    ss << argShape.serialize();
  }
  return ss.str();
}

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

  SleefVLAResolver(PlatformInfo & platInfo, StringRef baseName, Config config, Function & _scaFunc, const VectorShapeVec & _argShapes, int _vectorWidth)
  : FunctionResolver(platInfo.getModule())
  , vectorizer(platInfo, config)
  , vecInfo(nullptr)
  , scaFunc(_scaFunc)
  , clonedFunc(nullptr)
  , vecFunc(nullptr)
  , argShapes(_argShapes)
  , resShape(VectorShape::undef())
  , vectorWidth(_vectorWidth)
  , vecFuncName(MangleFunction(baseName, argShapes, vectorWidth))
  {
    IF_DEBUG_SLEEF { errs() << "VLA: " << vecFuncName << "\n"; }
  }

  // materialized the vectorized function in the module @insertInto and returns a reference to it
  llvm::Function& requestVectorized() {
    if (vecFunc) return *vecFunc;
    vecFunc = targetModule.getFunction(vecFuncName);
    if (vecFunc) return *vecFunc;

    // need a proper res shape
    requestResultShape();

    // prepare scalar copy for transforming
    const int maskPos = -1; // TODO add support for masking
    clonedFunc = &cloneFunctionIntoModule(scaFunc, targetModule, vecFuncName + ".tmp");
    assert(clonedFunc);

    // create SIMD declaration
    vecFunc = createVectorDeclaration(*clonedFunc, resShape, argShapes, vectorWidth);
    // TODO vecFunc->copyAttributesFrom(callerFunc);
    vecFunc->setName(vecFuncName);

    VectorMapping mapping(clonedFunc, vecFunc, vectorWidth, maskPos, resShape, argShapes);
    vectorizer.getPlatformInfo().addMapping(mapping); // prevent recursive vectorization

    // set-up vecInfo
    FunctionRegion funcWrapper(*clonedFunc);
    Region funcRegion(funcWrapper);
    VectorizationInfo vecInfo(funcRegion, mapping);

    // unify returns (if necessary)
    SingleReturnTrans::run(funcRegion);

    // compute anlaysis results
    PassBuilder PB;
    FunctionAnalysisManager FAM;
    PB.registerFunctionAnalyses(FAM);

    // compute DT, PDT, LI
    auto & DT = FAM.getResult<DominatorTreeAnalysis>(*clonedFunc);
    auto & PDT = FAM.getResult<PostDominatorTreeAnalysis>(*clonedFunc);
    auto & LI = FAM.getResult<LoopAnalysis>(*clonedFunc);
    auto & SE = FAM.getResult<ScalarEvolutionAnalysis>(*clonedFunc);
    auto & MDR = FAM.getResult<MemoryDependenceAnalysis>(*clonedFunc);
    auto & BPI = FAM.getResult<BranchProbabilityAnalysis>(*clonedFunc);

    // normalize loop exits (TODO make divLoopTrans work without this)
    {
      LoopInfo tmpLoopInfo(DT);
      LoopExitCanonicalizer canonicalizer(tmpLoopInfo);
      canonicalizer.canonicalize(*clonedFunc);
      DT.recalculate(*clonedFunc);
    }

    // run pipeline
    vectorizer.analyze(vecInfo, DT, PDT, LI);
    vectorizer.linearize(vecInfo, DT, PDT, LI, &BPI);
    vectorizer.vectorize(vecInfo, DT, LI, SE, MDR, nullptr);
    vectorizer.finalize();

    clonedFunc->eraseFromParent();

    return *vecFunc;
  }

  // result shape of function @funcName in target module @module
  VectorShape requestResultShape() {
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

static Function&
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

  assert(currBest);
  return *currBest;
}

std::unique_ptr<FunctionResolver>
SleefResolverService::resolve(llvm::StringRef funcName, llvm::FunctionType & scaFuncTy, const VectorShapeVec & argShapes, int vectorWidth, llvm::Module & destModule) {
  IF_DEBUG_SLEEF { errs() << "SLEEFResolverService: " << funcName << " for width " << vectorWidth << "\n"; }

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
  IF_DEBUG_SLEEF { errs() << "\t n/a\n"; }
  if (!archList) return nullptr;

  // decode bitwidth (for module lookup)
  bool doublePrecision = false;
  for (const auto * argTy : scaFuncTy.params()) {
    doublePrecision |= argTy->isDoubleTy();
  }

  auto & context = destModule.getContext();

  // remove the trailing isa specifier (_avx2/_avx/_sse/..)
  SleefISA isa = archList->isaIndex;

  std::string sleefName = funcDesc.vectorFnName;

  // TODO factor out
  bool isExtraFunc = funcDesc.vectorFnName.find("_extra") != std::string::npos;
  if (isExtraFunc) {
    int modIdx = (int) isa;
    auto *& mod = extraModules[modIdx];
    if (!mod) mod = createModuleFromBuffer(reinterpret_cast<const char*>(extraModuleBuffers[modIdx]), extraModuleBufferLens[modIdx], context);
    Function *vecFunc = mod->getFunction(sleefName);
    assert(vecFunc && "mapped extra function not found in module!");
    return std::make_unique<SleefLookupResolver>(destModule, /* RNG result */ VectorShape::varying(), *vecFunc, funcDesc.vectorFnName);
  }

  // Look in SLEEF module
  auto modIndex = sleefModuleIndex(isa, doublePrecision);
  llvm::Module*& mod = sleefModules[modIndex]; // TODO const Module
  if (!mod) {
    mod = createModuleFromBuffer(reinterpret_cast<const char*>(sleefModuleBuffers[modIndex]), sleefModuleBufferLens[modIndex], context);

    IF_DEBUG {
      bool brokenMod = verifyModule(*mod, &errs());
      if (brokenMod) abort();
    }
  }

  if (isa == SLEEF_VLA) {
    // on-the-fly vectorization module
    Function &vlaFunc = GetLeastPreciseImpl(*mod, sleefName, maxULPError);
    std::string baseName = vlaFunc.getName();
    return std::make_unique<SleefVLAResolver>(platInfo, baseName, config, vlaFunc, argShapes, vectorWidth);

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
    Function &vecFunc = GetLeastPreciseImpl(*mod, sleefName, maxULPError);
    std::string vecFuncName = vecFunc.getName().str() + "_" + archList->archSuffix;
    return std::make_unique<SleefLookupResolver>(destModule, resShape, vecFunc, vecFuncName);
  }
}




void
addSleefResolver(const Config & config, PlatformInfo & platInfo, unsigned maxULPError) {
  auto sleefRes = std::make_unique<SleefResolverService>(platInfo, config, maxULPError);
  IF_DEBUG { sleefRes->reportConfig(); }
  platInfo.addResolverService(std::move(sleefRes), true);
}






// TODO move into separate file
Constant & cloneConstant(Constant& constVal, Module & cloneInto) {
  if (isa<GlobalValue>(constVal)) {
    return cloneGlobalIntoModule(cast<GlobalValue>(constVal), cloneInto);
  }
  auto * expr = dyn_cast<ConstantExpr>(&constVal);
  if (!expr) return constVal;

  // descend into operands and replicate
  const ConstantExpr & constExpr = *expr;

  SmallVector<Constant*, 4> clonedOps;
  for (size_t i = 0; i < constExpr.getNumOperands(); ++i) {
    const Constant & cloned = cloneConstant(*constExpr.getOperand(i), cloneInto);
    clonedOps.push_back(const_cast<Constant*>(&cloned));
  }

  return *constExpr.getWithOperands(clonedOps);
}

GlobalValue & cloneGlobalIntoModule(GlobalValue &gv, Module &cloneInto) {
  if (isa<Function>(gv)) {
    auto & func = cast<Function>(gv);
    return cloneFunctionIntoModule(func, cloneInto, func.getName());

  } else {
    assert(isa<GlobalVariable>(gv));
    auto & global = cast<GlobalVariable>(gv);
    auto * clonedGv = cloneInto.getGlobalVariable(global.getName());
    if (clonedGv) return *clonedGv;

    // clone
    auto * clonedGlobal = cast<GlobalVariable>(cloneInto.getOrInsertGlobal(global.getName(), global.getValueType()));

    // clone initializer (could depend on other constants)
    if (global.hasInitializer()) {
      auto * initConst = const_cast<Constant*>(global.getInitializer());
      Constant * clonedInitConst = initConst;
      if (initConst) {
        clonedInitConst = &cloneConstant(*initConst, cloneInto);
      }
      clonedGlobal->setInitializer(clonedInitConst);
    }

    clonedGlobal->setThreadLocalMode(global.getThreadLocalMode());
    clonedGlobal->setAlignment(global.getAlignment());
    clonedGlobal->copyAttributesFrom(&global);
    return *clonedGlobal;
  }

  // unsupported global value
  abort();
}

Function &cloneFunctionIntoModule(Function &func, Module &cloneInto, StringRef name) {
  // eg already migrated
  auto * existingFn = cloneInto.getFunction(name);
  if (existingFn && (func.isDeclaration() == existingFn->isDeclaration())) {
    return *existingFn;
  }

  // create function in new module, create the argument mapping, clone function into new function body, return
  Function & clonedFn = *Function::Create(func.getFunctionType(), Function::LinkageTypes::ExternalLinkage,
                                        name, &cloneInto);
  clonedFn.copyAttributesFrom(&func);

  // external decl
  if (func.isDeclaration()) return clonedFn;

  ValueToValueMapTy VMap;
  auto CI = clonedFn.arg_begin();
  for (auto I = func.arg_begin(), E = func.arg_end(); I != E; ++I, ++CI) {
    Argument *arg = &*I, *carg = &*CI;
    carg->setName(arg->getName());
    VMap[arg] = carg;
  }
  // remap constants
  for (auto I = inst_begin(func), E = inst_end(func); I != E; ++I) {
    for (size_t i = 0; i < I->getNumOperands(); ++i) {
      auto * usedConstant = dyn_cast<Constant>(I->getOperand(i));
      if (!usedConstant) continue;

      auto & clonedConstant = cloneConstant(*usedConstant, cloneInto);
      VMap[usedConstant] = &clonedConstant;
    }
  }

  SmallVector<ReturnInst *, 1> Returns; // unused

  CloneFunctionInto(&clonedFn, &func, VMap, false, Returns);
  return clonedFn;
}


// compiler-rt early inlining
Function *
requestScalarImplementation(const StringRef & funcName, FunctionType & funcTy, Module &insertInto) {
#ifdef RV_ENABLE_CRT
  if (!scalarModule) {
    scalarModule = createModuleFromBuffer(reinterpret_cast<const char*>(&crt_Buffer), crt_BufferLen, insertInto.getContext());
  }

  if (!scalarModule) return nullptr; // could not load module

  auto * scalarFn = scalarModule->getFunction(funcName);
  if (!scalarFn) return nullptr;
  return &cloneFunctionIntoModule(*scalarFn, insertInto, funcName);
#else
  return nullptr; // compiler-rt not available as bc module
#endif
}



} // namespace rv
