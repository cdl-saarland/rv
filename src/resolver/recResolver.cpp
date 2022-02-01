//===- src/resolver/recResolver.cpp - inter-procedural vectorizer as resolver --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "rv/resolver/resolver.h"

#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/Support/raw_ostream.h>

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
#include <llvm/IR/Instructions.h>

#include "rv/annotations.h"
#include "rv/PlatformInfo.h"
#include "utils/rvTools.h"
#include "rvConfig.h"
#include "rv/rv.h"
#include "rv/utils.h"
#include "utils/rvTools.h"
#include "rv/region/FunctionRegion.h"
#include "rv/transform/singleReturnTrans.h"
#include "rv/passes/loopExitCanonicalizer.h"
#include "rv/passes/PassManagerSession.h"
#include "report.h"

using namespace llvm;

static bool
CanVectorizeType(const Type& type) {
  return type.isVoidTy() || (!type.isVectorTy() && (type.isFloatingPointTy() || type.isIntegerTy()));
}

#if 1
#define IF_DEBUG_RECRES IF_DEBUG
#else
#define IF_DEBUG_RECRES if (true)
#endif

namespace rv {


class RecursiveResolverService : public ResolverService {
  VectorizerInterface vectorizer;

public:
  std::unique_ptr<FunctionResolver> resolve(llvm::StringRef funcName, llvm::FunctionType & scaFuncTy, const VectorShapeVec & argShapes, int vectorWidth, bool hasPredicate, llvm::Module & destModule) override;

  RecursiveResolverService(PlatformInfo & platInfo, Config config)
  : vectorizer(platInfo, config)
  {}

  void print(raw_ostream & out) const override {
    out << "{RecursiveVectorizer}";
  }
};


// FIXME predication
class RecursiveResolver : public FunctionResolver {
  bool hasValidVectorFunc;
  VectorizerInterface vectorizer;
  VectorMapping recMapping;

public:
  StringRef
  getVectorName() const { return recMapping.vectorFn->getName(); }

  // a reference to it
  llvm::Function &requestVectorized() override {
    assert(isValid());
    assert(recMapping.vectorFn);
    return *recMapping.vectorFn;
  }

  // need a proper res shape
  VectorShape
  requestResultShape() override {
    return recMapping.resultShape;
  }

  // how the vector function of \p requestVectorized() should be called in a predicated context.
  CallPredicateMode getCallSitePredicateMode() override { return recMapping.predMode; }

  // mask position (if any)
  int getMaskPos() override { return recMapping.maskPos; }

  bool isValid() const { return hasValidVectorFunc; }

  RecursiveResolver(VectorizerInterface & vectorizer, Function & scaFunc, VectorShapeVec argShapes, int vectorWidth, bool hasCallSitePredicate)
  : FunctionResolver(*scaFunc.getParent())
  , hasValidVectorFunc(false)
  , vectorizer(vectorizer)
  , recMapping(&scaFunc, nullptr, vectorWidth, hasCallSitePredicate ? argShapes.size() : -1, VectorShape::undef(), argShapes, hasCallSitePredicate ? CallPredicateMode::PredicateArg : CallPredicateMode::Unpredicated)
  {
// create scalar copy
    ValueToValueMapTy cloneMap;
    Function * clonedFunc = CloneFunction(&scaFunc, cloneMap);
    if (recMapping.maskPos >= 0) {
      MaterializeEntryMask(*clonedFunc, vectorizer.getPlatformInfo());
    }

    recMapping.scalarFn = clonedFunc;
    assert(clonedFunc);

// run the analysis
    // use a preliminary self-mapping (this makes sure that we will not spawn
    // This makes sure that there will only ever be this RecursiveResolver session.

    // set-up vecInfo
    FunctionRegion funcWrapper(*clonedFunc);
    Region funcRegion(funcWrapper);

    // unify returns (if necessary)
    SingleReturnTrans::run(funcRegion);

    // compute anlaysis results
    PassManagerSession PMS;

    // re-establish LCSSA
    FunctionPassManager FPM;
    FPM.addPass<LCSSAPass>(LCSSAPass());
    FPM.run(*clonedFunc, PMS.FAM);

    // compute DT, PDT, LI
    // normalize loop exits (TODO make divLoopTrans work without this)
    {
      auto & tmpLI = PMS.FAM.getResult<LoopAnalysis>(*clonedFunc);
      LoopExitCanonicalizer canonicalizer(tmpLI);
      canonicalizer.canonicalize(*clonedFunc);
      auto PA = PreservedAnalyses::all();
      PA.abandon<DominatorTreeAnalysis>();
      PA.abandon<PostDominatorTreeAnalysis>();
      PA.abandon<LoopAnalysis>();
      PMS.FAM.invalidate(*clonedFunc, PA);
      PMS.FAM.getResult<LoopAnalysis>(*clonedFunc);
    }

// run analysis until result shape stabilizes
    // this is an ad-hoc mapping
    VectorShape lastResShape = VectorShape::undef();

    // callMapping will be the proper, final result mapping
    // CallPredicateMode predMode = recMapping.maskPos >= 0 ? CallPredicateMode::PredicateArg : CallPredicateMode::SafeWithoutPredicate;
    VectorMapping callMapping(&scaFunc, nullptr, recMapping.vectorWidth, recMapping.maskPos, lastResShape, argShapes, recMapping.predMode);
    bool returnsVoid = scaFunc.getReturnType()->isVoidTy();
    VectorShape nextResultShape = lastResShape;
    do {
      // update & publish the best known mapping
      vectorizer.getPlatformInfo().forgetMapping(callMapping);
      callMapping.resultShape = nextResultShape;
      vectorizer.getPlatformInfo().addMapping(callMapping); // prevent recursive vectorization
      lastResShape = callMapping.resultShape;

      IF_DEBUG_RECRES { errs() << "RR: analyzing " << scaFunc.getName() << " with res shape " << lastResShape.str() << "\n"; }

      // run VA (on clonedFunc) -> tempVecInfo
      VectorizationInfo tempVecInfo(funcRegion, recMapping);
      vectorizer.analyze(tempVecInfo, PMS.FAM);

      // refine the result shape (from tempVecInfo)
      if (!returnsVoid) {
        funcRegion.for_blocks([&](const BasicBlock & BB) {
          auto * retInst = dyn_cast<ReturnInst>(BB.getTerminator());
          if (!retInst) return true; // continue

          // FIXME this assumes that there is actually only one return
          // refined shape
          nextResultShape = tempVecInfo.getVectorShape(*retInst->getReturnValue());
          return false;
        });
      }
      IF_DEBUG_RECRES { errs() << "RR: refined result Shape for " << scaFunc.getName() << " to res shape " << callMapping.resultShape.str() << "\n"; }

      // TODO re-run the analysis if the result changed (start with undef shape..)
    } while (!returnsVoid && (lastResShape != nextResultShape));

// the return value has stabilized.. no generate code
    // FIXME this assertion will fire on really nastyc call graph SCCs that are nevertheless valid.
    // Would require proper inter-procedural VA to fix this.

    // bail if the return type did not turn out to be vectorizable
    if (nextResultShape.isVarying() && !CanVectorizeType(*clonedFunc->getReturnType())) {
      vectorizer.getPlatformInfo().forgetAllMappingsFor(*clonedFunc);
      clonedFunc->eraseFromParent();
      vectorizer.getPlatformInfo().forgetMapping(callMapping);        // drop the incomplete mapping
      hasValidVectorFunc = false;
      return;
    }

    std::string mangledName = vectorizer.getPlatformInfo().createMangledVectorName(scaFunc.getName(), callMapping.argShapes, callMapping.vectorWidth, callMapping.maskPos);
    auto * knownVecFunc = vectorizer.getModule().getFunction(mangledName);

    // Have we already emitted this function in a recursive incovation?
    Function * vecFunc = nullptr;
    if (knownVecFunc) {
      vecFunc = knownVecFunc; // reuse the existing function

      IF_DEBUG_RECRES { errs() << "Known func mapping!\n"; }

      // update mapping to use the identified \p vecFunc
      vectorizer.getPlatformInfo().forgetMapping(callMapping);
      callMapping.vectorFn = knownVecFunc;
      vectorizer.getPlatformInfo().addMapping(callMapping);

    // Otw, start emitting code
    } else {
    // create a proper SIMD declaration with the inferred type
      vecFunc = createVectorDeclaration(*clonedFunc, nextResultShape, callMapping.argShapes, callMapping.vectorWidth, callMapping.maskPos);
      vecFunc->setName(mangledName);
      vecFunc->copyAttributesFrom(&scaFunc);

    // update mapping to use the declared \p vecFunc
      vectorizer.getPlatformInfo().forgetMapping(callMapping);
      callMapping.vectorFn = vecFunc;
      vectorizer.getPlatformInfo().addMapping(callMapping);

    // setup a vectorization job from clonedFunc into vecFunc
      recMapping.resultShape = nextResultShape;
      recMapping.vectorFn = vecFunc;
      VectorizationInfo vecInfo(funcRegion, recMapping);

    // generate the vector function body
      vectorizer.analyze(vecInfo, PMS.FAM);
      vectorizer.linearize(vecInfo, PMS.FAM);
      vectorizer.vectorize(vecInfo, PMS.FAM, nullptr);
      vectorizer.finalize();
    }

    // can dispose of temporary function now
    vectorizer.getPlatformInfo().forgetAllMappingsFor(*clonedFunc);
    clonedFunc->eraseFromParent();

    // DEBUG
    IF_DEBUG_RECRES {
      errs() << "After recursive invocation!\n";
      vectorizer.getPlatformInfo().dump();
    }

    // success!
    recMapping = callMapping;
    assert(recMapping.vectorFn);
    hasValidVectorFunc = true;
  }
};


std::unique_ptr<FunctionResolver>
RecursiveResolverService::resolve(llvm::StringRef funcName, llvm::FunctionType & scaFuncTy, const VectorShapeVec & argShapes, int vectorWidth, bool hasPredicate, llvm::Module & destModule) {
// is this function defined?
  auto * scaFunc = destModule.getFunction(funcName);
  if (!scaFunc) return nullptr;
  if (scaFunc->isDeclaration()) return nullptr;
  if (!typesMatch(scaFunc->getFunctionType(), &scaFuncTy)) return nullptr;

  // have all varying params vectorizabel types?
  int i = 0;
  for (auto * paramTy : scaFuncTy.params()) {
    if (argShapes[i++].isVarying() && !CanVectorizeType(*paramTy)) return nullptr;
  }

  // FIXME legality?
  // under which circumstances may we vectorize this function?
  if (IsCriticalSection(*scaFunc)) {
    IF_DEBUG_RECRES { errs() << "RR: won't vectorize critical section " << scaFunc->getName() << "\n"; }
    return nullptr; // do not vectorize annotated critical sections
  }

  // try to create vector code for this function
  auto * recResolver = new RecursiveResolver(vectorizer, *scaFunc, argShapes, vectorWidth, hasPredicate);
  // the function could turn out to be unvectorizable (::isValid())
  if (!recResolver->isValid()) {
    return nullptr;
  }

  Report() << "recursively vectorized function " << funcName << " -> " << recResolver->getVectorName() << "\n";
  (void) recResolver->requestVectorized();
  return std::unique_ptr<FunctionResolver>(std::move(recResolver));
}


void
addRecursiveResolver(const Config & config, PlatformInfo & platInfo) {
  // recursive vectorize MUST go last to enable caching in platInfo
  platInfo.addResolverService(std::unique_ptr<ResolverService>(new RecursiveResolverService(platInfo, config)), false);
}


} // namespace rv
