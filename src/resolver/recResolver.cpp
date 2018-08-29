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
#include "rv/transform/loopExitCanonicalizer.h"
#include "report.h"

using namespace llvm;

namespace rv {


class RecursiveResolverService : public ResolverService {
  VectorizerInterface vectorizer;

public:
  std::unique_ptr<FunctionResolver> resolve(llvm::StringRef funcName, llvm::FunctionType & scaFuncTy, const VectorShapeVec & argShapes, int vectorWidth, llvm::Module & destModule);

  RecursiveResolverService(PlatformInfo & platInfo, Config config)
  : vectorizer(platInfo, config)
  {}

  void print(raw_ostream & out) const override {
    out << "{RecursiveVectorizer}";
  }
};


// FIXME predication
class RecursiveResolver : public FunctionResolver {
  VectorizerInterface vectorizer;
  VectorMapping recMapping;

public:
  // a reference to it
  llvm::Function &requestVectorized() {
    return *recMapping.vectorFn;
  }

  // need a proper res shape
  VectorShape
  requestResultShape() {
    return recMapping.resultShape;
  }

  RecursiveResolver(VectorizerInterface & vectorizer, Function & scaFunc, VectorShapeVec argShapes, int vectorWidth)
  : FunctionResolver(*scaFunc.getParent())
  , vectorizer(vectorizer)
  , recMapping(&scaFunc, nullptr, vectorWidth, -1, VectorShape::undef(), argShapes)
  {
    ValueToValueMapTy cloneMap;
    Function * clonedFunc = CloneFunction(&scaFunc, cloneMap);
    assert(clonedFunc);

// prepare scalar copy for transforming
    const int maskPos = -1; // TODO add support for masking

// run the analysis
    // use a preliminary self-mapping (this makes sure that we will not spawn
    // This makes sure that there will only ever be this RecursiveResolver session.

    // set-up vecInfo
    FunctionRegion funcWrapper(*clonedFunc);
    Region funcRegion(funcWrapper);

    // unify returns (if necessary)
    SingleReturnTrans::run(funcRegion);

    // compute anlaysis results
    PassBuilder PB;
    FunctionAnalysisManager FAM;
    PB.registerFunctionAnalyses(FAM);

    // compute DT, PDT, LI
    auto &DT = FAM.getResult<DominatorTreeAnalysis>(*clonedFunc);
    auto &PDT = FAM.getResult<PostDominatorTreeAnalysis>(*clonedFunc);
    auto &LI = FAM.getResult<LoopAnalysis>(*clonedFunc);
    auto &SE = FAM.getResult<ScalarEvolutionAnalysis>(*clonedFunc);
    auto &MDR = FAM.getResult<MemoryDependenceAnalysis>(*clonedFunc);
    auto &BPI = FAM.getResult<BranchProbabilityAnalysis>(*clonedFunc);

    // normalize loop exits (TODO make divLoopTrans work without this)
    {
      LoopInfo tmpLoopInfo(DT);
      LoopExitCanonicalizer canonicalizer(tmpLoopInfo);
      canonicalizer.canonicalize(*clonedFunc);
      DT.recalculate(*clonedFunc);
    }

// run analysis until result shape stabilizes
    // this is an ad-hoc mapping
    VectorShape lastResShape = VectorShape::undef();
    VectorMapping selfMapping(clonedFunc, clonedFunc, vectorWidth, maskPos, lastResShape, argShapes);
    bool returnsVoid = scaFunc.getReturnType()->isVoidTy();
    do {
      VectorizationInfo selfVecInfo(funcRegion, selfMapping);
      selfMapping.resultShape = recMapping.resultShape;

      // publish the best known mapping
      vectorizer.getPlatformInfo().forgetAllMappingsFor(*clonedFunc);
      vectorizer.getPlatformInfo().addMapping(selfMapping); // prevent recursive vectorization

      errs() << "RR: analyzing " << scaFunc.getName() << " with res shape " << lastResShape.str() << "\n";
      lastResShape = recMapping.resultShape;

      // run VA
      vectorizer.analyze(selfVecInfo, DT, PDT, LI);

      // refine the result shape
      if (!returnsVoid) {
        funcRegion.for_blocks([&](const BasicBlock & BB) {
          auto * retInst = dyn_cast<ReturnInst>(BB.getTerminator());
          if (!retInst) return true; // continue

          // FIXME this assumes that there is actually only one return
          recMapping.resultShape = selfVecInfo.getVectorShape(*retInst->getReturnValue());
          return false;
        });
      }

      // TODO re-run the analysis if the result changed (start with undef shape..)
    } while (!returnsVoid && (lastResShape != recMapping.resultShape));

// the return value has stabilized.. no generate code
    // FIXME this assertion will fire on really nastyc call graph SCCs that are nevertheless valid.
    // Would require proper inter-procedural VA to fix this.
    assert(recMapping.resultShape.isDefined() && "there must be a defined result if (a) any function in this CallGraph SCC returns at all and (b) it returns without further recursive descend..");

    VectorizationInfo vecInfo(funcRegion, recMapping);

    // create a proper SIMD declaration with the inferred type
    auto * vecFunc = createVectorDeclaration(*clonedFunc, recMapping.resultShape, recMapping.argShapes, recMapping.vectorWidth);
    vecFunc->copyAttributesFrom(&scaFunc);
    // vecFunc->setName(vecFuncName); // TODO use an OpenMP "pragma omp SIMD" name.

    // discard temporary mapping
    vectorizer.getPlatformInfo().forgetAllMappingsFor(*clonedFunc);
    // register final mapping
    vectorizer.getPlatformInfo().addMapping(recMapping);

// fill in SIMD code
    vectorizer.linearize(vecInfo, DT, PDT, LI, &BPI);
    vectorizer.vectorize(vecInfo, DT, LI, SE, MDR, nullptr);
    vectorizer.finalize();

    // can dispose of temporary function now
    clonedFunc->eraseFromParent();

    recMapping.vectorFn = vecFunc;
  }
};


std::unique_ptr<FunctionResolver>
RecursiveResolverService::resolve(llvm::StringRef funcName, llvm::FunctionType & scaFuncTy, const VectorShapeVec & argShapes, int vectorWidth, llvm::Module & destModule) {
// is this function defined?
  auto * scaFunc = destModule.getFunction(funcName);
  if (!scaFunc) return nullptr;
  if (!typesMatch(scaFunc->getFunctionType(), &scaFuncTy)) return nullptr;

  // FIXME legality?
  // under which circumstances may we vectorize this function?
  if (IsCriticalSection(*scaFunc)) {
    IF_DEBUG { errs() << "RR: won't vectorize critical section " << scaFunc->getName() << "\n"; }
    return nullptr; // do not vectorize annotated critical sections
  }

  return std::unique_ptr<FunctionResolver>(new RecursiveResolver(vectorizer, *scaFunc, argShapes, vectorWidth));
}


void
addRecursiveResolver(PlatformInfo & platInfo, const Config & config) {
  platInfo.addResolverService(std::unique_ptr<ResolverService>(new RecursiveResolverService(platInfo, config)), true);
}


} // namespace rv
