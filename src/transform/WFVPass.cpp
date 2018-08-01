//===- WFVPass.cpp - Vectorize whole functions  ----------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//
//===----------------------------------------------------------------------===//

#include "rv/transform/WFVPass.h"
#include "rv/LinkAllPasses.h"

#include "rv/rv.h"
#include "rv/vectorMapping.h"
#include "rv/region/LoopRegion.h"
#include "rv/region/Region.h"
#include "rv/sleefLibrary.h"
#include "rv/analysis/reductionAnalysis.h"
#include "rv/analysis/costModel.h"
#include "rv/transform/remTransform.h"
#include "rv/utils.h"
#include "rv/transform/singleReturnTrans.h"

#include "rvConfig.h"
#include "rv/rvDebug.h"
#include "rv/region/FunctionRegion.h"

#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Dominators.h"
#include "llvm/Passes/PassBuilder.h"

#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/ScalarEvolutionExpressions.h"
#include "llvm/Analysis/MemoryDependenceAnalysis.h"
#include "llvm/Analysis/BranchProbabilityInfo.h"

#include "llvm/Transforms/Utils/Cloning.h"

#include "report.h"
#include <map>
#include <sstream>
#include <cassert>

using namespace rv;
using namespace llvm;

/// Register all analyses and transformation required.
void
WFVPass::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequired<DominatorTreeWrapperPass>();
  AU.addRequired<PostDominatorTreeWrapperPass>();
  AU.addRequired<LoopInfoWrapperPass>();
  AU.addRequired<TargetTransformInfoWrapperPass>();
  AU.addRequired<TargetLibraryInfoWrapperPass>();
}

static
int
ParseRegisterWidth(char c) {
  switch (c) {
    case 'b': return 128; // SSE
    case 'c': // AVX
    case 'd': return 256; // AVX2
    case 'e': return 512; // AVX512
    default:
      abort();
  };
}


void
WFVPass::vectorizeFunction(VectorizerInterface & vectorizer, VectorMapping & wfvJob) {
  // clone scalar function
  ValueToValueMapTy cloneMap;
  Function* scalarCopy = CloneFunction(wfvJob.scalarFn, cloneMap, nullptr);
  wfvJob.scalarFn = scalarCopy;

  // regino setup
  FunctionRegion funcRegion(*wfvJob.scalarFn);
  Region funcRegionWrapper(funcRegion);

  // unify returns as necessary
  SingleReturnTrans::run(funcRegionWrapper);

  // prepare analyses
  DominatorTree DT(*scalarCopy);
  PostDominatorTree PDT;
  PDT.recalculate(*scalarCopy);
  LoopInfo LI(DT);

// early math func lowering
  // vectorizer.lowerRuntimeCalls(vecInfo, LI);
  // DT->recalculate(*F);
  // PDT->recalculate(*F);
  // cdg.create(*F);
  // dfg.create(*F);

  VectorizationInfo vecInfo(funcRegionWrapper, wfvJob);

// Vectorize
  // vectorizationAnalysis
  vectorizer.analyze(vecInfo, DT, PDT, LI); // TODO can be shared across jobs

  if (enableDiagOutput) {
    errs() << "-- VA result --\n";
    vecInfo.dump();
    errs() << "-- EOF --\n";
  }

  IF_DEBUG Dump(*scalarCopy);

  // control conversion
  vectorizer.linearize(vecInfo, DT, PDT, LI, nullptr);

  DominatorTree domTreeNew(
      *vecInfo.getMapping().scalarFn); // Control conversion does not preserve
                                       // the domTree so we have to rebuild it
                                       // for now

  // vectorize the prepared loop embedding it in its context
  ValueToValueMapTy vecMap;

  // FIXME SE is invalid at this point..
  PassBuilder pb;
  FunctionAnalysisManager fam;
  pb.registerFunctionAnalyses(fam);
  ScalarEvolutionAnalysis adhocAnalysis;
  adhocAnalysis.run(*scalarCopy, fam);

  MemoryDependenceAnalysis mdAnalysis;
  MemoryDependenceResults MDR = mdAnalysis.run(*scalarCopy, fam);

  auto & localSE = fam.getResult<ScalarEvolutionAnalysis>(*scalarCopy);

  // FIXME share state until this point (modified src function)
  bool vectorizeOk = vectorizer.vectorize(vecInfo, domTreeNew, LI, localSE, MDR, &vecMap);
  if (!vectorizeOk)
    llvm_unreachable("vector code generation failed");

  scalarCopy->eraseFromParent();
}

void
WFVPass::collectJobs(Function & F) {
  auto attribSet = F.getAttributes().getFnAttributes();

  // parse SIMD signatures
  for (auto attrib : attribSet) {
    if (!attrib.isStringAttribute()) continue;
    StringRef attribText = attrib.getKindAsString();

    VectorMapping vecMapping;
    if (!parseVectorMapping(F, attribText, vecMapping, true)) continue;
    wfvJobs.push_back(vecMapping);
  }
}

bool
WFVPass::runOnModule(Module & M) {
  enableDiagOutput = CheckFlag("WFV_DIAG");

  // collect WFV jobs
  for (auto & func : M) {
    if (func.isDeclaration()) continue;

    collectJobs(func);
  }

  // no annotated functions found (pragma omp declare simd)
  if (wfvJobs.empty()) return false;

  // configure platform info
  auto & TLI = getAnalysis<TargetLibraryInfoWrapperPass>().getTLI();

  // FIXME this assumes that all functions were compiled for the same target
  auto & TTI = getAnalysis<TargetTransformInfoWrapperPass>().getTTI(*wfvJobs[0].scalarFn);
  Config rvConfig; // TODO parse machine attributes of scalar function

  // configure platInfo
  rvConfig.useSLEEF = true;
  PlatformInfo platInfo(M, &TTI, &TLI);
  addSleefResolver(rvConfig, platInfo, 35);

  // add mappings for recursive vectorization
  for (auto & job : wfvJobs) {
    platInfo.addMapping(job);
  }

  // vectorize jobs
  VectorizerInterface vectorizer(platInfo, rvConfig);
  for (auto & job : wfvJobs) {
    vectorizeFunction(vectorizer, job);
  }

  return true;
}



char WFVPass::ID = 0;

ModulePass *rv::createWFVPass() { return new WFVPass(); }

INITIALIZE_PASS_BEGIN(WFVPass, "rv-function-vectorize",
                      "RV - Vectorize functions", false, false)
INITIALIZE_PASS_DEPENDENCY(DominatorTreeWrapperPass)
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(MemoryDependenceWrapperPass)
INITIALIZE_PASS_DEPENDENCY(PostDominatorTreeWrapperPass)
INITIALIZE_PASS_DEPENDENCY(BranchProbabilityInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(ScalarEvolutionWrapperPass)
// PlatformInfo
INITIALIZE_PASS_DEPENDENCY(TargetTransformInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(TargetLibraryInfoWrapperPass)
INITIALIZE_PASS_END(WFVPass, "rv-function-vectorize", "RV - Vectorize functions",
                    false, false)

