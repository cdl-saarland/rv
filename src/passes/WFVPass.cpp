//===- src/transform/WFVPass.h - whole-function vectorizer pass  --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#include "rv/passes/WFVPass.h"
#include "rv/legacy/LinkAllPasses.h"

#include "rv/rv.h"
#include "rv/vectorMapping.h"
#include "rv/region/LoopRegion.h"
#include "rv/region/Region.h"
#include "rv/resolver/resolvers.h"
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
#include "llvm/InitializePasses.h"
#include "llvm/Passes/PassBuilder.h"

#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/ScalarEvolutionExpressions.h"
#include "llvm/Analysis/MemoryDependenceAnalysis.h"
#include "llvm/Analysis/BranchProbabilityInfo.h"

#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/ADT/Sequence.h"

#include "report.h"
#include <map>
#include <sstream>
#include <cassert>

using namespace rv;
using namespace llvm;


///// Pass Implementation /////

WFV::WFV() : PMS() {}

void
WFV::vectorizeFunction(VectorizerInterface & vectorizer, VectorMapping & wfvJob) {
  // clone scalar function
  ValueToValueMapTy cloneMap;
  Function* scalarCopy = CloneFunction(wfvJob.scalarFn, cloneMap, nullptr);
  wfvJob.scalarFn = scalarCopy;

  if (wfvJob.maskPos >= 0) {
    MaterializeEntryMask(*scalarCopy, vectorizer.getPlatformInfo());
  }

  // regino setup
  FunctionRegion funcRegion(*wfvJob.scalarFn);
  Region funcRegionWrapper(funcRegion);

  // unify returns as necessary
  SingleReturnTrans::run(funcRegionWrapper);

// early math func lowering
  // vectorizer.lowerRuntimeCalls(vecInfo, LI);
  // DT->recalculate(*F);
  // PDT->recalculate(*F);
  // cdg.create(*F);
  // dfg.create(*F);

  VectorizationInfo vecInfo(funcRegionWrapper, wfvJob);

// Vectorize
  // vectorizationAnalysis
  vectorizer.analyze(vecInfo, PMS.FAM); // TODO can be shared across jobs

  if (enableDiagOutput) {
    errs() << "-- VA result --\n";
    vecInfo.dump();
    errs() << "-- EOF --\n";
  }

  IF_DEBUG Dump(*scalarCopy);

  // control conversion
  vectorizer.linearize(vecInfo, PMS.FAM);

  // vectorize the prepared loop embedding it in its context
  ValueToValueMapTy vecMap;

  // FIXME SE is invalid at this point..
  ScalarEvolutionAnalysis adhocAnalysis;
  adhocAnalysis.run(*scalarCopy, PMS.FAM);

  MemoryDependenceAnalysis mdAnalysis;
  mdAnalysis.run(*scalarCopy, PMS.FAM);

  // FIXME share state until this point (modified src function)
  bool vectorizeOk = vectorizer.vectorize(vecInfo, PMS.FAM, &vecMap);
  if (!vectorizeOk)
    llvm_unreachable("vector code generation failed");

  scalarCopy->eraseFromParent();
}

void
WFV::collectJobs(Function & F) {
  auto attribSet = F.getAttributes().getFnAttrs();

  // parse SIMD signatures
  for (auto attrib : attribSet) {
    if (!attrib.isStringAttribute()) continue;
    StringRef attribText = attrib.getKindAsString();

    VectorMapping vecMapping;
    if (!parseVectorMapping(F, attribText, vecMapping, true)) continue;

    wfvJobs.push_back(vecMapping);
  }
}

bool WFV::run(Module &M) {
  enableDiagOutput = CheckFlag("WFV_DIAG");

  // collect WFV jobs
  for (auto &func : M) {
    if (func.isDeclaration())
      continue;

    collectJobs(func);
  }

  // no annotated functions found (pragma omp declare simd)
  if (wfvJobs.empty())
    return false;

  auto &protoFunc = *wfvJobs[0].scalarFn;

  Config rvConfig = Config::createForFunction(protoFunc);

  auto &TTI = PMS.FAM.getResult<TargetIRAnalysis>(protoFunc);
  auto &TLI = PMS.FAM.getResult<TargetLibraryAnalysis>(protoFunc);

  // configure platInfo
  PlatformInfo platInfo(M, &TTI, &TLI);
  addSleefResolver(rvConfig, platInfo);

  // add mappings for recursive vectorization
  for (auto &job : wfvJobs) {
    platInfo.addMapping(job);
  }

  // vectorize jobs
  VectorizerInterface vectorizer(platInfo, rvConfig);
  for (auto &job : wfvJobs) {
    vectorizeFunction(vectorizer, job);
  }

  return true;
}

///// Legacy PM Wrapper /////

bool WFVLegacyPass::runOnModule(Module &M) {
  WFV WFVImpl;
  return WFVImpl.run(M);
}

/// Register all analyses and transformation required.
void
WFVLegacyPass::getAnalysisUsage(AnalysisUsage &AU) const {
}

char WFVLegacyPass::ID = 0;

ModulePass *rv::createWFVLegacyPass() { return new WFVLegacyPass(); }

INITIALIZE_PASS_BEGIN(WFVLegacyPass, "rv-function-vectorize",
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
INITIALIZE_PASS_END(WFVLegacyPass, "rv-function-vectorize", "RV - Vectorize functions",
                    false, false)

///// New PM Pass /////

WFVWrapperPass::WFVWrapperPass() {}

llvm::PreservedAnalyses WFVWrapperPass::run(llvm::Module &M,
                                            llvm::ModuleAnalysisManager &MAM) {
  WFV WFVImpl;
  if (WFVImpl.run(M))
    return llvm::PreservedAnalyses::none();
  else
    return llvm::PreservedAnalyses::all();
}
