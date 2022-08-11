//===- src/transform/WFVPass.h - whole-function vectorizer pass  --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#include "rv/transform/WFVPass.h"
#include "rv/LinkAllPasses.h"

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

/// Register all analyses and transformation required.
void
WFVPass::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequired<DominatorTreeWrapperPass>();
  AU.addRequired<PostDominatorTreeWrapperPass>();
  AU.addRequired<LoopInfoWrapperPass>();
  AU.addRequired<TargetTransformInfoWrapperPass>();
  AU.addRequired<TargetLibraryInfoWrapperPass>();
}

void
WFVPass::vectorizeFunction(VectorizerInterface & vectorizer, VectorMapping & wfvJob) {
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

  // prepare analyses
  PassBuilder PB;
  FunctionAnalysisManager FAM;
  PB.registerFunctionAnalyses(FAM);

// early math func lowering
  // vectorizer.lowerRuntimeCalls(vecInfo, LI);
  // DT->recalculate(*F);
  // PDT->recalculate(*F);
  // cdg.create(*F);
  // dfg.create(*F);

  VectorizationInfo vecInfo(funcRegionWrapper, wfvJob);

// Vectorize
  // vectorizationAnalysis
  vectorizer.analyze(vecInfo, FAM); // TODO can be shared across jobs

  if (enableDiagOutput) {
    errs() << "-- VA result --\n";
    vecInfo.dump();
    errs() << "-- EOF --\n";
  }

  IF_DEBUG Dump(*scalarCopy);

  // control conversion
  vectorizer.linearize(vecInfo, FAM);

  // vectorize the prepared loop embedding it in its context
  ValueToValueMapTy vecMap;

  // FIXME SE is invalid at this point..
  ScalarEvolutionAnalysis adhocAnalysis;
  adhocAnalysis.run(*scalarCopy, FAM);

  MemoryDependenceAnalysis mdAnalysis;
  mdAnalysis.run(*scalarCopy, FAM);

  // FIXME share state until this point (modified src function)
  bool vectorizeOk = vectorizer.vectorize(vecInfo, FAM, &vecMap);
  if (!vectorizeOk)
    llvm_unreachable("vector code generation failed");

  scalarCopy->eraseFromParent();
}

bool
WFVPass::isSaneMapping(VectorMapping & wfvJob) const {
  DataLayout DL(wfvJob.scalarFn->getParent());

  auto & scaFuncTy = *wfvJob.scalarFn->getFunctionType();
  for (auto argIdx : seq<int>(0, wfvJob.argShapes.size())) {
    // uniform shapes are always permissible
    auto argShape = wfvJob.argShapes[argIdx];
    if (argShape.isUniform()) continue;

    // do not allow strided pointers with overlapping element types
    auto * argPtrTy = dyn_cast<PointerType>(scaFuncTy.getParamType(argIdx));
    if (!argPtrTy) continue;
    size_t elemByteSize = DL.getTypeStoreSize(argPtrTy->getElementType());

    if (argShape.hasStridedShape() &&
       ((size_t) std::abs(argShape.getStride())) < elemByteSize) {
      return false;
    }
  }

  // passed all tests
  return true;
}

void
WFVPass::collectJobs(Function & F) {
  auto attribSet = F.getAttributes().getFnAttrs();

  // parse SIMD signatures
  for (auto attrib : attribSet) {
    if (!attrib.isStringAttribute()) continue;
    StringRef attribText = attrib.getKindAsString();

    VectorMapping vecMapping;
    if (!parseVectorMapping(F, attribText, vecMapping, true)) continue;
    if (!isSaneMapping(vecMapping)) continue;

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
 
  auto & protoFunc = *wfvJobs[0].scalarFn;

  // configure platform info
  auto & TLI = getAnalysis<TargetLibraryInfoWrapperPass>().getTLI(protoFunc);

  // FIXME this assumes that all functions were compiled for the same target
  auto & TTI = getAnalysis<TargetTransformInfoWrapperPass>().getTTI(protoFunc);
  Config rvConfig = Config::createForFunction(protoFunc);

  // configure platInfo
  PlatformInfo platInfo(M, &TTI, &TLI);
  addSleefResolver(rvConfig, platInfo);

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

