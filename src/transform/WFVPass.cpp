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

#include "rvConfig.h"
#include "rv/rvDebug.h"

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

int
ParseInt(std::stringstream & stream) {
  unsigned n = 0;
  char c = stream.peek();
  while ('0' <= c && c <= '9') {
    n = 10*n + (c - '0');
  }
  return n;
}

bool
WFVPass::parseVectorMapping(Function & scalarFn, StringRef & attribText, VectorMapping & mapping) {
  if (!attribText.startswith("_ZGV")) return false;
  errs() << "Checking: " << attribText << "\n";

  if (attribText.size() < 6) return false;

  // parse general vector attribs
  int vecRegisterBits = ParseRegisterWidth(attribText[4]);

  bool needsMask = attribText[5] == 'M';

  // parse vectorization factor
  char * pos; // = attribText.begin() + 6; // "_ZGV<api><vecbits>"
  unsigned vectorWidth = strtol(attribText.begin() + 6, &pos, 10);

  // process arument shapes
  VectorShapeVec argShapes;

  auto * endText = attribText.end();

  for (; pos != endText && *pos != '_'; ) {
    char token = *pos;
    switch(token) {
      case 'v': argShapes.push_back(VectorShape::varying()); ++pos; break;
      case 'u': argShapes.push_back(VectorShape::uni()); ++pos; break;
      case 'a': {
        char * nextPos;
        ++pos;
        auto alignVal = strtol(pos, &nextPos, 10);
        pos = nextPos;

        int lastArgIdx = argShapes.size() - 1;
        assert(lastArgIdx >= 0);
        argShapes[lastArgIdx].setAlignment(alignVal);
      } break;
      case 'l': {
        ++pos;
        char * nextPos;
        auto strideVal = strtol(pos, &nextPos, 10);
        pos = nextPos;

        argShapes.push_back(VectorShape::strided(strideVal));
      } break;
      // case 's': // ??

      default:
        abort();
    }
  }

  // TODO create SIMD declaration
  mapping.scalarFn = &scalarFn;
  mapping.resultShape = VectorShape::varying();
  mapping.argShapes = argShapes;
  mapping.maskPos = needsMask ? 0 : -1;
  mapping.vectorFn = createVectorDeclaration(scalarFn, mapping.resultShape, mapping.argShapes, vectorWidth, mapping.maskPos);
  mapping.vectorFn->setLinkage(GlobalValue::ExternalLinkage); // FIXME for debugging

  mapping.dump(errs());
  return true;
}

bool
WFVPass::runOnFunction(Function & F, PlatformInfo & platInfo) {
  auto attribSet = F.getAttributes().getFnAttributes();

  // parse SIMD signatures
  std::vector<VectorMapping> wfvJobs;
  for (auto attrib : attribSet) {
    if (!attrib.isStringAttribute()) continue;
    StringRef attribText = attrib.getKindAsString();

    VectorMapping vecMapping;
    if (attribText.size() < 2) continue;

    if (!parseVectorMapping(F, attribText, vecMapping)) continue;
    wfvJobs.push_back(vecMapping);
  }

  if (wfvJobs.empty()) return false;

  // vectorize jobs
  Config rvConfig; // TODO parse machine attributes of scalar function
  VectorizerInterface vectorizer(platInfo, rvConfig);
  for (auto & job : wfvJobs) {
  }


  //
  return; // TODO
}

bool
WFVPass::runOnModule(Module & M) {
  bool changed = false;
  for (auto & func : M) {
    if (func.isDeclaration()) continue;

    auto & TTI = getAnalysis<TargetTransformInfoWrapperPass>().getTTI(func);
    auto & TLI = getAnalysis<TargetLibraryInfoWrapperPass>().getTLI();
    PlatformInfo platInfo(M, &TTI, &TLI);

    changed |= runOnFunction(func, platInfo);
  }

  return changed;
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

