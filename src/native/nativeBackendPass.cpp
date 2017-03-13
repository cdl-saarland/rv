//===- nativebackendPass.cpp -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @author simon

#include "rv/VectorizationInfoProxyPass.h"

#include "NatBuilder.h"
#include "nativeBackendPass.h"

using namespace llvm;

namespace rv {

  char NativeBackendPass::ID = 0;

  NativeBackendPass::NativeBackendPass()
      : FunctionPass(ID),
        vi(0),
        pi(0),
        domTree(0) {

  }

  NativeBackendPass::NativeBackendPass(VectorizationInfo *vi, PlatformInfo *pi, DominatorTree const *domTree, ReductionAnalysis * _reda)
  : FunctionPass(ID)
  , vi(vi)
  , pi(pi)
  , domTree(domTree)
  , reda(_reda)
  {}

  NativeBackendPass::~NativeBackendPass() {}


  void
  NativeBackendPass::getAnalysisUsage(llvm::AnalysisUsage &AU) const {
    if (!(vi && pi && domTree)) {
      AU.addRequired<VectorizationInfoProxyPass>();
      AU.addRequired<DominatorTreeWrapperPass>();
    }
    AU.addRequired<MemoryDependenceAnalysis>();
    AU.addRequired<ScalarEvolutionWrapperPass>();
  }

  bool
  NativeBackendPass::runOnFunction(llvm::Function &F) {
    auto & vecInfo = vi ? *vi : getAnalysis<VectorizationInfoProxyPass>().getInfo();
    auto & platformInfo = pi ? *pi : getAnalysis<VectorizationInfoProxyPass>().getPlatformInfo();

// Strip legacy metadata calls
    std::vector<Instruction *> killList;
    for (auto &block : F) {
      for (auto &inst : block) {
        auto *call = dyn_cast<CallInst>(&inst);
        if (!call) continue;
        auto *callee = call->getCalledFunction();
        if (!callee) continue;
        if (callee->getName() == "rvMetadataFn") {
          killList.push_back(call);
        }
      }
    }

    for (auto *inst : killList) inst->eraseFromParent();

// Get dominator tree and analyses
    auto &dtree = domTree ? *domTree : getAnalysis<DominatorTreeWrapperPass>().getDomTree();
    auto &mda = getAnalysis<MemoryDependenceAnalysis>();
    auto &se = getAnalysis<ScalarEvolutionWrapperPass>().getSE();

// invoke native
    assert(vecInfo.getMapping().scalarFn && "no scalar function to vectorize provided");
    assert(vecInfo.getMapping().vectorFn && "user has to provide simd function");
    assert((vecInfo.getRegion() ||
               (!vecInfo.getRegion() && (vecInfo.getMapping().scalarFn != vecInfo.getMapping().vectorFn)))
               && "scalar function and simd function must not be the same");
    native::NatBuilder builder(platformInfo, vecInfo, dtree, mda, se, *reda);
    builder.vectorize();

    return true;
  }


} /* namespace rv */

