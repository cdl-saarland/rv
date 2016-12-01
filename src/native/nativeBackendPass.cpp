//===- nativebackendPass.cpp -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @author simon

#include "rv/rvInfoProxyPass.h"
#include "rv/VectorizationInfoProxyPass.h"
#include "rv/rvInfo.h"

#include "NatBuilder.h"
#include "nativeBackendPass.h"

using namespace llvm;

namespace rv {

  char NativeBackendPass::ID = 0;

  NativeBackendPass::NativeBackendPass()
      : FunctionPass(ID) {

  }

  NativeBackendPass::~NativeBackendPass() {}


  void
  NativeBackendPass::getAnalysisUsage(llvm::AnalysisUsage &AU) const {
    AU.addRequired<VectorizationInfoProxyPass>();
    AU.addRequired<RVInfoProxyPass>();
    AU.addRequired<DominatorTreeWrapperPass>();
  }

  bool
  NativeBackendPass::runOnFunction(llvm::Function &F) {
    auto &vi = getAnalysis<VectorizationInfoProxyPass>().getInfo();

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

// Fetch platform info
    TargetIRAnalysis irAnalysis;
    TargetTransformInfo tti = irAnalysis.run(*vi.getMapping().scalarFn);
    TargetLibraryAnalysis libAnalysis;
    TargetLibraryInfo tli = libAnalysis.run(*vi.getMapping().scalarFn->getParent());
    PlatformInfo platformInfo(&tti, &tli);
    // mappings[vi->getMapping().scalarFn] = &vi->getMapping();

// Get dominator tree
    auto &dtree = getAnalysis<DominatorTreeWrapperPass>().getDomTree();

// invoke native
    assert(vi.getMapping().scalarFn && "no scalar function to vectorize provided");
    assert(vi.getMapping().vectorFn && "user has to provide empty simd function");
    assert(vi.getMapping().scalarFn != vi.getMapping().vectorFn &&
           "scalar function and simd function must not be the same");
    native::NatBuilder builder(platformInfo, vi, dtree);
    builder.vectorize();

    return true;
  }


} /* namespace rv */

