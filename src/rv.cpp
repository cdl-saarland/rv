//===- rv.cpp ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @authors kloessner
//

#include <llvm/Transforms/Utils/Cloning.h>
#include <llvm/IR/LegacyPassManager.h>

#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/Dominators.h>
#include <llvm/Analysis/PostDominators.h>
#include <llvm/IR/Verifier.h>

#include "rv/rv.h"
#include "rv/analysis/DFG.h"
#include "rv/analysis/VectorizationAnalysis.h"
#include "rv/analysis/maskAnalysis.h"
#include "rv/transform/maskGenerator.h"
#include "rv/transform/loopExitCanonicalizer.h"
#include "rv/analysis/ABAAnalysis.h"

#include "rv/PlatformInfo.h"
#include "rv/vectorizationInfo.h"
#include "rv/analysis/DFG.h"
#include "rv/analysis/maskAnalysis.h"

#include "rv/transform/Linearizer.h"

#include "rv/transform/structOpt.h"

#include "native/nativeBackendPass.h"
#include "native/NatBuilder.h"

#include "utils/rvTools.h"

#include "rvConfig.h"


namespace {

void removeTempFunction(Module& mod, const std::string& name)
{
    if (Function* tmpFn = mod.getFunction(name))
    {
        assert (tmpFn->use_empty());
        tmpFn->eraseFromParent();
    }
}

void
removeUnusedRVLibFunctions(Module& mod)
{
#define REMOVE_LIB_FN(name) \
    { \
        Function* fn = mod.getFunction(#name); \
        if (fn && fn->use_empty()) \
        { \
            fn->eraseFromParent(); \
        } \
    } \
    ((void)0)

    REMOVE_LIB_FN(log2_ps);
    REMOVE_LIB_FN(exp2_ps);
    REMOVE_LIB_FN(log_ps);
    REMOVE_LIB_FN(exp_ps);
    REMOVE_LIB_FN(sin_ps);
    REMOVE_LIB_FN(cos_ps);
    REMOVE_LIB_FN(sincos_ps);
    REMOVE_LIB_FN(fabs_ps);
    REMOVE_LIB_FN(pow_ps);
    REMOVE_LIB_FN(log2256_ps);
    REMOVE_LIB_FN(exp2256_ps);
    REMOVE_LIB_FN(log256_ps);
    REMOVE_LIB_FN(exp256_ps);
    REMOVE_LIB_FN(sin256_ps);
    REMOVE_LIB_FN(cos256_ps);
    REMOVE_LIB_FN(sincos256_ps);
    REMOVE_LIB_FN(fabs256_ps);
    REMOVE_LIB_FN(pow256_ps);

#undef REMOVE_LIB_FN
}

}




namespace rv {

VectorizerInterface::VectorizerInterface(PlatformInfo & _platInfo)
        : platInfo(_platInfo)
{
  addPredicateIntrinsics();
}

void
VectorizerInterface::addPredicateIntrinsics() {
    for (Function & func : platInfo.getModule()) {
        bool isMaskPredicate = false;
        if (func.getName() == "rv_any") {
            isMaskPredicate = true;
        } else if ((func.getName() == "rv_all")) {
            isMaskPredicate = true;
        } else if ((func.getName() == "rv_extract")) {
            isMaskPredicate = true;
        }

        if (isMaskPredicate) {
          VectorMapping predMapping(
                              &func,
                              &func,
                              0, // no specific vector width
                              -1, //
                              VectorShape::uni(),
                              {VectorShape::varying()}
                              );
          platInfo.addSIMDMapping(predMapping);
        }
    }
}

void
VectorizerInterface::analyze(VectorizationInfo& vectorizationInfo,
                             const CDG& cdg,
                             const DFG& dfg,
                             const LoopInfo& loopInfo,
                             const PostDominatorTree& postDomTree,
                             const DominatorTree& domTree)
{
    VectorizationAnalysis vea(platInfo,
                                  vectorizationInfo,
                                  cdg,
                                  dfg,
                                  loopInfo,
                                  domTree, postDomTree);

    ABAAnalysis abaAnalysis(platInfo,
                            vectorizationInfo,
                            loopInfo,
                            postDomTree,
                            domTree);

    auto & scalarFn = vectorizationInfo.getScalarFunction();
    vea.analyze(scalarFn);
    abaAnalysis.analyze(scalarFn);
}

MaskAnalysis*
VectorizerInterface::analyzeMasks(VectorizationInfo& vectorizationInfo, const LoopInfo& loopinfo)
{
    MaskAnalysis* maskAnalysis = new MaskAnalysis(platInfo, vectorizationInfo, loopinfo);
    maskAnalysis->analyze(vectorizationInfo.getScalarFunction());
    return maskAnalysis;
}

bool
VectorizerInterface::generateMasks(VectorizationInfo& vecInfo,
                                   MaskAnalysis& maskAnalysis,
                                   const LoopInfo& loopInfo)
{
    MaskGenerator maskgenerator(vecInfo, maskAnalysis, loopInfo);
    return maskgenerator.generate(vecInfo.getScalarFunction());
}

bool
VectorizerInterface::linearizeCFG(VectorizationInfo& vectorizationInfo,
                                  MaskAnalysis& maskAnalysis,
                                  LoopInfo& loopInfo,
                                  DominatorTree& domTree)
{
    // use a fresh domtree here
    DominatorTree fixedDomTree(vectorizationInfo.getScalarFunction()); // FIXME someone upstream broke the domtree
    domTree.recalculate(vectorizationInfo.getScalarFunction());
    Linearizer linearizer(vectorizationInfo, maskAnalysis, fixedDomTree, loopInfo);

    IF_DEBUG {
      errs() << "--- VecInfo before Linearizer ---\n";
      vectorizationInfo.dump();
    }

    linearizer.run();

    IF_DEBUG {
      errs() << "--- VecInfo after Linearizer ---\n";
      vectorizationInfo.dump();
    }

    return true;
}

bool
VectorizerInterface::vectorize(VectorizationInfo &vecInfo, const DominatorTree &domTree)
{
  StructOpt sopt(vecInfo, platInfo.getDataLayout());
  sopt.run();

// vectorize with native
//    native::NatBuilder natBuilder(platInfo, vecInfo, domTree);
//    natBuilder.vectorize();
  legacy::FunctionPassManager fpm(vecInfo.getScalarFunction().getParent());
  fpm.add(new MemoryDependenceAnalysis());
  fpm.add(new ScalarEvolutionWrapperPass());
  fpm.add(new NativeBackendPass(&vecInfo, &platInfo, &domTree));
  fpm.doInitialization();
  fpm.run(vecInfo.getScalarFunction());
  fpm.doFinalization();

    return true;
}

// TODO move this in a public header
static bool
IsPredicateIntrinsic(Function & func) {
  return (func.getName() == "rv_any") ||
         (func.getName() == "rv_all") ||
         (func.getName() == "rv_extract");
}

void
VectorizerInterface::finalize(VectorizationInfo & vecInfo)
{
    const auto & scalarName = vecInfo.getScalarFunction().getName();

    Function& finalFn = vecInfo.getVectorFunction();

    assert (!finalFn.isDeclaration());

    IF_DEBUG {
      rv::writeFunctionToFile(finalFn, (finalFn.getName() + ".ll").str());
    }
    // Remove all functions that were linked in but are not used.
    // TODO: This is a very bad temporary hack to get the "noise" project
    //       running. We should add functions lazily.
    removeUnusedRVLibFunctions(platInfo.getModule());

    // Remove temporary functions if inserted during mask generation.
    removeTempFunction(platInfo.getModule(), "entryMaskUseFn");
    removeTempFunction(platInfo.getModule(), "entryMaskUseFnSIMD");

    IF_DEBUG {
            outs() << "### Whole-Function Vectorization of function '" << scalarName
            << "' SUCCESSFUL!\n";
    }
}

static void
ReplaceByIdentity(Function & func) {
  for (
      auto itUse = func.use_begin();
      itUse != func.use_end();
      itUse = func.use_begin())
  {
    auto *user = itUse->getUser();

    if (!isa<CallInst>(user)) {
      errs() << "Non Call: " << *user << "\n";
    }

    auto * call = cast<CallInst>(user);
    call->replaceAllUsesWith(call->getOperand(0));
    call->eraseFromParent();
  }
}

void
lowerPredicateIntrinsics(Module & mod) {
  auto * anyFunc = mod.getFunction("rv_any");
  if (anyFunc) ReplaceByIdentity(*anyFunc);
  auto * allFunc = mod.getFunction("rv_all");
  if (allFunc) ReplaceByIdentity(*allFunc);
  auto * extractFunc = mod.getFunction("rv_extract");
  if (extractFunc) ReplaceByIdentity(*extractFunc);
}

void
lowerPredicateIntrinsics(Function & func) {
  for (auto & block : func) {
    BasicBlock::iterator itStart = block.begin(), itEnd = block.end();
    for (BasicBlock::iterator it = itStart; it != itEnd; ) {
      auto * inst = &*it++;
      auto * call = dyn_cast<CallInst>(inst);
      if (!call) continue;

      auto * callee = call->getCalledFunction();
      if (callee && IsPredicateIntrinsic(*callee)) {
        Value *predicate = call->getArgOperand(0);
        call->replaceAllUsesWith(predicate);
        call->eraseFromParent();
      }
    }
  }
}



} // namespace rv
