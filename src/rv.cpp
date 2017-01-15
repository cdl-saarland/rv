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
#include <llvm/Analysis/PostDominators.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Verifier.h>

#include <rv/rv.h>
#include <rv/pda/ProgramDependenceAnalysis.h>
#include <rv/pda/DFG.h>
#include <rv/analysis/maskAnalysis.h>
#include <rv/analysis/MetadataMaskAnalyzer.h>
#include <rv/transform/maskGenerator.h>
#include <rv/analysis/vectorizationAnalysis.h>
#include <rv/transform/loopExitCanonicalizer.h>
#include <rv/pda/ABAAnalysis.h>

#include "rv/transform/Linearizer.h"

#include <native/nativeBackendPass.h>
#include <native/NatBuilder.h>


#include "rvConfig.h"


namespace {

void removeTempFunction(Module* mod, const std::string& name)
{
    assert (mod);
    if (Function* tmpFn = mod->getFunction(name))
    {
        assert (tmpFn->use_empty());
        tmpFn->eraseFromParent();
    }
}

void
removeUnusedRVLibFunctions(Module* mod)
{
#define REMOVE_LIB_FN(name) \
    { \
        Function* fn = mod->getFunction(#name); \
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

VectorizerInterface::VectorizerInterface(RVInfo& rvInfo, Function* scalarCopy)
        : mInfo(rvInfo), mScalarFn(scalarCopy)
{
    const Function& scalarFunction = *rvInfo.mScalarFunction;
    const Function& simdFunction = *rvInfo.mSimdFunction;
    const std::string& scalarName = scalarFunction.getName();
    const std::string& simdName = simdFunction.getName();

    if (&scalarFunction != &simdFunction) {
      if (scalarFunction.isVarArg())
      {
          throw std::logic_error("ERROR while vectorizing function in module '"
          + mInfo.mModule->getModuleIdentifier() + "': function '"
          + scalarName + "' has a variable argument list (not supported)!");
      }

      if (scalarFunction.isDeclaration())
      {
          throw std::logic_error("ERROR while vectorizing function in module '"
          + mInfo.mModule->getModuleIdentifier() + "': scalar source function '"
          + scalarName + "' has no body!");
      }

      if (!simdFunction.isDeclaration())
      {
          assert (!simdFunction.getBasicBlockList().empty() &&
                  "Function is no declaration but does not have basic blocks?!");
          throw std::logic_error("ERROR while vectorizing function in module '"
          + mInfo.mModule->getModuleIdentifier() + "': extern target function '"
          + simdName + "' must not have a body!");
      }

      if (!verifyFunctionSignaturesMatch(*mScalarFn, simdFunction))
      {
          throw std::logic_error("ERROR: Function signatures do not match!\n");
      }
    }

    IF_DEBUG {
      scalarFunction.print(outs());
      rv::writeFunctionToFile(scalarFunction, scalarName+".ll");
      rv::writeModuleToFile(*mInfo.mModule, scalarName+".mod.ll");
      verifyFunction(scalarFunction);

      simdFunction.print(outs());
    }

    mInfo.configure();

    addPredicateInstrinsics();
}

/* VectorizerInterface::~VectorizerInterface()
{
    // Delete the copy
    delete mScalarFn;
}*/

bool
VectorizerInterface::addSIMDSemantics(const Function& f,
                                      const bool      isOpUniform,
                                      const bool      isOpVarying,
                                      const bool      isOpSequential,
                                      const bool      isOpSequentialGuarded,
                                      const bool      isResultUniform,
                                      const bool      isResultVector,
                                      const bool      isResultScalars,
                                      const bool      isAligned,
                                      const bool      isIndexSame,
                                      const bool      isIndexConsecutive)
{
    // We also map the function to itself so that instruction vectorization
    // "knows" it and replaces it by itself.
    const bool mayHaveSideEffects = !isOpUniform && !isResultUniform;
    mInfo.addSIMDMapping(f, f, -1, mayHaveSideEffects);

    return mInfo.addSIMDSemantics(f,
                                  isOpUniform,
                                  isOpVarying,
                                  isOpSequential,
                                  isOpSequentialGuarded,
                                  isResultUniform,
                                  isResultVector,
                                  isResultScalars,
                                  isAligned,
                                  isIndexSame,
                                  isIndexConsecutive);
}

void
VectorizerInterface::addPredicateInstrinsics() {
    FunctionType * simdPredTy = FunctionType::get(mInfo.mScalarBoolTy, ArrayRef<Type*>(mInfo.mVectorTyBoolSIMD), false);
    IF_DEBUG errs() << "SIMD Mapping for mask intrinsic: " << *simdPredTy << "\n";
    for (Function & func : *mInfo.mModule) {
        bool isMaskPredicate = false;
        if (func.getName() == "rv_any") {
            isMaskPredicate = true;
            // Function * anySimdFunc = cast<Function>(mInfo->mModule->getOrInsertFunction("rv_any_simd", simdPredTy));
            // addSIMDMapping(func, *anySimdFunc, -1, false);
        } else if ((func.getName() == "rv_all")) {
            isMaskPredicate = true;
            // Function * allSimdFunc = cast<Function>(mInfo->mModule->getOrInsertFunction("rv_all_simd", simdPredTy));
            // addSIMDMapping(func, *allSimdFunc, -1, false);
        }

        if (isMaskPredicate) {
            addSIMDSemantics(func,
                             true, // isOpUniform
                             false,// isOpVarying
                             false,// isOpSequential
                             false,// isOpSequentialGuarded
                             true, // isResultUniform
                             false,// isResultVector
                             false,// isResultScalars
                             true, // isAligned
                             true, // isIndexSame
                             false // isIndexConsecutive
            );
        }
    }
}

bool
VectorizerInterface::verifyVectorizedType(Type* scalarType, Type* vecType)
{
    // Check for uniform equivalence.
    if (scalarType == vecType) return true;
    if (rv::typesMatch(scalarType, vecType)) return true;

    // Check for varying equivalence.
    Type* vectorizedType = rv::vectorizeSIMDType(scalarType, mInfo.mVectorizationFactor);
    if (rv::typesMatch(vecType, vectorizedType)) return true;

    return false;
}

bool
VectorizerInterface::verifyFunctionSignaturesMatch(const Function& f,
                                                   const Function& f_SIMD)
{
    if (f.arg_size() != f_SIMD.arg_size())
    {
        errs() << "ERROR: number of function arguments does not match!\n";
        return false;
    }

    // check argument and return types
    Type* scalarReturnType      = f.getReturnType();
    Type* foundVectorReturnType = f_SIMD.getReturnType();

    if (!verifyVectorizedType(scalarReturnType, foundVectorReturnType))
    {
        errs()
        << "ERROR: return type does not match!\n"
        << "       scalar      : " << *scalarReturnType << "\n"
        << "       vec found   : " << *foundVectorReturnType << "\n";
        return false;
    }

    for (auto A = f.arg_begin(), extA = f_SIMD.arg_begin();
         A != f.arg_end() && extA != f_SIMD.arg_end();
         ++A, ++extA)
    {
        Type* scalarType = A->getType();
        Type* foundVectorType = extA->getType();

        if (!verifyVectorizedType(scalarType, foundVectorType))
        {
            errs()
            << "ERROR: argument type does not match: " << *A << "\n"
            << "       scalar      : " << *scalarType << "\n"
            << "       vec found   : " << *foundVectorType << "\n";
            return false;
        }
    }

    return true;
}

void
VectorizerInterface::analyze(VectorizationInfo& vectorizationInfo,
                             const CDG& cdg,
                             const DFG& dfg,
                             const LoopInfo& loopInfo,
                             const PostDominatorTree& postDomTree,
                             const DominatorTree& domTree)
{
    MetadataMaskAnalyzer maskAnalyzer(vectorizationInfo);

    PDA programDependenceAnalysis(vectorizationInfo,
                                  cdg,
                                  dfg,
                                  mInfo.getVectorFuncMap(),
                                  loopInfo);

    ABAAnalysis abaAnalysis(vectorizationInfo,
                            mInfo.getVectorFuncMap(),
                            loopInfo,
                            postDomTree,
                            domTree);

    programDependenceAnalysis.analyze(*mScalarFn);
    abaAnalysis.analyze(*mScalarFn);
    maskAnalyzer.markMasks(*mScalarFn);
}

MaskAnalysis*
VectorizerInterface::analyzeMasks(VectorizationInfo& vectorizationInfo, const LoopInfo& loopinfo)
{
    MaskAnalysis* maskAnalysis = new MaskAnalysis(vectorizationInfo, mInfo, loopinfo);
    maskAnalysis->analyze(*mScalarFn);
    return maskAnalysis;
}

bool
VectorizerInterface::generateMasks(VectorizationInfo& vectorizationInfo,
                                   MaskAnalysis& maskAnalysis,
                                   const LoopInfo& loopInfo)
{
    MaskGenerator maskgenerator(mInfo, vectorizationInfo, maskAnalysis, loopInfo);
    return maskgenerator.generate(*mScalarFn);
}

bool
VectorizerInterface::linearizeCFG(VectorizationInfo& vectorizationInfo,
                                  MaskAnalysis& maskAnalysis,
                                  LoopInfo& loopInfo,
                                  PostDominatorTree& postDomTree,
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
VectorizerInterface::vectorize(PlatformInfo &platformInfo, VectorizationInfo &vecInfo, const DominatorTree &domTree)
{
#if 0
    // Strip legacy metadata calls
    std::vector<Instruction *> killList;
    for (auto &block : *vecInfo.getMapping().scalarFn) {
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
#endif

    // vectorize with native
    native::NatBuilder natBuilder(platformInfo, vecInfo, domTree);
    natBuilder.vectorize();

    return true;
}

// TODO move this in a public header
static bool
IsPredicateIntrinsic(Function & func) {
  return (func.getName() == "rv_any") ||
         (func.getName() == "rv_all");
}

void
VectorizerInterface::finalize()
{
    const auto & scalarName = mScalarFn->getName();

    Function* finalFn = mInfo.mSimdFunction;

    assert (finalFn);
    assert (!finalFn->isDeclaration());

    IF_DEBUG {
      rv::writeFunctionToFile(*finalFn, (finalFn->getName() + ".ll").str());
    }
    // Remove all functions that were linked in but are not used.
    // TODO: This is a very bad temporary hack to get the "noise" project
    //       running. We should add functions lazily.
    removeUnusedRVLibFunctions(mInfo.mModule);

    // Remove temporary functions if inserted during mask generation.
    removeTempFunction(mInfo.mModule, "entryMaskUseFn");
    removeTempFunction(mInfo.mModule, "entryMaskUseFnSIMD");

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
