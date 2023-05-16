//===- src/transform/AutoMathPass.cpp - math auto-vectorization pass  --*- C++
//-*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This path automatically supplement vector math functions using RV's resolver
// API
//
//===----------------------------------------------------------------------===//

#include "rv/legacy/LinkAllPasses.h"
#include "rv/passes/AutoMathPass.h"

#include "rv/analysis/costModel.h"
#include "rv/analysis/reductionAnalysis.h"
#include "rv/region/LoopRegion.h"
#include "rv/region/Region.h"
#include "rv/resolver/resolvers.h"
#include "rv/rv.h"
#include "rv/transform/remTransform.h"
#include "rv/transform/singleReturnTrans.h"
#include "rv/utils.h"
#include "rv/vectorMapping.h"

#include "rv/region/FunctionRegion.h"
#include "rv/rvDebug.h"
#include "rvConfig.h"

#include "llvm/ADT/Triple.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"
#include "llvm/InitializePasses.h"
#include "llvm/Passes/PassBuilder.h"
#include "rv/passes/PassManagerSession.h"

#include "llvm/Analysis/BranchProbabilityInfo.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/MemoryDependenceAnalysis.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/ScalarEvolutionExpressions.h"

#include "llvm/ADT/Sequence.h"
#include "llvm/Transforms/Utils/Cloning.h"

#include "report.h"
#include <cassert>
#include <map>
#include <sstream>

using namespace rv;
using namespace llvm;

#if 0
#define IF_DEBUG_AM if (true)
#else
#define IF_DEBUG_AM IF_DEBUG
#endif

static bool IsForVE(Function &F) {
  Triple Triple(F.getParent()->getTargetTriple());
  return Triple.isVE();
}

static std::pair<Type *, unsigned> UnvectorizeType(Type *Ty) {
  if (auto VecTy = dyn_cast<FixedVectorType>(Ty)) {
    return std::make_pair<>(VecTy->getElementType(), VecTy->getNumElements());
  }
  if (auto FuncTy = dyn_cast<FunctionType>(Ty)) {
    // scalarize all argument types (succeeding with the entire function type if
    // all types gree)
    Optional<unsigned> FuncVecWidth;
    unsigned NumParams = FuncTy->getNumParams();
    std::vector<Type *> ScaArgTypeVec;
    for (unsigned i = 0; i < NumParams; ++i) {
      auto ParTy = FuncTy->getParamType(i);
      auto UnvecParTyWidth = UnvectorizeType(ParTy);
      auto ScaParTy = UnvecParTyWidth.first;
      unsigned ArgVecWidth = UnvecParTyWidth.second;
      if (!FuncVecWidth) {
        FuncVecWidth = ArgVecWidth;
      } else if (FuncVecWidth != ArgVecWidth) {
        return std::make_pair<>(FuncTy, 1);
      }
      ScaArgTypeVec.push_back(ScaParTy);
    }
    Type *ScaRetTy = nullptr;
    if (FuncTy->getReturnType()->isVoidTy()) {
      ScaRetTy = FuncTy->getReturnType();
    } else {
      auto UnvecRetTyWidth = UnvectorizeType(FuncTy->getReturnType());
      ScaRetTy = UnvecRetTyWidth.first;
      unsigned RetVecWidth = UnvecRetTyWidth.second;
      if (!FuncVecWidth) {
        FuncVecWidth = RetVecWidth;
      } else if (FuncVecWidth != RetVecWidth) {
        return std::make_pair<>(FuncTy, 1);
      }
    }

    auto ScaFuncTy =
        FunctionType::get(ScaRetTy, ScaArgTypeVec, FuncTy->isVarArg());
    return std::make_pair<>(ScaFuncTy, FuncVecWidth.value_or(1));
  }
  return std::make_pair<>(Ty, 1);
}

static bool IsArgumentKind(Intrinsic::IITDescriptor IT) {
  using IIT = Intrinsic::IITDescriptor;
  return (IT.Kind == IIT::Argument || IT.Kind == IIT::ExtendArgument ||
          IT.Kind == IIT::TruncArgument || IT.Kind == IIT::HalfVecArgument ||
          IT.Kind == IIT::SameVecWidthArgument ||
          IT.Kind == IIT::PtrToArgument || IT.Kind == IIT::VecElementArgument ||
          IT.Kind == IIT::Subdivide2Argument ||
          IT.Kind == IIT::Subdivide4Argument ||
          IT.Kind == IIT::VecOfBitcastsToInt);
}

static bool VerifyType(Intrinsic::IITDescriptor::IITDescriptorKind K,
                       Type *ScaType) {
  using IK = Intrinsic::IITDescriptor::IITDescriptorKind;
  switch (K) {
  case IK::VecOfAnyPtrsToElt:
  case IK::Vector:
    return isa<VectorType>(ScaType);

  default:
    return true;
  }
}

static Function *DeclareIntrinsic(Module &M, Intrinsic::ID ID,
                                  FunctionType *DestFuncTy) {
  SmallVector<Intrinsic::IITDescriptor, 2> IITVec;
  Intrinsic::getIntrinsicInfoTableEntries(ID, IITVec);
  std::vector<Type *> IntrinTypeVec;
  int ArgPos = -1;
  for (auto It : IITVec) {
    ++ArgPos;
    bool IsMatchArg =
        IsArgumentKind(It) &&
        It.getArgumentKind() == Intrinsic::IITDescriptor::AK_MatchType;
    if (IsMatchArg)
      continue;

    // Sanitize argument.
    auto *ScaArgType = DestFuncTy->getParamType(ArgPos);
    if (!VerifyType(It.Kind, ScaArgType))
      return nullptr;

    IntrinTypeVec.push_back(ScaArgType);
  }

  return Intrinsic::getDeclaration(&M, ID, IntrinTypeVec);
}

struct FuncSession {
  // Vectorizer handles
  Function &F;
  Config RVConfig;
  PlatformInfo PlatInfo;
  VectorizerInterface Vectorizer;

  // Vectorization jobs
  struct ResolverJob {
    std::unique_ptr<FunctionResolver> ResolverPtr;
    std::set<CallInst *> CallInsts;

    ResolverJob() {}
    ResolverJob(std::unique_ptr<FunctionResolver> &&MovingResPtr, CallInst &CI)
        : ResolverPtr(std::move(MovingResPtr)), CallInsts() {
      CallInsts.insert(&CI);
    }
  };

  using VectorIntrinKey = std::pair<Function *, unsigned>;
  std::map<VectorIntrinKey, ResolverJob> VectorJobs;

  bool addVectorizeJob(VectorIntrinKey FuncWidthKey, CallInst &CI) {
    auto ScalarCallee = FuncWidthKey.first;
    unsigned CalleeWidth = FuncWidthKey.second;

    // Check for a cached entry
    auto It = VectorJobs.find(FuncWidthKey);
    if (It != VectorJobs.end()) {
      return true;
    }

    // Check whether we can get a resolver for this
    VectorShapeVec VecArgShapes(CI.arg_size(), VectorShape::varying());

    auto ResolverPtr = PlatInfo.getResolver(ScalarCallee->getName(),
                                            *ScalarCallee->getFunctionType(),
                                            VecArgShapes, CalleeWidth, false);
    if (!ResolverPtr) {
      IF_DEBUG_AM { errs() << "\tcould not get a resolver"; }
      return false;
    }

    VectorJobs[FuncWidthKey] = ResolverJob(std::move(ResolverPtr), CI);
    return true;
  }

  FuncSession(Function &F, TargetTransformInfo &TTI, TargetLibraryInfo &TLI)
      : F(F), RVConfig(Config::createForFunction(F)),
        PlatInfo(*F.getParent(), &TTI, &TLI), Vectorizer(PlatInfo, RVConfig) {
    addSleefResolver(RVConfig, PlatInfo);
  }

  bool inspectCallSite(CallInst &C) {
    if (C.getIntrinsicID() == Intrinsic::not_intrinsic)
      return false;

    // Un-vectorize the callsite, generating a scalar intrinsic declaration
    // on-the-fly
    IF_DEBUG_AM { errs() << "Inspecting : " << C << "\n"; }
    auto ScaFuncTyWidth = UnvectorizeType(C.getFunctionType());
    auto ScaFuncTy = cast<FunctionType>(ScaFuncTyWidth.first);
    unsigned VecWidth = ScaFuncTyWidth.second;
    if (VecWidth == 1) {
      IF_DEBUG_AM { errs() << "\tcould not un-vectorize\n"; }
      return false;
    }

    IF_DEBUG_AM {
      errs() << "Unvectorized(" << VecWidth << ") : " << *ScaFuncTy << "\n";
    }
    auto IntrinID = C.getIntrinsicID();
    auto ScaIntrinFunc = DeclareIntrinsic(*F.getParent(), IntrinID, ScaFuncTy);

    if (!ScaIntrinFunc) {
      IF_DEBUG_AM {
        errs() << "\t Scalarized signature is incomaptible with intrinsic "
                  "constraints!\n";
      }
      return false;
    }

    IF_DEBUG_AM { errs() << "Scalar intrinsic : " << *ScaIntrinFunc << "\n"; }

    // Add the intrinsic to our worklist
    return addVectorizeJob(std::make_pair<>(ScaIntrinFunc, VecWidth), C);
  }

  bool run() {
    // Collect call sites to vectorize
    for (auto &BB : F) {
      for (auto &I : BB) {
        auto C = dyn_cast<CallInst>(&I);
        if (!C)
          continue;
        bool AutoMathed = inspectCallSite(*C);
        IF_DEBUG_AM {
          if (AutoMathed)
            errs() << "\twill be auto-mathed!\n";
        }
      }
    }

    if (VectorJobs.empty()) {
      IF_DEBUG_AM { errs() << "Done. No math to vectorize found.\n"; }
      return false;
    }

    // Vectorize jobs
    IF_DEBUG_AM {
      errs() << "Auto-vectorizing " << VectorJobs.size() << " in function "
             << F.getName() << "\n";
    }
    for (auto &ItJob : VectorJobs) {
      ResolverJob &ResJob = ItJob.second;
      auto &VecMathFunc = ResJob.ResolverPtr->requestVectorized();
      // Use a fastcc and resolve conflicts (unless this is an intrinsic)
      if (VecMathFunc.getIntrinsicID() == Intrinsic::not_intrinsic) {
        VecMathFunc.setLinkage(GlobalVariable::WeakAnyLinkage);
        VecMathFunc.setCallingConv(CallingConv::Fast);
      }
      for (auto &VecMathCI : ResJob.CallInsts) {
        VecMathCI->setCalledFunction(&VecMathFunc);
      }
    }
    IF_DEBUG_AM { errs() << "Done. All vector math supplemented.\n"; }

    VectorJobs.clear();
    return true;
#if 0
    std::string IntrinName;
    FunctionType *ScaFuncTy;
    unsigned VectorWidth;

    VectorShapeVec ArgShapes;

    PlatInfo.getResolver(IntrinName, ScaFuncTy, ArgShapes, VectorWidth, false);
#endif
  }
};

AutoMathPass::AutoMathPass() : PMS()
{}

bool AutoMathPass::run(Function &F) {
  // Should we vectorize math here?
  if (!IsForVE(F))
    return false;

  // Setup the vectorizer
  auto &TLI = PMS.FAM.getResult<TargetLibraryAnalysis>(F);
  auto &TTI = PMS.FAM.getResult<TargetIRAnalysis>(F);
  FuncSession FuncSession(F, TTI, TLI);
  return FuncSession.run();
}

bool AutoMathPass::run(Module &M) {
  bool Changed = false;
  // analyze math function usage in all VE functions
  for (auto &func : M) {
    if (func.isDeclaration())
      continue;

    Changed |= run(func);
  }

  return Changed;
}

///// New PM Pass /////

AutoMathWrapperPass::AutoMathWrapperPass() {}

llvm::PreservedAnalyses
AutoMathWrapperPass::run(llvm::Module &M, llvm::ModuleAnalysisManager &MAM) {
  AutoMathPass AMP;
  if (AMP.run(M))
    return PreservedAnalyses::none();
  else
    return PreservedAnalyses::all();
}

///// Old PM Pass /////
void AutoMathLegacyPass::getAnalysisUsage(llvm::AnalysisUsage &AU) const {}
bool AutoMathLegacyPass::runOnModule(llvm::Module &M) {
  AutoMathPass AMP;
  return AMP.run(M);
}

char AutoMathLegacyPass::ID = 0;

ModulePass *rv::createAutoMathLegacyPass() { return new AutoMathLegacyPass(); }

INITIALIZE_PASS_BEGIN(AutoMathLegacyPass, "rv-automath",
                      "RV - Auto-vectorize math functions", false, false)
INITIALIZE_PASS_END(AutoMathLegacyPass, "rv-automath",
                    "RV - Auto-vectorize math functions", false, false)
