//===- LoopVectorizer.cpp - Vectorize Loops  ----------------===//
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

#include "rv/transform/LoopVectorizer.h"
#include "rv/LinkAllPasses.h"

#include "rv/analysis/costModel.h"
#include "rv/analysis/loopAnnotations.h"
#include "rv/analysis/reductionAnalysis.h"
#include "rv/region/LoopRegion.h"
#include "rv/region/Region.h"
#include "rv/resolver/resolvers.h"
#include "rv/rv.h"
#include "rv/transform/remTransform.h"
#include "rv/vectorMapping.h"

#include "rv/config.h"
#include "rv/rvDebug.h"
#ifdef RV_ENABLE_LOOPDIST
#include "rv/transform/loopDistTrans.h"
#endif
#include "rvConfig.h"

#include "llvm/IR/Dominators.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Verifier.h"
#include "llvm/InitializePasses.h"
#include "llvm/Passes/PassBuilder.h"

#include "llvm/ADT/GraphTraits.h"
#include "llvm/Analysis/BranchProbabilityInfo.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/MemoryDependenceAnalysis.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/ScalarEvolutionExpressions.h"

#include "llvm/Transforms/Utils/Cloning.h"

#include "report.h"
#include <map>

using namespace rv;
using namespace llvm;

// typedef DomTreeNodeBase<BasicBlock*> DomTreeNode;

#if 1
#define IF_DEBUG_LV IF_DEBUG
#else
#define IF_DEBUG_LV if (true)
#endif

bool LoopVectorizer::canVectorizeLoop(Loop &L) {
  if (!L.isAnnotatedParallel())
    return false;

  BasicBlock *ExitingBlock = L.getExitingBlock();
  if (!ExitingBlock)
    return false;

  if (!isa<BranchInst>(ExitingBlock->getTerminator()))
    return false;

  return true;
}

int LoopVectorizer::getTripAlignment(Loop &L) {
  int tripCount = getTripCount(L);
  if (tripCount > 0)
    return tripCount;
  return 1;
}

bool LoopVectorizer::canAdjustTripCount(Loop &L, int VectorWidth,
                                        int TripCount) {
  if (VectorWidth == TripCount)
    return true;

  return false;
}

int LoopVectorizer::getTripCount(Loop &L) {
  auto &SE = FAM.getResult<ScalarEvolutionAnalysis>(*F);
  auto *BTC = dyn_cast<SCEVConstant>(SE.getBackedgeTakenCount(&L));
  if (!BTC)
    return -1;

  int64_t BTCVal = BTC->getValue()->getSExtValue();
  if (BTCVal <= 1 || ((int64_t)((int)BTCVal)) != BTCVal)
    return -1;

  return BTCVal + 1;
}

bool LoopVectorizer::scoreLoop(LoopJob &LJ, LoopScore &LS, Loop &L) {
  LoopMD mdAnnot = GetLoopAnnotation(L);

  if (enableDiagOutput) {
    Report() << "loopVecPass: ";
    mdAnnot.print(ReportContinue()) << "\n";
  }

  // only trigger on annotated loops
  if (!mdAnnot.vectorizeEnable.safeGet(false)) {
    if (enableDiagOutput)
      Report() << "loopVecPass skip " << L.getName()
               << " . not explicitly triggered.\n";
    return false;
  }

  // skip if already vectorized
  if (mdAnnot.alreadyVectorized.safeGet(false)) {
    // too verbose
    if (enableDiagOutput)
      Report() << "loopVecPass skip " << L.getName()
               << " . already vectorized.\n";
    return false;
  }

  LJ.DepDist = mdAnnot.minDepDist.safeGet(ParallelDistance);

  // skip if iteration dependence distance precludes vectorization
  if (LJ.DepDist <= 1) {
    if (enableDiagOutput)
      Report() << "loopVecPass skip " << L.getName()
               << " . Min dependence distance was " << LJ.DepDist << "\n";
    return false;
  }

  // use the explicitVectorWidth (if specified). Otherwise
  bool hasFixedWidth = mdAnnot.explicitVectorWidth.isSet();
  LJ.VectorWidth =
      hasFixedWidth ? mdAnnot.explicitVectorWidth.get() : LJ.DepDist;

  // environment user override
  char *userWidthText = getenv("RV_FORCE_WIDTH");
  if (userWidthText) {
    hasFixedWidth = true;
    LJ.VectorWidth = atoi(userWidthText);
    if (enableDiagOutput)
      Report()
          << "loopVecPass: with user-provided vector width (RV_FORCE_WIDTH="
          << LJ.VectorWidth << ")\n";
  }

  // Narrow VectorWidth to constant trip count (where applicable).
  int KnownTripCount = getTripCount(L);
  if (KnownTripCount > 1) {
    if (LJ.VectorWidth > (size_t) KnownTripCount) {
      LJ.VectorWidth = KnownTripCount;
      if (enableDiagOutput) {
        Report() << "loopVecPass, costModel: narrowing to known trip count: " << KnownTripCount << "\n";
      }
    }
  }

  // pick a vectorization factor (unless user override is set)
  if (!hasFixedWidth) {
    size_t initialWidth = LJ.VectorWidth == 0 ? LJ.DepDist : LJ.VectorWidth;

    // Re-fine using cost model
    CostModel costModel(vectorizer->getPlatformInfo(), config);
    LoopRegion tmpLoopRegionImpl(L);
    Region tmpLoopRegion(tmpLoopRegionImpl);
    size_t refinedWidth = costModel.pickWidthForRegion(
        tmpLoopRegion, initialWidth); // TODO run VA first

    if (refinedWidth <= 1) {
      if (enableDiagOutput) {
        Report() << "loopVecPass, costModel: vectorization not beneficial\n";
      }
      return false;
    } else if (refinedWidth != (size_t)LJ.VectorWidth) {
      if (enableDiagOutput) {
        Report() << "loopVecPass, costModel: refined vector width to "
                 << DepDistToString(refinedWidth) << " from ";
        if (LJ.VectorWidth > 1)
          Report() << DepDistToString(LJ.VectorWidth) << "\n";
        else
          ReportContinue() << " unbounded\n";
      }
      LJ.VectorWidth = refinedWidth;
    }
  }

  static int GlobalLoopCount = 0;

  const char * SelLoopTxt = getenv("RV_SELECT_LOOP");
  if (SelLoopTxt) {
    int SelLoop = atoi(SelLoopTxt);
    GlobalLoopCount++;

    if (SelLoop != GlobalLoopCount) {
      Report() << "loopVecPass, RV_SELECT_LOOP != " << GlobalLoopCount << ". not vectorizing!\n";
      return false;
    }
  }

  const char* SelName = getenv("RV_SELECT_NAME") ;
  if (SelName) {
    std::string NameLoopTxt = SelName;

    bool SelectByName = L.getHeader()->getName().startswith(NameLoopTxt);

    if (!SelectByName) {
      Report() << "loopVecPass, RV_SELECT_NAME != " << NameLoopTxt << ". not vectorizing!\n";
      return false;
    }
  }

  LJ.TripAlign = getTripAlignment(L);
  LJ.Header = L.getHeader();
  LS.Score = 0; // TODO compute score
  return true;
}

enum ForLoopsControl {
  Descend = 0,     // continue for_loops into child lops
  SkipChildren = 1 // do not descend into child loops
};

static void for_loops(LoopInfo &LI,
                      std::function<ForLoopsControl(Loop &)> BodyFunc) {
  std::vector<Loop *> LoopVec;
  for (auto *L : LI) {
    LoopVec.push_back(L);
  }

  while (!LoopVec.empty()) {
    auto *L = LoopVec.back();
    LoopVec.pop_back();
    ForLoopsControl Next = BodyFunc(*L);
    if (Next == SkipChildren)
      continue;
    assert(Next == Descend);
    for (auto *ChildL : *L) {
      LoopVec.push_back(ChildL);
    }
  }
}

bool LoopVectorizer::collectLoopJobs(LoopInfo & LI) {
  // TODO consider loops inside legal loops for vectorization on cost grounds.
  for_loops(LI, [&](Loop &L) {
    LoopJob LJ;
    LoopScore LS;

    // cost & legality
    bool Legal = scoreLoop(LJ, LS, L);
    if (!Legal)
      return Descend;

    LoopsToPrepare.emplace_back(LJ);
    return SkipChildren;
  });

  return !LoopsToPrepare.empty();
}

PreparedLoop LoopVectorizer::transformToVectorizableLoop(
    Loop &L, int VectorWidth, int tripAlign, ValueSet &uniformOverrides) {
  IF_DEBUG {
    errs() << "\tPreparing loop structure of " << L.getName() << "\n";
  }

  // try to applu the remainder transformation
  ReductionAnalysis MyReda(*F, FAM);
  MyReda.analyze(L);
  RemainderTransform remTrans(*F, FAM, MyReda);
  PreparedLoop LoopPrep = remTrans.createVectorizableLoop(
      L, uniformOverrides, config.useAVL, VectorWidth, tripAlign);

  return LoopPrep;
}

bool LoopVectorizer::prepareLoopVectorization() {
  auto & LI = *FAM.getCachedResult<LoopAnalysis>(*F);
  for (LoopJob &LJ : LoopsToPrepare) {
    auto &L = *LI.getLoopFor(LJ.Header);

    Report() << "loopVecPass: Vectorize " << L.getName()
             << " with VW: " << LJ.VectorWidth
             << " , Dependence Distance: " << DepDistToString(LJ.DepDist)
             << " and TripAlignment: " << LJ.TripAlign << "\n";

    // match vector loop structure
    ValueSet uniOverrides;
    auto LoopPrep = transformToVectorizableLoop(L, LJ.VectorWidth, LJ.TripAlign,
                                                uniOverrides);
    if (!LoopPrep.TheLoop) {
      Report() << "loopVecPass: Cannot prepare vectorization of the loop\n";
      return false;
    }

#ifdef RV_ENABLE_LOOPDIST
    /// BEGIN EXPERIMENTAL SECTION
    {
      ReductionAnalysis MyReda(*F, FAM);
      MyReda.analyze(*LoopPrep.TheLoop);

      LoopComponentAnalysis LCA(*F, *LoopPrep.TheLoop, FAM, MyReda);
      LCA.run();
      LoopDistributionTransform loopDistTrans(vectorizer->getPlatformInfo(),
                                              LJ.VectorWidth, LCA);
      loopDistTrans.run();
    }
    /// END EXPERIMENTAL SECTION
#endif

    // Make sure that there is a preheader in any case
    BasicBlock * UniquePred = nullptr;
    if (!LoopPrep.TheLoop->getLoopPreheader()) {
      auto *Head = LoopPrep.TheLoop->getHeader();
      for (auto * InB : predecessors(Head)) {
        if (LoopPrep.TheLoop->contains(InB)) continue;
  
        if (!UniquePred) {
          UniquePred = InB;
        } else {
          abort(); // Multiple edges to the loop header!!!
        }
      }
  
      // break the edge
      std::string PHName = Head->getName().str() + ".ph";
      auto *PH = BasicBlock::Create(F->getContext(), PHName, F, Head);
      UniquePred->getTerminator()->replaceUsesOfWith(Head, PH);
      BranchInst::Create(Head, PH);
      for (auto & phi : Head->phis()) {
        for (unsigned i = 0; i < phi.getNumIncomingValues(); ++i) {
          if (phi.getIncomingBlock(i) == UniquePred) {
            phi.setIncomingBlock(i, PH);
          }
        }
      }
  
      auto *PHLoop = LI.getLoopFor(UniquePred);
      if (PHLoop) {
        PHLoop->addBasicBlockToLoop(PH, LI);
      }
    }
    assert(L.getLoopPreheader());

    // mark the remainder loop as un-vectorizable
    LoopMD llvmLoopMD;
    llvmLoopMD.alreadyVectorized = true;
    SetLLVMLoopAnnotations(L, std::move(llvmLoopMD));

    // clear loop annotations from our copy of the lop
    ClearLoopVectorizeAnnotations(*LoopPrep.TheLoop);

    // print configuration banner once
    if (!introduced) {
      Report() << " rv::Config: ";
      config.print(ReportContinue());
      introduced = true;
    }

    // use prepared loop instead
    LJ.Header = LoopPrep.TheLoop->getHeader();
    LoopsToVectorize.push_back(
        LoopVectorizerJob{LJ, uniOverrides, LoopPrep.EntryAVL});
  }

  LoopsToPrepare.clear();
  return !LoopsToVectorize.empty();
}

static bool IsSupportedReduction(Loop &L, Reduction &red) {
  // check that all users of the reduction are either (a) part of it or (b)
  // outside the loop
  for (auto *inst : red.elements) {
    for (auto itUser : inst->users()) {
      auto *userInst = dyn_cast<Instruction>(itUser);
      if (!userInst)
        return false; // unsupported
      if (L.contains(userInst->getParent()) && !red.elements.count(userInst)) {
        errs() << "Unsupported user of reduction: ";
        Dump(*userInst);
        return false;
      }
    }
  }

  return true;
}

bool LoopVectorizer::vectorizeLoop(LoopVectorizerJob &LVJob) {
  // auto &LI = *FAM.getCachedResult<LoopAnalysis>(*F);
  auto &LI = FAM.getResult<LoopAnalysis>(*F);
  auto &L = *LI.getLoopFor(LVJob.LJ.Header);

  // analyze the recurrence patterns of this loop
  ReductionAnalysis MyReda(*F, FAM);
  MyReda.analyze(L);

  // start vectorizing the prepared loop
  IF_DEBUG { errs() << "rv: Vectorizing loop " << L.getName() << "\n"; }

  VectorMapping targetMapping(F, F, LVJob.LJ.VectorWidth,
                              CallPredicateMode::SafeWithoutPredicate);
  LoopRegion LoopRegionImpl(L);
  Region LoopRegion(LoopRegionImpl);

  VectorizationInfo vecInfo(*F, LVJob.LJ.VectorWidth, LoopRegion);
  if (LVJob.EntryAVL) {
    vecInfo.setEntryAVL(LVJob.EntryAVL);
  }

  // Check reduction patterns of vector loop phis
  // configure initial shape for induction variable
  for (auto &inst : *LVJob.LJ.Header) {
    auto *phi = dyn_cast<PHINode>(&inst);
    if (!phi)
      continue;

    rv::StridePattern *pat = MyReda.getStrideInfo(*phi);
    VectorShape phiShape;
    if (pat) {
      IF_DEBUG { pat->dump(); }
      phiShape = pat->getShape(LVJob.LJ.VectorWidth);
    } else {
      rv::Reduction *redInfo = MyReda.getReductionInfo(*phi);
      IF_DEBUG { errs() << "loopVecPass: header phi  " << *phi << " : "; }

      // failure to derive a reduction descriptor
      if (!redInfo) {
        Report() << "\n\tskip: unrecognized phi use in vector loop "
                 << L.getName() << "\n";
        return false;
      }

      if (false) { // !IsSupportedReduction(*LoopPrep.TheLoop, *redInfo)) {
        Report() << " unsupported reduction: ";
        redInfo->print(ReportContinue());
        ReportContinue() << "\n";
        return false;
      }

      // unsupported reduction kind
      if (redInfo->kind == RedKind::Top) {
        Report() << " can not vectorize this non-trivial SCC: ";
        redInfo->print(ReportContinue());
        ReportContinue() << "\n";
        return false;
      }

      // FIXME rv codegen only supports trivial recurrences at the moment
      if (false) { // redInfo->kind == RedKind::Bot) {
        Report() << " can not vectorize this non-affine recurrence: ";
        redInfo->print(ReportContinue());
        ReportContinue() << "\n";
        return false;
      }

      // Otw, this is a privatizable reduction pattern
      IF_DEBUG { redInfo->dump(); }
      phiShape = redInfo->getShape(LVJob.LJ.VectorWidth);
    }

    IF_DEBUG {
      errs() << "header phi " << phi->getName() << " has shape "
             << phiShape.str() << "\n";
    }

    if (phiShape.isDefined())
      vecInfo.setPinnedShape(*phi, phiShape);
  }

  // set uniform overrides
  IF_DEBUG { errs() << "-- Setting remTrans uni overrides --\n"; }
  for (auto *val : LVJob.uniOverrides) {
    IF_DEBUG { errs() << "- " << *val << "\n"; }
    vecInfo.setPinnedShape(*val, VectorShape::uni());
  }

  // early math func lowering
  vectorizer->lowerRuntimeCalls(vecInfo, FAM);

  // Vectorize
  // vectorizationAnalysis
  vectorizer->analyze(vecInfo, FAM);

  if (enableDiagOutput) {
    errs() << "-- VA result --\n";
    vecInfo.dump();
    errs() << "-- EOF --\n";
  }

  IF_DEBUG Dump(*F);

  assert(L.getLoopPreheader());

  // control conversion
  vectorizer->linearize(vecInfo, FAM);

  // vectorize the prepared loop embedding it in its context
  ValueToValueMapTy vecMap;

  ScalarEvolutionAnalysis adhocAnalysis;
  adhocAnalysis.run(*F, FAM);

  bool vectorizeOk = vectorizer->vectorize(vecInfo, FAM, &vecMap);
  if (!vectorizeOk)
    llvm_unreachable("vector code generation failed");

  if (enableDiagOutput) {
    errs() << "-- Vectorized --\n";
    for (const BasicBlock *BB : L.blocks()) {
      const BasicBlock *vecB = cast<const BasicBlock>(vecMap[BB]);
      Dump(*vecB);
    }
    errs() << "-- EOF --\n";
  }
  return true;
}

bool LoopVectorizer::vectorizeLoopRegions() {
  bool Changed = false;

  for (auto &LVJob : LoopsToVectorize) {
    // FIXME repair loop info on the go
    // Rebuild analysis structured
    FAM.invalidate<DominatorTreeAnalysis>(*F);
    FAM.invalidate<PostDominatorTreeAnalysis>(*F);
    FAM.invalidate<LoopAnalysis>(*F);

    Changed |= vectorizeLoop(LVJob);
  }
  LoopsToVectorize.clear();

  return Changed;
}

bool LoopVectorizer::runOnFunction(Function &F) {
  // have we introduced ourself? (reporting output)
  enableDiagOutput = CheckFlag("LV_DIAG");
  introduced = false;

  if (getenv("RV_DISABLE"))
    return false;

  if (CheckFlag("RV_PRINT_FUNCTION")) {
    errs() << "-- RV::LoopVectorizer::runOnFunction(F) --\n";
    F.print(errs());
  }

  IF_DEBUG {
    errs() << " -- module before RV --\n";
    Dump(*F.getParent());
  }

  if (enableDiagOutput)
    Report() << "loopVecPass: run on " << F.getName() << "\n";

  bool Changed = false;

  // create private analysis infrastructure
  PassBuilder PB;
  PB.registerFunctionAnalyses(FAM);

  this->config = Config::createForFunction(F);
  this->F = &F;
  TargetTransformInfo &PassTTI =
      getAnalysis<TargetTransformInfoWrapperPass>().getTTI(F); // FIXME use FAM
  TargetLibraryInfo &PassTLI =
      getAnalysis<TargetLibraryInfoWrapperPass>().getTLI(F); // FIXME use FAM

  // setup PlatformInfo
  PlatformInfo platInfo(*F.getParent(), &PassTTI, &PassTLI);

  // TODO translate fast-math flag to ULP error bound
  if (!CheckFlag("RV_NO_SLEEF")) {
    addSleefResolver(config, platInfo);
  }

  // enable inter-procedural vectorization
  if (config.enableGreedyIPV) {
    Report() << "Using greedy inter-procedural vectorization.\n";
    addRecursiveResolver(config, platInfo);
  }

  vectorizer.reset(new VectorizerInterface(platInfo, config));

  if (enableDiagOutput) {
    platInfo.print(ReportContinue());
  }

  // Step 1: cost, legal, collect loopb jobs
  auto & LI = FAM.getResult<LoopAnalysis>(F);
  bool FoundAnyLoops = collectLoopJobs(LI);
  if (!FoundAnyLoops)
    return false;

  // Step 2: Refactor loop for vectorization
  bool PrepOK =
      prepareLoopVectorization(); // TODO mark this function as un-vectorizable
                                  // (or prepare a holdout copy)
  if (!PrepOK) {
    abort(); // loop nest preparation failed badly
  }

  // Step :3 Vectorize the prepare loops
  Changed |= vectorizeLoopRegions();

  IF_DEBUG_LV if (CheckFlag("RV_PRINT_FUNCTION")) {
    errs() << " -- module after RV --\n";
    Dump(*F.getParent());
  }

  // cleanup
  vectorizer.reset();
  this->F = nullptr;
  return Changed;
}

void LoopVectorizer::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequired<TargetTransformInfoWrapperPass>();
  AU.addRequired<TargetLibraryInfoWrapperPass>();
}

char LoopVectorizer::ID = 0;

FunctionPass *rv::createLoopVectorizerPass() { return new LoopVectorizer(); }

INITIALIZE_PASS_BEGIN(LoopVectorizer, "rv-loop-vectorize",
                      "RV - Vectorize loops", false, false)
INITIALIZE_PASS_DEPENDENCY(DominatorTreeWrapperPass)
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(MemoryDependenceWrapperPass)
INITIALIZE_PASS_DEPENDENCY(PostDominatorTreeWrapperPass)
INITIALIZE_PASS_DEPENDENCY(BranchProbabilityInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(ScalarEvolutionWrapperPass)
// PlatformInfo
INITIALIZE_PASS_DEPENDENCY(TargetTransformInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(TargetLibraryInfoWrapperPass)
INITIALIZE_PASS_END(LoopVectorizer, "rv-loop-vectorize", "RV - Vectorize loops",
                    false, false)
