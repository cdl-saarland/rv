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

#include "rv/passes/LoopVectorizer.h"
#include "rv/legacy/LinkAllPasses.h"

#include "rv/analysis/costModel.h"
#include "rv/analysis/loopAnnotations.h"
#include "rv/analysis/reductionAnalysis.h"
#include "rv/region/LoopRegion.h"
#include "rv/region/Region.h"
#include "rv/resolver/resolvers.h"
#include "rv/rv.h"
#include "rv/transform/remTransform.h"
#include "rv/vectorMapping.h"

#include "rv/passes/PassManagerSession.h"
#include "rv/config.h"
#include "rv/rvDebug.h"
#ifdef RV_ENABLE_LOOPDIST
#include "rv/transform/loopDistTrans.h"
#endif
#include "rvConfig.h"

#include "llvm/IR/DiagnosticInfo.h"
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
#include "llvm/Analysis/OptimizationRemarkEmitter.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/ScalarEvolutionExpressions.h"

#include "llvm/Support/CommandLine.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include <sstream>

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

cl::OptionCategory
    rvLoopVecCategory("RV LoopVectorizer Options",
                      "Configure the Outer-Loop Vectorizer of RV");

static bool IsSupportedReduction(Loop &L, Reduction &red) {
  // check that all users of the reduction are either (a) part of it or (b)
  // outside the loop
  for (auto *inst : red.elements) {
    for (auto itUser : inst->users()) {
      auto *userInst = dyn_cast<Instruction>(itUser);
      if (!userInst)
        return false; // unsupported
#if 0
      // FIXME: This does not work with min/max reductions (the CmpInst is not an element of the reduction).
      if (L.contains(userInst->getParent()) && !red.elements.count(userInst)) {
        Report() << "Unsupported user of reduction: " << *userInst << "\n";
        return false;
      }
#endif
    }
  }

  return true;
}

static void getRemarkLoc(Loop &L, Instruction *I, Value *&CodeRegion,
                         DebugLoc &DL) {
  CodeRegion = L.getHeader();
  DL = L.getStartLoc();
  if (I) {
    CodeRegion = I->getParent();
    // If there is no debug location attached to the instruction, revert back to
    // using the loop's.
    if (I->getDebugLoc())
      DL = I->getDebugLoc();
  }
}

#if 0
static OptimizationRemark createLoopRemark(StringRef RemarkName, Loop *TheLoop,
                                           Instruction *I) {
  Value *CodeRegion = TheLoop->getHeader();
  DebugLoc DL = TheLoop->getStartLoc();

  if (I) {
    CodeRegion = I->getParent();
    // If there is no debug location attached to the instruction, revert back to
    // using the loop's.
    if (I->getDebugLoc())
      DL = I->getDebugLoc();
  }

  return OptimizationRemark("rv-loopvec", RemarkName, DL, CodeRegion);
}
#endif

void LoopVectorizer::remark(const StringRef OREMsg, const StringRef ORETag,
                            Loop &TheLoop, llvm::Instruction *I) const {
  Value *CodeRegion;
  DebugLoc DL;
  getRemarkLoc(TheLoop, I, CodeRegion, DL);

  auto Remark = OptimizationRemark("rv-loopvec", ORETag, DL, CodeRegion);
  PassORE.emit(Remark << OREMsg);
}

void LoopVectorizer::remarkMiss(const StringRef OREMsg, const StringRef ORETag,
                                Loop &TheLoop, Instruction *I) const {
  Value *CodeRegion;
  DebugLoc DL;
  getRemarkLoc(TheLoop, I, CodeRegion, DL);

  auto Remark = OptimizationRemark("rv-loopvec", ORETag, DL, CodeRegion);
  PassORE.emit(Remark << OREMsg);
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
  auto &SE = PMS.FAM.getResult<ScalarEvolutionAnalysis>(F);
  auto *BTC = dyn_cast<SCEVConstant>(SE.getBackedgeTakenCount(&L));
  if (!BTC)
    return -1;

  int64_t BTCVal = BTC->getValue()->getSExtValue();
  if (BTCVal <= 1 || ((int64_t)((int)BTCVal)) != BTCVal)
    return -1;

  return BTCVal + 1;
}

bool LoopVectorizer::hasVectorizableLoopStructure(Loop &L, bool EmitRemarks) {
  ReductionAnalysis MyReda(F, PMS.FAM);
  MyReda.analyze(L);

  // Verify vectorizable control flow
  RemainderTransform remTrans(F, PMS.FAM, MyReda);
  if (!remTrans.analyzeLoopStructure(L))
    return false;

  // Next, check that we can vectorize all value recurrences (header phi nodes)
  // in this lop
  for (auto &Phi : L.getHeader()->phis()) {
    // Trivial constant strided shape
    rv::StridePattern *pat = MyReda.getStrideInfo(Phi);
    if (pat)
      continue;

    rv::Reduction *redInfo = MyReda.getReductionInfo(Phi);
    IF_DEBUG { errs() << "loopVecPass: header phi  " << Phi << " : "; }

    // failure to derive a reduction descriptor
    if (!redInfo) {
      if (EmitRemarks)
        remarkMiss("Unknown recurrence", "RVLoopVecNot", L, &Phi);
      Report() << "\n\tskip: unrecognized phi use in vector loop "
               << L.getName() << "\n";
      return false;
    }

    if (!IsSupportedReduction(L, *redInfo)) {
      if (EmitRemarks)
        remarkMiss("Invalid use of recurrence", "RVLoopVecNot", L, &Phi);
      Report() << " unsupported reduction: ";
      redInfo->print(ReportContinue());
      ReportContinue() << "\n";
      return false;
    }

    // unsupported reduction kind (operator in value SCC unrecognized)
    if (redInfo->kind == RedKind::Top) {
      if (EmitRemarks)
        remarkMiss("Unknown recurrence pattern", "RVLoopVecNot", L, &Phi);
      Report() << " can not vectorize this non-trivial SCC: ";
      redInfo->print(ReportContinue());
      ReportContinue() << "\n";
      return false;
    }

    // Unsupported recurrence (definition and use in different loop
    // iterations)
    if (redInfo->kind == RedKind::Bot) {
      if (EmitRemarks)
        remarkMiss("Unsupported loop-carried variable", "RVLoopVecNot", L,
                   &Phi);
      Report() << " can not vectorize this non-affine recurrence: ";
      redInfo->print(ReportContinue());
      ReportContinue() << "\n";
      return false;
    }

    // Otw, this is a privatizable reduction pattern
    IF_DEBUG { redInfo->dump(); }
  }

  return true;
}

static unsigned getOnlyLine() {
  char *OnlyLine = getenv("RV_ONLY_LINE");
  if (!OnlyLine) return 0;
  return atoi(OnlyLine);
}

static std::string getTag(DebugLoc DL) {
  if (!DL)
    return "";
  std::stringstream ss;
  ss << DL->getFilename().str() << ":" << DL->getLine();
  return ss.str();
}

bool LoopVectorizer::scoreLoop(LoopJob &LJ, LoopScore &LS, Loop &L) {
  Value *CodeRegion;
  DebugLoc DL;
  getRemarkLoc(L, &*L.getHeader()->phis().begin(), CodeRegion, DL);

  Report() << "loopVecPass::scopeLoop ";
  Report() << " at " << getTag(DL) << "\n";

  if (unsigned OnlyLine = getOnlyLine()) {
    if (DL.getLine() != OnlyLine)
      return false;
    remark("Hit the RV_ONLY_LINE loop", "RVOnlyLine", L);
  }

  LoopMD mdAnnot = GetLoopAnnotation(L);

  if (mdAnnot.alreadyVectorized.safeGet(false)) {
    Report() << "x already vectorized\n";
    return false;
  }

  // Preserve user intent.
  if (mdAnnot.vectorizeEnable.safeGet(false))
    LS.HasSIMDAnnotation = true;

  // Report reasons if this loop has a vectorization hint.
  bool DoReportFail = mdAnnot.vectorizeEnable.safeGet(false);

  // Check we are technically capable of turning this loop into a lock-step loop
  // (cheap-ish)
  if (!hasVectorizableLoopStructure(L, DoReportFail)) {
    Report() << "x unfit loop structure\n";
    return false;
  }

  // Trivial case.
  if (L.isAnnotatedParallel()) {
    mdAnnot.minDepDist = ParallelDistance;
    mdAnnot.vectorizeEnable = true;

    // Check whether this loop is actually parallel (expensive)
  }

  if (enableDiagOutput) {
    Report() << "loopVecPass: ";
    mdAnnot.print(ReportContinue()) << "\n";
  }

  // only trigger on annotated loops
  if (!mdAnnot.vectorizeEnable.safeGet(false)) {
    if (enableDiagOutput)
      Report() << "x loopVecPass skip " << L.getName()
               << " . not explicitly triggered.\n";
    return false;
  }

  // skip if already vectorized
  if (mdAnnot.alreadyVectorized.safeGet(false)) {
    // too verbose
    if (enableDiagOutput)
      Report() << "x loopVecPass skip " << L.getName()
               << " . already vectorized.\n";
    return false;
  }

  LJ.DepDist = mdAnnot.minDepDist.safeGet(ParallelDistance);

  // skip if iteration dependence distance precludes vectorization
  if (LJ.DepDist <= 1) {
    if (enableDiagOutput)
      Report() << "x loopVecPass skip " << L.getName()
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
    if (LJ.VectorWidth > (size_t)KnownTripCount) {
      LJ.VectorWidth = KnownTripCount;
      if (enableDiagOutput) {
        Report() << "loopVecPass, costModel: narrowing to known trip count: "
                 << KnownTripCount << "\n";
      }
    }
  }

  // pick a vectorization factor (unless user override is set)
  if (!hasFixedWidth) {
    size_t initialWidth = LJ.VectorWidth == 0 ? LJ.DepDist : LJ.VectorWidth;

    // Re-fine using cost model
    CostModel costModel(vectorizer->getPlatformInfo(), RVConfig);
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

  const char *SelLoopTxt = getenv("RV_SELECT_LOOP");
  if (SelLoopTxt) {
    int SelLoop = atoi(SelLoopTxt);
    GlobalLoopCount++;

    if (SelLoop != GlobalLoopCount) {
      Report() << "loopVecPass, RV_SELECT_LOOP != " << GlobalLoopCount
               << ". not vectorizing!\n";
      return false;
    }
  }

  const char *SelName = getenv("RV_SELECT_NAME");
  if (SelName) {
    std::string NameLoopTxt = SelName;

    bool SelectByName = L.getHeader()->getName().startswith(NameLoopTxt);

    if (!SelectByName) {
      Report() << "loopVecPass, RV_SELECT_NAME != " << NameLoopTxt
               << ". not vectorizing!\n";
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

static void for_loops(Loop &L,
                      std::function<ForLoopsControl(Loop &)> BodyFunc) {
  if (BodyFunc(L) == SkipChildren)
    return;
  for (auto *ChildL : L) {
    for_loops(*ChildL, BodyFunc);
  }
}

static void for_loops(LoopInfo &LI,
                      std::function<ForLoopsControl(Loop &)> BodyFunc) {
  std::vector<Loop *> LoopVec;
  for (auto *L : LI) {
    for_loops(*L, BodyFunc);
  }
}
#if 0
static bool for_loops_post(Loop &L, std::function<bool(Loop &)> BodyFunc) {
  bool Stop = false;
  for (auto *ChildL : L) {
    Stop |= for_loops_post(*ChildL, BodyFunc);
  }
  if (Stop)
    return true;
  return BodyFunc(L);
}

static void for_loops_post(LoopInfo &LI, std::function<bool(Loop &)> BodyFunc) {
  std::vector<Loop *> LoopVec;
  for (auto *L : LI) {
    for_loops_post(*L, BodyFunc);
  }
}
#endif

bool LoopVectorizer::collectLoopJobs(LoopInfo &LI) {
#if 0
  // Outer-loop preference (ignores nested 'pragma omp simd' loops).
  // TODO consider loops inside legal loops for vectorization on cost grounds.
  for_loops(LI, [&](Loop &L) {
    LoopJob LJ;
    LoopScore LS;

    // cost & legality
    bool Legal = scoreLoop(LJ, LS, L);
    if (!Legal)
      return Descend;

    // TODO: Check whether there is any child loop with an explicit SIMD
    // annotation.

    LoopsToPrepare.emplace_back(LJ);
    return SkipChildren;
  });

#else
  // OpenMP mode (vectorize 'parallel for' loops unless there is a nested
  // 'pragma omp simd' loop). Outer-loop preference (ignores nested 'pragma omp
  // simd' loops).
  // TODO consider loops inside legal loops for vectorization on cost grounds.
  for_loops(LI, [&](Loop &L) {
    LoopJob LJ;
    LoopScore LS;

    // cost & legality
    bool Legal = scoreLoop(LJ, LS, L);
    if (!Legal)
      return Descend;

    // Check whether there is a legal inner loop with a vectorization pragma.
    bool FoundSIMDLoop = false;
    for_loops(L, [&](Loop &InnerL) {
      // Skip parent scope.
      if (&InnerL == &L)
        return Descend;

      LoopJob InnerLJ;
      LoopScore InnerLS;

      // cost & legality
      bool InnerLegal = scoreLoop(InnerLJ, InnerLS, InnerL);
      if (!InnerLegal)
        return Descend;

      if (InnerLS.HasSIMDAnnotation) {
        FoundSIMDLoop = true;
        LoopsToPrepare.emplace_back(InnerLJ);
        return SkipChildren;
      }
      return Descend;
    });

    if (!FoundSIMDLoop)
      LoopsToPrepare.emplace_back(LJ);

    return SkipChildren;
  });
#endif

  return !LoopsToPrepare.empty();
}

PreparedLoop LoopVectorizer::transformToVectorizableLoop(
    Loop &L, int VectorWidth, int tripAlign, ValueSet &uniformOverrides) {
  IF_DEBUG {
    errs() << "\tPreparing loop structure of " << L.getName() << "\n";
  }

  // try to applu the remainder transformation
  ReductionAnalysis MyReda(F, PMS.FAM);
  MyReda.analyze(L);
  RemainderTransform remTrans(F, PMS.FAM, MyReda);
  PreparedLoop LoopPrep = remTrans.createVectorizableLoop(
      L, uniformOverrides, false, VectorWidth, tripAlign);

  return LoopPrep;
}

bool LoopVectorizer::prepareLoopVectorization() {
  auto &LI = *PMS.FAM.getCachedResult<LoopAnalysis>(F);
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
      ReductionAnalysis MyReda(*F, PMS.FAM);
      MyReda.analyze(*LoopPrep.TheLoop);

      LoopComponentAnalysis LCA(*F, *LoopPrep.TheLoop, PMS.FAM, MyReda);
      LCA.run();
      LoopDistributionTransform loopDistTrans(vectorizer->getPlatformInfo(),
                                              LJ.VectorWidth, LCA);
      loopDistTrans.run();
    }
    /// END EXPERIMENTAL SECTION
#endif

    // Make sure that there is a preheader in any case
    BasicBlock *UniquePred = nullptr;
    if (!LoopPrep.TheLoop->getLoopPreheader()) {
      auto *Head = LoopPrep.TheLoop->getHeader();
      for (auto *InB : predecessors(Head)) {
        if (LoopPrep.TheLoop->contains(InB))
          continue;

        if (!UniquePred) {
          UniquePred = InB;
        } else {
          abort(); // Multiple edges to the loop header!!!
        }
      }

      // break the edge
      std::string PHName = Head->getName().str() + ".ph";
      auto *PH = BasicBlock::Create(F.getContext(), PHName, &F, Head);
      UniquePred->getTerminator()->replaceUsesOfWith(Head, PH);
      BranchInst::Create(Head, PH);
      for (auto &phi : Head->phis()) {
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
      Report() << " rv::RVConfig: ";
      RVConfig.print(ReportContinue());
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

bool LoopVectorizer::vectorizeLoop(LoopVectorizerJob &LVJob) {
  // auto &LI = *PMS.FAM.getCachedResult<LoopAnalysis>(*F);
  auto &LI = PMS.FAM.getResult<LoopAnalysis>(F);
  auto &L = *LI.getLoopFor(LVJob.LJ.Header);

  // analyze the recurrence patterns of this loop
  ReductionAnalysis MyReda(F, PMS.FAM);
  MyReda.analyze(L);

  // start vectorizing the prepared loop
  IF_DEBUG { errs() << "rv: Vectorizing loop " << L.getName() << "\n"; }

  VectorMapping targetMapping(&F, &F, LVJob.LJ.VectorWidth,
                              CallPredicateMode::SafeWithoutPredicate);
  LoopRegion LoopRegionImpl(L);
  Region LoopRegion(LoopRegionImpl);

  VectorizationInfo vecInfo(F, LVJob.LJ.VectorWidth, LoopRegion);
  std::stringstream Str;
  Str << "Loop vectorized (width " << LVJob.LJ.VectorWidth << ")";
  assert(!LVJob.EntryAVL && "AVL support broken!");
 #if  0
  if (LVJob.EntryAVL) {
    vecInfo.setEntryAVL(LVJob.EntryAVL);
    Str << " with dynamic VL";
  } else {
#endif
  Str << " with scalar remainder loop";
  remark(Str.str(), "RVLoopVectorized", L);

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

      assert(redInfo);
      assert(IsSupportedReduction(L, *redInfo));
      // unsupported reduction kind (operator in value SCC unrecognized)
      assert(redInfo->kind != RedKind::Top);

      // Unsupported recurrence (definition and use in different loop
      // iterations)
      assert(redInfo->kind != RedKind::Bot);

      // Otw, this is a privatizable reduction pattern
      IF_DEBUG { redInfo->dump(); }
      phiShape = redInfo->getShape(LVJob.LJ.VectorWidth);
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
  vectorizer->lowerRuntimeCalls(vecInfo, PMS.FAM);

  // Vectorize
  // vectorizationAnalysis
  vectorizer->analyze(vecInfo, PMS.FAM);

  if (enableDiagOutput) {
    errs() << "-- VA result --\n";
    vecInfo.dump();
    errs() << "-- EOF --\n";
  }

  IF_DEBUG Dump(F);

  assert(L.getLoopPreheader());

  // control conversion
  vectorizer->linearize(vecInfo, PMS.FAM);

  // vectorize the prepared loop embedding it in its context
  ValueToValueMapTy vecMap;

  ScalarEvolutionAnalysis adhocAnalysis;
  adhocAnalysis.run(F, PMS.FAM);

  bool vectorizeOk = vectorizer->vectorize(vecInfo, PMS.FAM, &vecMap);
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
    auto PA = PreservedAnalyses::all();
    // FIXME repair loop info on the go
    // Rebuild analysis structured
    PA.abandon<DominatorTreeAnalysis>();
    PA.abandon<PostDominatorTreeAnalysis>();
    PA.abandon<LoopAnalysis>();
    PMS.FAM.invalidate(F, PA);

    Changed |= vectorizeLoop(LVJob);
  }
  LoopsToVectorize.clear();

  return Changed;
}

LoopVectorizer::LoopVectorizer(Function &F, TargetTransformInfo &PassTTI,
                               TargetLibraryInfo &PassTLI,
                               OptimizationRemarkEmitter &PassORE)
    : RVConfig(Config::createForFunction(F)), F(F), PassTTI(PassTTI),
      PassTLI(PassTLI), PassORE(PassORE) {
  // have we introduced ourself? (reporting output)
  enableDiagOutput = CheckFlag("LV_DIAG");
  introduced = false;
}

bool LoopVectorizer::run() {
  if (getenv("RV_DISABLE"))
    return false;

  if (enableDiagOutput)
    Report() << "loopVecPass: run on " << F.getName() << "\n";

  if (CheckFlag("RV_PRINT_FUNCTION")) {
    Report() << "-- RV::LoopVectorizer --\n";
    F.print(Report());
  }

  IF_DEBUG {
    errs() << " -- module before RV --\n";
    Dump(*F.getParent());
  }

  // setup PlatformInfo
  PlatformInfo platInfo(*F.getParent(), &PassTTI, &PassTLI);
  vectorizer.reset(new VectorizerInterface(platInfo, RVConfig));

  // TODO translate fast-math flag to ULP error bound
  if (!CheckFlag("RV_NO_SLEEF")) {
    addSleefResolver(RVConfig, platInfo);
  }

  // enable inter-procedural vectorization
  if (RVConfig.enableGreedyIPV) {
    Report() << "Using greedy inter-procedural vectorization.\n";
    addRecursiveResolver(RVConfig, platInfo);
  }

  if (enableDiagOutput) {
    platInfo.print(ReportContinue());
  }

  bool Changed = false;

  // Step 1: cost, legal, collect loopb jobs
  auto &LI = PMS.FAM.getResult<LoopAnalysis>(F);
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

  if (CheckFlag("RV_PRINT_FUNCTION")) {
    errs() << " -- module after RV --\n";
    Dump(*F.getParent());
  }

  // cleanup
  vectorizer.reset();
  return Changed;
}

///// OldPM Wrapper Pass /////

void LoopVectorizerLegacyPass::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequired<TargetTransformInfoWrapperPass>();
  AU.addRequired<TargetLibraryInfoWrapperPass>();
  AU.addRequired<OptimizationRemarkEmitterWrapperPass>();
}

bool LoopVectorizerLegacyPass::runOnFunction(Function &F) {
  auto &TTI = getAnalysis<TargetTransformInfoWrapperPass>().getTTI(F);
  auto &TLI = getAnalysis<TargetLibraryInfoWrapperPass>().getTLI(F);
  auto &ORE = getAnalysis<OptimizationRemarkEmitterWrapperPass>().getORE();

  LoopVectorizer LoopVec(F, TTI, TLI, ORE);
  return LoopVec.run();
}

char LoopVectorizerLegacyPass::ID = 0;

FunctionPass *rv::createLoopVectorizerLegacyPass() { return new LoopVectorizerLegacyPass(); }

INITIALIZE_PASS_BEGIN(LoopVectorizerLegacyPass, "rv-loop-vectorize",
                      "RV - Vectorize loops", false, false)
INITIALIZE_PASS_DEPENDENCY(DominatorTreeWrapperPass)
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(MemoryDependenceWrapperPass)
INITIALIZE_PASS_DEPENDENCY(PostDominatorTreeWrapperPass)
INITIALIZE_PASS_DEPENDENCY(BranchProbabilityInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(ScalarEvolutionWrapperPass)
INITIALIZE_PASS_DEPENDENCY(OptimizationRemarkEmitterWrapperPass)
// PlatformInfo
INITIALIZE_PASS_DEPENDENCY(TargetTransformInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(TargetLibraryInfoWrapperPass)
INITIALIZE_PASS_END(LoopVectorizerLegacyPass, "rv-loop-vectorize", "RV - Vectorize loops",
                    false, false)


///// NewPM Wrapper Pass /////

llvm::PreservedAnalyses
LoopVectorizerWrapperPass::run(llvm::Function &F,
                               llvm::FunctionAnalysisManager &FAM) {
  auto &TTI = FAM.getResult<TargetIRAnalysis>(F);
  auto &TLI = FAM.getResult<TargetLibraryAnalysis>(F);
  auto &ORE = FAM.getResult<OptimizationRemarkEmitterAnalysis>(F);

  LoopVectorizer LoopVec(F, TTI, TLI, ORE);
  if (LoopVec.run())
    return llvm::PreservedAnalyses::none();
  return llvm::PreservedAnalyses::all();
}
