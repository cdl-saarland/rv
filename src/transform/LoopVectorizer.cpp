//===- LoopVectorizer.cpp - Vectorize Loops  ----------------===//
//
//                     The LLVM Compiler Infrastructure
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

#include "rv/rv.h"
#include "rv/vectorMapping.h"
#include "rv/analysis/maskAnalysis.h"
#include "rv/region/LoopRegion.h"
#include "rv/region/Region.h"
#include "rv/sleefLibrary.h"

#include "rvConfig.h"

#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/ScalarEvolutionExpressions.h"
#include "llvm/IR/Dominators.h"

#include "report.h"

using namespace rv;
using namespace llvm;

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

bool LoopVectorizer::canAdjustTripCount(Loop &L, ScalarEvolution &SE,
                                        int VectorWidth, int TripCount) {
  if (VectorWidth == TripCount)
    return true;

  return false;
}

int LoopVectorizer::getTripCount(Loop &L, ScalarEvolution &SE) {
  auto *BTC = dyn_cast<SCEVConstant>(SE.getBackedgeTakenCount(&L));
  if (!BTC)
    return -1;

  int64_t BTCVal = BTC->getValue()->getSExtValue();
  if (BTCVal <= 1 || ((int64_t)((int) BTCVal)) != BTCVal)
    return -1;

  return BTCVal + 1;
}

int LoopVectorizer::getVectorWidth(Loop &L, ScalarEvolution &SE) {
  auto *LID = L.getLoopID();
  if (!LID)
    return -1;

  for (int i = 0, e = LID->getNumOperands(); i < e; i++) {
    const MDOperand &Op = LID->getOperand(i);
    auto *OpMD = dyn_cast<MDNode>(Op);
    if (!OpMD || OpMD->getNumOperands() != 2)
      continue;

    auto *Str = dyn_cast<MDString>(OpMD->getOperand(0));
    auto *Cst = dyn_cast<ConstantAsMetadata>(OpMD->getOperand(1));
    if (!Str || !Cst)
      continue;

    if (Str->getString().equals("llvm.loop.vectorize.enable")) {
      if (Cst->getValue()->isNullValue())
        return -1;
    }

    if (Str->getString().equals("llvm.loop.vectorize.width")) {
      if (auto *CstInt = dyn_cast<ConstantInt>(Cst->getValue()))
        return CstInt->getSExtValue();
    }
  }

  return -1;
}

bool LoopVectorizer::vectorizeLoop(Loop &L, ScalarEvolution &SE, VectorizerInterface & vectorizer) {
  if (!canVectorizeLoop(L))
    return false;

  int TripCount = getTripCount(L, SE);
  if (TripCount < 0) {
    Report() << "loopVecPass skip: won't vectorize " << L.getName() << " . Infered trip count was " << TripCount << "\n";
    return false;
  }

  int VectorWidth = getVectorWidth(L, SE);
  if (VectorWidth < 0) {
    Report() << "loopVecPass skip: won't vectorize " << L.getName() << " . Vector width was " << VectorWidth << "\n";
    return false;
  }

  if (!canAdjustTripCount(L, SE, VectorWidth, TripCount)) {
    Report() << "loopVecPass skip: won't vectorize " << L.getName() << " . Could not adjust trip count\n";
    return false;
  }

  Report() << "loopVecPass: Vectorize " << L.getName() << " with VW: " << VectorWidth
         << " and TC: " << TripCount << "\n";

  BasicBlock *ExitingBlock = L.getExitingBlock();
  Function &F = *ExitingBlock->getParent();
  Module &M = *F.getParent();

  // Adjust trip count
  // TODO only if VectorWidth == TripCount
  assert(VectorWidth == TripCount);
  auto *ExitingBI = cast<BranchInst>(ExitingBlock->getTerminator());
  if (L.contains(ExitingBI->getSuccessor(0)))
    ExitingBI->setCondition(ConstantInt::getFalse(ExitingBI->getContext()));
  else
    ExitingBI->setCondition(ConstantInt::getTrue(ExitingBI->getContext()));

  VectorMapping targetMapping(&F, &F, VectorWidth);

  auto &DT = getAnalysis<DominatorTreeWrapperPass>().getDomTree();
  auto &PDT = getAnalysis<PostDominatorTreeWrapperPass>().getPostDomTree();
  auto &LI = getAnalysis<LoopInfoWrapperPass>().getLoopInfo();

  // rebuild dominator info
  DT.recalculate(F);
  PDT.recalculate(F);

  //DT.verifyDomTree();
  //LI.verify(DT);

  // Domin Frontier Graph
  DFG dfg(DT);
  dfg.create(F);

  // Control Dependence Graph
  CDG cdg(PDT);
  cdg.create(F);

  LoopRegion LoopRegionImpl(L);
  Region LoopRegion(LoopRegionImpl);

  VectorizationInfo vecInfo(F, VectorWidth, LoopRegion);

  // configure initial shape for induction variable
  auto *header = L.getHeader();
  PHINode *xPhi = cast<PHINode>(&*header->begin());
  IF_DEBUG { errs() << "Vectorizing loop with induction variable " << *xPhi << "\n"; }
  vecInfo.setVectorShape(*xPhi, VectorShape::cont(VectorWidth));

  // configure exit condition to be non-divergent in any case
  //vecInfo.setVectorShape(*ExitingTI, VectorShape::uni());
  //vecInfo.setVectorShape(
      //*cast<BranchInst>(ExitingTI)->getOperand(0),
      //VectorShape::uni());


  // vectorizationAnalysis
  vectorizer.analyze(vecInfo, cdg, dfg, LI, PDT, DT);

  // mask analysis
  auto *maskAnalysis = vectorizer.analyzeMasks(vecInfo, LI);
  assert(maskAnalysis);
  IF_DEBUG { maskAnalysis->print(errs(), &M); }

  // mask generator
  bool genMaskOk = vectorizer.generateMasks(vecInfo, *maskAnalysis, LI);
  if (!genMaskOk)
    llvm_unreachable("mask generation failed.");

  // control conversion
  bool linearizeOk =
      vectorizer.linearizeCFG(vecInfo, *maskAnalysis, LI, DT);
  if (!linearizeOk)
    llvm_unreachable("linearization failed.");

  const DominatorTree domTreeNew(
      *vecInfo.getMapping().scalarFn); // Control conversion does not preserve
                                       // the domTree so we have to rebuild it
                                       // for now
  bool vectorizeOk = vectorizer.vectorize(vecInfo, domTreeNew, LI);
  if (!vectorizeOk)
    llvm_unreachable("vector code generation failed");

  delete maskAnalysis;
  return true;
}

bool LoopVectorizer::vectorizeLoopOrSubLoops(Loop &L, ScalarEvolution &SE, VectorizerInterface & vectorizer) {
  if (vectorizeLoop(L, SE, vectorizer))
    return true;

  bool Changed = false;
  for (Loop* SubL : L)
    Changed |= vectorizeLoopOrSubLoops(*SubL, SE, vectorizer);

  return Changed;
}

bool LoopVectorizer::runOnFunction(Function &F) {
  Report() << "loopVecPass: run on " << F.getName() << "\n";
  bool Changed = false;
  auto &LI = getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
  auto &SE = getAnalysis<ScalarEvolutionWrapperPass>().getSE();


  FunctionAnalysisManager fam;
  ModuleAnalysisManager mam;

  TargetIRAnalysis irAnalysis;
  TargetTransformInfo tti = irAnalysis.run(F, fam);
  TargetLibraryAnalysis libAnalysis;
  TargetLibraryInfo tli = libAnalysis.run(*F.getParent(), mam);
  PlatformInfo platformInfo(*F.getParent(), &tti, &tli);

  bool useSSE = false, useAVX = true, useAVX2 = true, useImpreciseFunctions = true;

  addSleefMappings(useSSE, useAVX, useAVX2, platformInfo, useImpreciseFunctions);
  VectorizerInterface vectorizer(platformInfo);

  for (Loop *L : LI)
    Changed |= vectorizeLoopOrSubLoops(*L, SE, vectorizer);

  // cleanup
  // if (Changed) {
    vectorizer.finalize();
  // }

  return Changed;
}

#if 0
bool LoopVectorizer::vectorizeRVLoop(RVInfo &RVI) {

  return true;
}
#endif

void LoopVectorizer::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequired<LoopInfoWrapperPass>();
  AU.addRequired<DominatorTreeWrapperPass>();
  AU.addRequired<ScalarEvolutionWrapperPass>();
  AU.addRequired<PostDominatorTreeWrapperPass>();
}

char LoopVectorizer::ID = 0;

Pass *rv::createLoopVectorizerPass() { return new LoopVectorizer(); }

INITIALIZE_PASS_BEGIN(LoopVectorizer, "rv-loop-vectorize",
                      "RV - Vectorize loops", false, false)
INITIALIZE_PASS_DEPENDENCY(LoopInfoWrapperPass)
INITIALIZE_PASS_DEPENDENCY(DominatorTreeWrapperPass)
INITIALIZE_PASS_DEPENDENCY(ScalarEvolutionWrapperPass)
INITIALIZE_PASS_DEPENDENCY(PostDominatorTreeWrapperPass)
INITIALIZE_PASS_END(LoopVectorizer, "rv-loop-vectorize", "RV - Vectorize loops",
                    false, false)
