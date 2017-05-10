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

#include "rv/rv.h"
#include "rv/vectorMapping.h"
#include "rv/analysis/maskAnalysis.h"
#include "rv/region/LoopRegion.h"
#include "rv/region/Region.h"
#include "rv/sleefLibrary.h"
#include "rv/analysis/reductionAnalysis.h"

#include "rvConfig.h"

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Dominators.h"

#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/Analysis/ScalarEvolutionExpressions.h"
#include "llvm/Analysis/MemoryDependenceAnalysis.h"


#include "report.h"

#ifdef IF_DEBUG
#undef IF_DEBUG
#endif

#define IF_DEBUG if (true)

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

int
LoopVectorizer::getDependenceDistance(Loop & L) {
  int vectorWidth = getVectorWidth(L);
  if (vectorWidth > 0) return vectorWidth;

  if (!canVectorizeLoop(L)) return 1;
  return ParallelDistance; // fully parallel
}

int
LoopVectorizer::getTripAlignment(Loop & L) {
  int tripCount = getTripCount(L);
  if (tripCount > 0) return tripCount;
  return 1;
}


bool LoopVectorizer::canAdjustTripCount(Loop &L, int VectorWidth, int TripCount) {
  if (VectorWidth == TripCount)
    return true;

  return false;
}

int
LoopVectorizer::getTripCount(Loop &L) {
  auto *BTC = dyn_cast<SCEVConstant>(SE->getBackedgeTakenCount(&L));
  if (!BTC)
    return -1;

  int64_t BTCVal = BTC->getValue()->getSExtValue();
  if (BTCVal <= 1 || ((int64_t)((int) BTCVal)) != BTCVal)
    return -1;

  return BTCVal + 1;
}

int LoopVectorizer::getVectorWidth(Loop &L) {
  auto *LID = L.getLoopID();

  // try to recover from latch
  if (!LID) {
    auto * latch = L.getLoopLatch();
    LID = latch->getTerminator()->getMetadata("llvm.loop");
    if (LID) IF_DEBUG { errs() << "Recovered loop MD from latch!\n"; }
  }

  if (!LID) return -1;

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

typedef std::map<PHINode*, PHINode*> PHIMap;
typedef std::map<Instruction*, Instruction*> InstMap;

struct LoopRemainderTransform {
  Function & F;
  Loop & ScalarL;
  ValueToValueMapTy & vecValMap;
  VectorizationInfo & vecInfo;
  ReductionAnalysis & reda;
  int tripAlign;
  int vectorWidth;

  // - original loop -
  //
  // entry
  //   |
  // ScalarL <>
  //   |
  //  Exit
  //
  // - transformed loop -
  // entry --> ... // entry branch to the loop
  //   |
  // vecGuard // cost model block and VectorL preheader
  //  |   \
  //  |   VectorL <> // vector loop produced by RV
  //  |    |
  //  |   VecToScalar --> Exit  // reduces vector values to scalars
  //  |   /
  //  ScalarGuard //  scalar loop preHeader
  //    |
  //  ScalarL <> --> Exit

// scalar loop context
  // the old preheader
  BasicBlock * entryBlock;

  // the single loop exit
  BasicBlock * loopExit;

// embedding blocks
  // either dispatches to the vectorized or the scalar loop
  BasicBlock * vecGuardBlock;

  // entry point to the scalar loop (scalar loop prehader)W
  BasicBlock * scalarGuardBlock;

  // exit of the vector loop to the scalar guard
  BasicBlock * vecToScalarExit;

  static
  BasicBlock*
  GetUniqueExiting(llvm::Loop & L) {
    auto * exitBlock = L.getExitBlock();
    for (auto & use : exitBlock->uses()) {
      auto * exitingBr = dyn_cast<BranchInst>(use.getUser());
      if (!exitingBr) continue;
      if (!L.contains(exitingBr->getParent())) continue;

      return exitingBr->getParent();
    }
    return nullptr;
  }

  LoopRemainderTransform(Function & _F, Loop & _ScalarL, ValueToValueMapTy & _vecValMap, VectorizationInfo & _vecInfo, ReductionAnalysis & _reda, int _tripAlign, int _vectorWidth)
  : F(_F)
  , ScalarL(_ScalarL)
  , vecValMap(_vecValMap)
  , vecInfo(_vecInfo)
  , reda(_reda)
  , tripAlign(_tripAlign)
  , vectorWidth(_vectorWidth)
  , entryBlock(ScalarL.getLoopPreheader())
  , loopExit(ScalarL.getExitBlock())
  , vecGuardBlock(nullptr)
  , scalarGuardBlock(nullptr)
  , vecToScalarExit(nullptr)
  {
    assert(loopExit && "multi exit loops unsupported (yet)");
    assert(ScalarL.getExitingBlock() && "Scalar loop does not have a unique exiting block (unsupported)");

    // create all basic blocks
    setupControl();

    // repair value flow through the blocks
    repairValueFlow();
  }

  // set-up the vector loop CFG
  void
  setupControl() {
    auto * scalarHead = ScalarL.getHeader();
    auto & context = scalarHead->getContext();
    auto * vecHead = cast<BasicBlock>(vecValMap[scalarHead]);

    std::string loopName = ScalarL.getName().str();
    vecGuardBlock = BasicBlock::Create(context, loopName + ".vecg", &F, scalarHead);
    scalarGuardBlock = BasicBlock::Create(context, loopName + ".scag", &F, scalarHead);
    vecToScalarExit = BasicBlock::Create(context, loopName + ".vec2scalar", &F, scalarHead);

  // branch to vecGuard instead to the scalar loop
    auto * entryTerm = entryBlock->getTerminator();
    for (size_t i = 0; i < entryTerm->getNumSuccessors(); ++i) {
      if (scalarHead == entryTerm->getSuccessor(i)) {
        entryTerm->setSuccessor(i, vecGuardBlock);
        break;
      }
    }

  // dispatch to vector loop header or the scalar guard
    // TODO cost model / pre-conditions
    Value * constTrue = ConstantInt::getTrue(context);
    BranchInst::Create(vecHead, scalarGuardBlock, constTrue, vecGuardBlock);

  // make the vector loop exit to vecToScalar
    auto * scalarTerm = ScalarL.getExitingBlock()->getTerminator();
    auto * vecTerm = cast<TerminatorInst>(vecValMap[scalarTerm]);

    for (size_t i = 0; i < scalarTerm->getNumSuccessors(); ++i) {
      if (scalarTerm->getSuccessor(i) != loopExit) continue;
      vecTerm->setSuccessor(i, vecToScalarExit);
      break;
    }

  // branch from vecToScalarExit to the scalarGuard
    BranchInst::Create(scalarGuardBlock, vecToScalarExit);

  // make scalarGuard the new preheader of the scalar loop
    BranchInst::Create(scalarHead, scalarGuardBlock);
  }

  // reduce a vector loop liveout to a scalar value
  Value&
  ReduceValueToScalar(Value & scalarVal, BasicBlock & where) {
    auto valShape = vecInfo.getVectorShape(scalarVal);
    if (valShape.isUniform()) {
      return scalarVal;

    } else if (valShape.hasStridedShape()) {
      int64_t reducedStride = valShape.getStride() * vecInfo.getVectorWidth();
      IRBuilder<> builder(&where, where.getTerminator()->getIterator());
      return *builder.CreateAdd(vecValMap[&scalarVal], ConstantInt::get(scalarVal.getType(), reducedStride));

    } else {
      errs() << "general on-the-fly reduction not yet implemented!\n";
      abort();
    }
  }

  // reduce all vector values to scalar values
  // vecLoopHis will contain the reduced loop header phis (to be used as initial values in the scalar loop)
  // vecLiveOuts will contain all reduced liveouts of the scalar loop
  // header phis may be contained in both sets
  void
  reduceVectorLiveOuts(ValueToValueMapTy & vecLoopPhis, ValueToValueMapTy & vecLiveOuts) {
    auto * scalHeader = ScalarL.getHeader();

  // reduce all loop header phis
    for (auto & scalInst : *scalHeader) {
      if (!isa<PHINode>(scalInst)) break;
      auto & scalarPhi = cast<PHINode>(scalInst);

      auto & reducedVecPhi = ReduceValueToScalar(scalarPhi, *vecToScalarExit);

      vecLoopPhis[&scalarPhi] = &reducedVecPhi;
    }

  // reduce all remaining live outs
    for (auto * BB : ScalarL.blocks()) {
      for (auto & Inst : *BB) {
        for (auto & use : Inst.uses()) {
          auto * userInst = cast<Instruction>(use.getUser());
          if (ScalarL.contains(userInst)) continue;

          // we already reduced this loop header phi
          if (vecLoopPhis.count(&Inst)) {
            vecLiveOuts[&Inst] = vecLoopPhis[&Inst];
            continue;
          }

          // otw, reduce it now
          ReduceValueToScalar(Inst, *vecToScalarExit);
          assert(false && "");
        }
      }
    }
    // TODO not supported yet
  }

  void
  updateScalarLoopStartValues(ValueToValueMapTy & vecLoopPhis) {
    auto * scalHeader = ScalarL.getHeader();

    IRBuilder<> scaGuardBuilder(scalarGuardBlock, scalarGuardBlock->begin());

    for (auto & scalInst : *scalHeader) {
      if (!isa<PHINode>(scalInst)) break;
      auto & scalarPhi = cast<PHINode>(scalInst);

      std::string phiName = scalarPhi.getName().str();

      int preHeaderIdx = scalarPhi.getBasicBlockIndex(entryBlock);
      assert(preHeaderIdx >= 0);
      auto & initialValue = *scalarPhi.getIncomingValue(preHeaderIdx);

    // create a PHI for every loop header phi in the scalar loop
      // when coming from the vectorGuard -> use the old initial values
      // when coming from the vecToScalarExit -> use the reduced scalar values from the vector loop
      auto &scaGuardPhi = *scaGuardBuilder.CreatePHI(scalarPhi.getType(), 2, phiName + ".scaGuard");
      scaGuardPhi.addIncoming(vecLoopPhis[&scalarPhi], vecToScalarExit);
      scaGuardPhi.addIncoming(&initialValue, vecGuardBlock);

    // take scaGuardPhi from the new preHeader of the scalar loop (scalarGuardBlock)
      scalarPhi.setIncomingBlock(preHeaderIdx, scalarGuardBlock);
      scalarPhi.setIncomingValue(preHeaderIdx, &scaGuardPhi);
    }
  }

  void
  updateExitLiveOuts(ValueToValueMapTy & vecLiveOuts) {
  // reduce all remaining live outs
    IRBuilder<> exitBuilder(loopExit, loopExit->begin());

    for (auto * BB : ScalarL.blocks()) {
      for (auto & Inst : *BB) {
        PHINode * mergePhi = nullptr;
        for (auto & use : Inst.uses()) {
          auto * userInst = cast<Instruction>(use.getUser());
          if (ScalarL.contains(userInst)) continue;

          auto scaLiveOut = &Inst;
          auto vecLiveOut = vecLiveOuts[scaLiveOut];

          assert(vecLiveOut && "live out was not reduced in vector loop");

          if (isa<PHINode>(userInst) && userInst->getParent() == mergePhi->getParent()) {
            auto & userPhi = *cast<PHINode>(userInst);
            int exitingIdx = userPhi.getBasicBlockIndex(ScalarL.getExitingBlock());
            assert(exitingIdx >= 0);

            // vector loop is exiting to this block now as well
            userPhi.addIncoming(vecLiveOut, vecToScalarExit);

          } else {

            // Create a new phi node to receive this value
            if (!mergePhi) {
              std::string liveOutName = scaLiveOut->getName().str();
              mergePhi = exitBuilder.CreatePHI(scaLiveOut->getType(), 2, liveOutName + ".merge");
              mergePhi->addIncoming(scaLiveOut, ScalarL.getExitingBlock());
              mergePhi->addIncoming(vecLiveOut, vecToScalarExit);
              IF_DEBUG { errs() << "\tCreated merge phi " << *mergePhi << "\n"; }
            }

            userInst->setOperand(use.getOperandNo(), mergePhi);
            IF_DEBUG { errs() << "\t- fixed user " << *userInst << "\n"; }
          }
        }
      }
    }
  }

  void
  fixVecLoopHeaderPhis() {
    auto vecHead = cast<BasicBlock>(vecValMap[ScalarL.getHeader()]);
    for (auto & Inst : *vecHead) {
      auto * phi = dyn_cast<PHINode>(&Inst);
      if (!phi) break;
      int initOpIdx = phi->getBasicBlockIndex(entryBlock);
      phi->setIncomingBlock(initOpIdx, vecGuardBlock);
    }
  }

  void
  repairValueFlow() {
    ValueToValueMapTy vecLoopPhis, vecLiveOuts;

    // start edge now coming from vecGuardBlock (instead of old preheader entrBlock)
    fixVecLoopHeaderPhis();

    // reduce loop live outs and ALL vector loop phis (that existed in the scalar loop)
    reduceVectorLiveOuts(vecLoopPhis, vecLiveOuts);

    // let the scalar loop start from the remainder vector loop remainder (if the VL was executed)
    updateScalarLoopStartValues(vecLoopPhis);

    // repair vector loop liveouts
    updateExitLiveOuts(vecLiveOuts);
  }
};

static
rv::VectorShape
GetShapeFromReduction(rv::Reduction & redInfo, int vectorWidth) {
  auto & redInst = redInfo.getReductor();

  if (redInst.getOpcode() != Instruction::Add) {
    errs() << redInst << "\n";
    return VectorShape::varying();
  }

  auto *inConst = dyn_cast<ConstantInt>(&redInfo.getReducibleValue());

  if (!inConst) {
    return VectorShape::varying();
  }
  errs() << *inConst << "\n";

  auto constInc = inConst->getSExtValue();

  return VectorShape::strided(constInc, constInc * vectorWidth);
}


void
LoopVectorizer::embedVectorizedLoop(Loop &L, llvm::ValueToValueMapTy & vecValMap, VectorizationInfo & vecInfo, int VectorWidth, int tripAlign) {
  IF_DEBUG { errs() << "\tCreating scalar remainder Loop for " << L.getName() << "\n"; }

  // TODO update vector loop trip count

  // run remainder transform
  LoopRemainderTransform remTrans(*F, L, vecValMap, vecInfo, *reda, VectorWidth, tripAlign);

// modify CFG
  // Adjust trip count
}

bool
LoopVectorizer::vectorizeLoop(Loop &L) {
// check the dependence distance of this loop
  int depDist = getDependenceDistance(L);
  if (depDist <= 1) {
    Report() << "loopVecPass skip: won't vectorize " << L.getName() << " . Min dependence distance was " << depDist << "\n";
    return false;
  }

  //
  int tripAlign = getTripAlignment(L);

  int VectorWidth = getVectorWidth(L);
  if (VectorWidth < 0) {
    Report() << "loopVecPass skip: won't vectorize " << L.getName() << " . Vector width was " << VectorWidth << "\n";
    return false;
  }

  // if (tripAlign % VectorWidth != 0) {
  //   Report() << "loopVecPass skip: won't vectorize " << L.getName() << " . Could not adjust trip count\n";
  //   return false;
  // }

  Report() << "loopVecPass: Vectorize " << L.getName() << " with VW: " << VectorWidth
         << " and TripAlignment: " << tripAlign << "\n";

  BasicBlock *ExitingBlock = L.getExitingBlock();
  Function &F = *ExitingBlock->getParent();
  Module &M = *F.getParent();

  VectorMapping targetMapping(&F, &F, VectorWidth);

  LoopRegion LoopRegionImpl(L);
  Region LoopRegion(LoopRegionImpl);

  VectorizationInfo vecInfo(F, VectorWidth, LoopRegion);

  IF_DEBUG { errs() << "rv: Vectorizing loop " << L.getName() << "\n"; }


// Check reduction patterns of vector loop phis
  // configure initial shape for induction variable
  for (auto & inst : *L.getHeader()) {
    auto * phi = dyn_cast<PHINode>(&inst);
    if (!phi) continue;

    rv::Reduction * redInfo = reda->getReductionInfo(*phi);
    IF_DEBUG { errs() << "loopVecPass: header phi  " << *phi << " : "; }

    if (!redInfo) {
      errs() << "\n\tskip: non-reduction phi in vector loop header " << L.getName() << "\n";
      return false;
    }

    rv::VectorShape phiShape = GetShapeFromReduction(*redInfo, VectorWidth);

    IF_DEBUG{ redInfo->dump(); }
    IF_DEBUG { errs() << "header phi " << phi->getName() << " has shape " << phiShape.str() << "\n"; }

    vecInfo.setVectorShape(*phi, phiShape);
  }

// Force the loop exit branch to uniform (which it is going to be after the transformation)
  auto * exitBr = L.getExitingBlock()->getTerminator();
  vecInfo.setVectorShape(*exitBr, VectorShape::uni());

// prepare analyses
  auto &DT = getAnalysis<DominatorTreeWrapperPass>().getDomTree();
  auto &PDT = getAnalysis<PostDominatorTreeWrapperPass>().getPostDomTree();

  //DT.verifyDomTree();
  //LI.verify(DT);

  // Domin Frontier Graph
  DFG dfg(DT);
  dfg.create(F);

  // Control Dependence Graph
  CDG cdg(PDT);
  cdg.create(F);


// Vectorize
  // vectorizationAnalysis
  vectorizer->analyze(vecInfo, cdg, dfg, *LI, PDT, DT);

  // mask analysis
  auto maskAnalysis = vectorizer->analyzeMasks(vecInfo, *LI);
  assert(maskAnalysis);
  IF_DEBUG { maskAnalysis->print(errs(), &M); }

  // mask generator
  bool genMaskOk = vectorizer->generateMasks(vecInfo, *maskAnalysis, *LI);
  if (!genMaskOk)
    llvm_unreachable("mask generation failed.");

  // control conversion
  bool linearizeOk =
      vectorizer->linearizeCFG(vecInfo, *maskAnalysis, *LI, DT);
  if (!linearizeOk)
    llvm_unreachable("linearization failed.");

  const DominatorTree domTreeNew(
      *vecInfo.getMapping().scalarFn); // Control conversion does not preserve
                                       // the domTree so we have to rebuild it
                                       // for now

  // FIXME do this for tripCount == vectorWidth loops
  // auto *ExitingBI = cast<BranchInst>(ExitingBlock->getTerminator());
  // if (L.contains(ExitingBI->getSuccessor(0)))
  //   ExitingBI->setCondition(ConstantInt::getFalse(ExitingBI->getContext()));
  // else
  //   ExitingBI->setCondition(ConstantInt::getTrue(ExitingBI->getContext()));

  ValueToValueMapTy vecInstMap;
  bool vectorizeOk = vectorizer->vectorize(vecInfo, domTreeNew, *LI, *SE, *MDR, &vecInstMap);
  if (!vectorizeOk)
    llvm_unreachable("vector code generation failed");

// embed vectorized loop in CFG
  embedVectorizedLoop(L, vecInstMap, vecInfo, VectorWidth, tripAlign);

// restore analysis structures
  DT.recalculate(F);
  PDT.recalculate(F);

  return true;
}

bool LoopVectorizer::vectorizeLoopOrSubLoops(Loop &L) {
  if (vectorizeLoop(L))
    return true;

  bool Changed = false;
  for (Loop* SubL : L)
    Changed |= vectorizeLoopOrSubLoops(*SubL);

  return Changed;
}

bool LoopVectorizer::runOnFunction(Function &F) {
  if (getenv("RV_DISABLE")) return false;

  IF_DEBUG { errs() << " -- module before RV --\n"; F.getParent()->dump(); }

  Report() << "loopVecPass: run on " << F.getName() << "\n";
  bool Changed = false;

  // query analysis results
  this->LI = &getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
  this->SE = &getAnalysis<ScalarEvolutionWrapperPass>().getSE();
  this->MDR = &getAnalysis<MemoryDependenceWrapperPass>().getMemDep();
  this->F = &F;

  TargetTransformInfo & tti = getAnalysis<TargetTransformInfoWrapperPass>().getTTI(F);
  TargetLibraryInfo & tli = getAnalysis<TargetLibraryInfoWrapperPass>().getTLI();

  // setup vectorizer session
  PlatformInfo platInfo(*F.getParent(), &tti, &tli);

  // TODO query target capabilities
  bool useSSE = false, useAVX = true, useAVX2 = true, useImpreciseFunctions = true;
  addSleefMappings(useSSE, useAVX, useAVX2, platInfo, useImpreciseFunctions);
  vectorizer.reset(new VectorizerInterface(platInfo));

  reda.reset(new ReductionAnalysis(F, *LI));
  reda->analyze();


  for (Loop *L : *LI)
    Changed |= vectorizeLoopOrSubLoops(*L);


  IF_DEBUG { errs() << " -- module after RV --\n"; F.getParent()->dump(); }

  // cleanup
  reda.reset();
  vectorizer.reset();
  LI = nullptr;
  SE = nullptr;
  MDR = nullptr;
  return Changed;
}

void LoopVectorizer::getAnalysisUsage(AnalysisUsage &AU) const {
  AU.addRequired<MemoryDependenceWrapperPass>();
  AU.addRequired<LoopInfoWrapperPass>();
  AU.addRequired<DominatorTreeWrapperPass>();
  AU.addRequired<ScalarEvolutionWrapperPass>();
  AU.addRequired<PostDominatorTreeWrapperPass>();

  // PlatformInfo
  AU.addRequired<TargetTransformInfoWrapperPass>();
  AU.addRequired<TargetLibraryInfoWrapperPass>();
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
