#include "rv/session.h"

#include "report.h"
#include "rvConfig.h"

#include "rv/vectorizationInfo.h"
#include "rv/PlatformInfo.h"

#include "rv/analysis/reductionAnalysis.h"
#include "rv/analysis/DFG.h"
#include "rv/analysis/BranchDependenceAnalysis.h"
#include "rv/analysis/VectorizationAnalysis.h"

#include "rv/transform/maskExpander.h"
#include "rv/transform/divLoopTrans.h"
#include "rv/transform/redOpt.h"
#include "rv/transform/Linearizer.h"
#include "rv/transform/bosccTransform.h"
#include "rv/transform/splitAllocas.h"
#include "rv/transform/structOpt.h"
#include "rv/transform/srovTransform.h"

#include "native/NatBuilder.h"

#include "rv/transform/irPolisher.h"

#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/Dominators.h>
#include <llvm/Analysis/ScalarEvolution.h>
#include <llvm/Analysis/MemoryDependenceAnalysis.h>
#include <llvm/Transforms/Utils/ValueMapper.h>
#include <llvm/IR/Verifier.h>

using namespace llvm;

namespace rv {

Session::Session(PlatformInfo & _platInfo, DominatorTree & _domTree, PostDominatorTree & _postDomTree, LoopInfo & _loopInfo, ScalarEvolution & _scev, MemoryDependenceResults & _MDR, llvm::BranchProbabilityInfo * _pbInfo)
: platInfo(_platInfo)
, domTree(_domTree)
, postDomTree(_postDomTree)
, loopInfo(_loopInfo)
, scev(_scev)
, MDR(_MDR)
, pbInfo(_pbInfo)
{}

Session::~Session()
{}

// vectorize
void
Session::run(VectorizationInfo & vecInfo, OptConfig optConfig, ValueToValueMapTy * vecInstMap) {
  // determines value and control shapes
  VAConfig vaConfig;
  auto & sourceFunc = vecInfo.getScalarFunction();

// Vectorization Analysis
  CDG cdg(postDomTree);
  DFG dfg(domTree);

  BranchDependenceAnalysis BDA(sourceFunc, cdg, dfg, loopInfo);
  VectorizationAnalysis vea(vaConfig, platInfo, vecInfo, BDA, loopInfo);
  vea.analyze();



// control conversion
  // use a fresh domtree here
  // DominatorTree fixedDomTree(vecInfo.getScalarFunction()); // FIXME someone upstream broke the domtree
  domTree.recalculate(vecInfo.getScalarFunction());

  // lazy mask generator
  MaskExpander maskEx(vecInfo, domTree, postDomTree, loopInfo);

  // convert divergent loops inside the region to uniform loops
  DivLoopTrans divLoopTrans(platInfo, vecInfo, maskEx, domTree, loopInfo);
  divLoopTrans.transformDivergentLoops();

  postDomTree.recalculate(vecInfo.getScalarFunction()); // FIXME
  domTree.recalculate(vecInfo.getScalarFunction()); // FIXME

  // insert BOSCC branches if desired
  if (optConfig.enableHeuristicBOSCC && pbInfo) {
    BOSCCTransform bosccTrans(vecInfo, platInfo, maskEx, domTree, postDomTree, loopInfo, pbInfo);
    bosccTrans.run();
  }

  // expand masks after BOSCC
  maskEx.expandRegionMasks();

  postDomTree.recalculate(sourceFunc);
  domTree.recalculate(sourceFunc);

  IF_DEBUG {
    errs() << "--- VecInfo before Linearizer ---\n";
    vecInfo.dump();
  }

  // FIXME use external reduction analysis result (if available)
  ReductionAnalysis reda(vecInfo.getScalarFunction(), loopInfo);
  auto * hostLoop = loopInfo.getLoopFor(&vecInfo.getEntry());
  if (hostLoop) reda.analyze(*hostLoop);

  // optimize reduction data flow
  ReductionOptimization redOpt(vecInfo, reda, domTree);
  redOpt.run();

  // partially linearize acyclic control in the region
  Linearizer linearizer(vecInfo, maskEx, domTree, loopInfo);
  linearizer.run();

  IF_DEBUG {
    errs() << "--- VecInfo after Linearizer ---\n";
    vecInfo.dump();
  }


// CodeGen
  // split structural allocas
  if (optConfig.enableSplitAllocas) {
    SplitAllocas split(vecInfo);
    split.run();
  } else {
    Report() << "Split allocas opt disabled (RV_DISABLE_SPLITALLOCAS != 0)\n";
  }

  // transform allocas from Array-of-struct into Struct-of-vector where possibe
  if (optConfig.enableStructOpt) {
    StructOpt sopt(vecInfo, platInfo.getDataLayout());
    sopt.run();
  } else {
    Report() << "Struct opt disabled (RV_DISABLE_STRUCTOPT != 0)\n";
  }

  // Scalar-Replication-Of-Varying-(Aggregates): split up structs of vectorizable elements to promote use of vector registers
  if (optConfig.enableSROV) {
    SROVTransform srovTransform(vecInfo, platInfo);
    srovTransform.run();
  } else {
    Report() << "SROV opt disabled (RV_DISABLE_SROV != 0)\n";
  }

  if (hostLoop) reda.analyze(*hostLoop);

// vectorize with native
  native::NatBuilder natBuilder(optConfig, platInfo, vecInfo, domTree, MDR, scev, reda);
  natBuilder.vectorize(true, vecInstMap);

  // IR Polish phase: promote i1 vectors and perform early instruction (read: intrinsic) selection
  if (optConfig.enableIRPolish) {
    IRPolisher polisher(vecInfo.getVectorFunction(), platInfo.getVectorISA());
    polisher.polish();
    Report() << "IR Polisher enabled (RV_ENABLE_POLISH != 0)\n";
  }

  IF_DEBUG verifyFunction(vecInfo.getVectorFunction());
}


}
