#include "rv/transform/redOpt.h"

#include "llvm/IR/Dominators.h"
#include "rv/vectorizationInfo.h"
#include "rv/analysis/reductionAnalysis.h"

#include "rv/transform/redTools.h"
#include "report.h"

using namespace llvm;
using namespace rv;


ReductionOptimization::ReductionOptimization(VectorizationInfo & _vecInfo, ReductionAnalysis & _reda, DominatorTree & _dt)
: vecInfo(_vecInfo)
, reda(_reda)
, dt(_dt)
{}

bool
ReductionOptimization::optimize(PHINode & phi, Reduction & red) {
  auto phiShape = vecInfo.getVectorShape(phi);

  if (phi.getNumUses() == 1) return false; // only one user -> nothing to optimize here

  auto & neutral = GetNeutralElement(red.kind, *phi.getType());
  phi.replaceAllUsesWith(&neutral);

  auto * inAtZero = dyn_cast<Instruction>(phi.getIncomingValue(0));
  int latchIdx = (inAtZero && vecInfo.inRegion(*inAtZero)) ? 0 : 1;

  auto * latchInst = dyn_cast<Instruction>(phi.getIncomingValue(latchIdx));
  assert(latchInst && "recurrence was annotated as reduction!");

  auto itLatch = latchInst->getIterator();
  ++itLatch;

  // fold accumulator into latch update after chains have been merged
  IRBuilder<> builder(latchInst->getParent(), itLatch);
  auto & latchUpdate = CreateReductInst(builder, red.kind, phi, *latchInst);
  vecInfo.setVectorShape(latchUpdate, phiShape);

  // use the late latch update instead
  phi.setIncomingValue(latchIdx, &latchUpdate);

  return true;
}

bool
ReductionOptimization::run() {
  if (!vecInfo.getRegion()) return false; // not applicable in WFV mode (wouldn't help)

  size_t numOptimizedReductions = 0;

  for (auto & inst : vecInfo.getEntry()) {
    auto * phi = dyn_cast<PHINode>(&inst);
    if (!phi) break;

    auto * redInfo = reda.getReductionInfo(*phi);
    if (!redInfo) continue;

    // optimize this reduction header phi
    bool changedRed = optimize(*phi, *redInfo);
    if (changedRed) {
      numOptimizedReductions++;
    }
  }

  Report() << "redOpt: optimized " << numOptimizedReductions << " reduction chains.\n";

  return numOptimizedReductions != 0;
}
