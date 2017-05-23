//===- reductionAnalysis.cpp ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//

#include "rv/analysis/reductionAnalysis.h"

#include <llvm/IR/Function.h>
#include <llvm/IR/Constants.h>
#include <llvm/Support/raw_ostream.h>

#include "rvConfig.h"
#include "rv/vectorShape.h"

#if 1
#define IF_DEBUG_RED IF_DEBUG
#else
#define IF_DEBUG_RED if (false)
#endif

using namespace llvm;

namespace rv {

// struct Reduction


void
Reduction::dump() const {
  errs() << "Reduction { " << phi.getName() << " reductor " << reductorInst << " with neutral elem " << neutralElem << "}\n";
}


rv::VectorShape
Reduction::getShape(int vectorWidth) {
  auto & redInst = getReductor();

  if (redInst.getOpcode() != Instruction::Add) {
    errs() << redInst << "\n";
    return VectorShape::varying();
  }

  auto *inConst = dyn_cast<ConstantInt>(&getReducibleValue());

  if (!inConst) {
    return VectorShape::varying();
  }
  errs() << *inConst << "\n";

  auto constInc = inConst->getSExtValue();

  return VectorShape::strided(constInc, constInc * vectorWidth);
}




// ReductionAnalysis


ReductionAnalysis::ReductionAnalysis(Function & _func, const LoopInfo & _loopInfo)
: loopInfo(_loopInfo)
{}

ReductionAnalysis::~ReductionAnalysis() {
  SmallPtrSet<Reduction*, 16> seen;

  for (auto itRed : reductMap) {
    if (seen.insert(itRed.second).second) {
      delete itRed.second;
    }
  }
}


Constant *
ReductionAnalysis::inferNeutralElement(Instruction & reductInst) {
  auto * reductTy = reductInst.getType();
  switch(reductInst.getOpcode()) {
    case Instruction::Add:
    case Instruction::Sub:
    case Instruction::FAdd:
    case Instruction::FSub:
      return Constant::getNullValue(reductTy);

    case Instruction::Mul:
      return ConstantInt::getSigned(reductTy, 1);
    case Instruction::FMul:
      return ConstantFP::get(reductTy, 1.0);

    default:
      IF_DEBUG_RED { errs() << "red: Unknown neutral element for " << reductInst << "\n"; }
      return nullptr;
  };
}

Reduction*
ReductionAnalysis::tryInferReduction(PHINode & headerPhi) {
// phi must be loop carried
  auto * reductLoop = loopInfo.getLoopFor(headerPhi.getParent());
  if (!reductLoop) {
    IF_DEBUG_RED { errs() << "red: not a loop carried phi " << headerPhi.getName() << "\n"; }
    return nullptr; // not a loop carried phi
  }

// expect two inputs (atm)
  int numIncoming = headerPhi.getNumIncomingValues();
  if (numIncoming != 2) {
    IF_DEBUG_RED { errs() << "red: != 2 incoming values " << headerPhi.getName() << "\n"; }
    return nullptr;
  }

// identify the index of the loop carried index
  int initIndex = 0, loopIndex = 1;
  if (reductLoop->contains(headerPhi.getIncomingBlock(initIndex))) {
      std::swap<>(initIndex, loopIndex);
  }

// loop carried input must be a instruction (carrying out the reduction)
  auto * reductInput = dyn_cast<Instruction>(headerPhi.getIncomingValue(loopIndex));
  if (!reductInput) {
    IF_DEBUG_RED { errs() << "red: loop carried valus is not an instruction " << headerPhi.getName() << "\n"; }
    return nullptr;
  }

//
  auto * neutralElem = inferNeutralElement(*reductInput);
  if (!neutralElem) {
    IF_DEBUG_RED { errs() << "red: neutral element for reduction unknown " << headerPhi.getName() << "\n"; }
    return nullptr;
  }

  IF_DEBUG_RED { errs() << "red: recognized: "; }

  auto *red = new
    Reduction(
        *neutralElem,
        *reductInput,
        *reductLoop,
        headerPhi,
        initIndex,
        loopIndex
    );

  IF_DEBUG_RED { red->dump(); }

  return red;
}

void
ReductionAnalysis::analyze(Loop & loop) {
  for (auto * childLoop : loop) analyze(*childLoop);

  for (auto & inst : *loop.getHeader()) {
    auto * phi = dyn_cast<PHINode>(&inst);
    if (!phi) break;

    auto * red = tryInferReduction(*phi);
    if (!red) continue;

    reductMap[phi] = red;
    reductMap[&red->getReductor()] = red;
  }
}

void
ReductionAnalysis::analyze() {
  for (auto * loop : loopInfo) {
    analyze(*loop);
  }
}

Reduction *
ReductionAnalysis::getReductionInfo(Instruction & inst) const {
  auto itReduct = reductMap.find(&inst);
  if (itReduct == reductMap.end()) {
    return nullptr;
  } else {
    return itReduct->second;
  }
}

void
ReductionAnalysis::updateForClones(LoopInfo & LI, ValueToValueMapTy & cloneMap) {
  std::vector<std::pair<Instruction*, Reduction*>> cloneVec;

  // check for cloned phis and register new reductions for them
  for (auto itRed : reductMap) {
    auto it = cloneMap.find(itRed.first);
    if (it == cloneMap.end()) {
      continue;
    }

    // both reductors and phis are in this map -> only remap when we see the phi
    auto * clonedPhi = dyn_cast<PHINode>(cloneMap[itRed.first]);
    if (!clonedPhi) continue; // skip reductor mappings

    auto & origRed = *itRed.second;
    auto * clonedReductor = cast<Instruction>(cloneMap[&origRed.reductorInst]);

    assert(clonedPhi && clonedReductor);

    auto * clonedLoop = LI.getLoopFor(cast<BasicBlock>(cloneMap[origRed.redLoop.getHeader()]));

    auto * clonedRed = new Reduction(
        origRed.neutralElem,
        *clonedReductor,
        *clonedLoop,
        *clonedPhi,
        origRed.initInputIndex,
        origRed.loopInputIndex
      );

    cloneVec.emplace_back(clonedPhi, clonedRed);
    cloneVec.emplace_back(clonedReductor, clonedRed);
  }

  // modify map after traversal
  for (auto it : cloneVec) {
    reductMap[it.first] = it.second;
  }
}


} // namespace rv
