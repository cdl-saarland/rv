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

#if 1
#define IF_DEBUG_RED IF_DEBUG
#else
#define IF_DEBUG_RED if (false)
#endif

using namespace rv;
using namespace llvm;


// struct Reduction


void
Reduction::dump() const {
  errs() << "Reduction { " << phi.getName() << " reductInst " << redInput << " with neutral elem " << neutralElem << "}\n";
}




// ReductionAnalysis


ReductionAnalysis::ReductionAnalysis(Function & _func, const LoopInfo & _loopInfo)
: func(_func)
, loopInfo(_loopInfo)
{}

ReductionAnalysis::~ReductionAnalysis() {
  for (auto itRed : reductMap) {
    delete itRed.second;
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
  }
}

void
ReductionAnalysis::analyze() {
  for (auto * loop : loopInfo) {
    analyze(*loop);
  }
}

Reduction *
ReductionAnalysis::getReductionInfo(PHINode & phi) const {
  auto itReduct = reductMap.find(&phi);
  if (itReduct == reductMap.end()) {
    return nullptr;
  } else {
    return itReduct->second;
  }
}

