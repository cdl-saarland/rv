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
#include <llvm/IR/IRBuilder.h>

#include "rvConfig.h"
#include "rv/vectorShape.h"

#if 1
#define IF_DEBUG_RED IF_DEBUG
#else
#define IF_DEBUG_RED if (false)
#endif

using namespace llvm;

namespace rv {


static
const char *
to_string(RedKind red) {
  switch (red) {
    case RedKind::Bot: return "Bot";
    case RedKind::Top: return "Top";
    case RedKind::Add: return "Add";
    case RedKind::Mul: return "Mul";
    case RedKind::And: return "And";
    case RedKind::Or: return "Or";
    default:
      abort();
  }
}

// try to infer the reduction kind of the operator implemented by inst
static RedKind
InferRedKind(Instruction & inst) {
  switch (inst.getOpcode()) {
    case Instruction::FAdd:
    case Instruction::Add:
      return RedKind::Add;

    case Instruction::FMul:
    case Instruction::Mul:
      return RedKind::Mul;

    case Instruction::Or:
      return RedKind::Or;

    case Instruction::And:
      return RedKind::And;

    default:
      return RedKind::Top;
  }
  abort();
}

// materialize a single instance of firstArg [[RedKind~OpCode]] secondArg
static Instruction&
CreateReduce(IRBuilder<> & builder, Value & firstArg, Value & secondArg) {
  abort();
}

// reduce the vector @vectorVal to a scalar value (using redKind)
static Value &
CreateVectorReduce(IRBuilder<> & builder, RedKind redKind, Value & vectorVal) {
  abort();
}




// struct Reduction


void
Reduction::dump() const {
  print(errs());
}

void
Reduction::print(raw_ostream & out) {
   out << "Reduction { " << phi.getName() << " reductor " << reductorInst << " with neutral elem " << neutralElem << "}\n";
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

RedNode &
ReductionAnalysis::requestNode(PHINode & chainPhi, Instruction & inst) {
  auto it = reductMap.find(&inst);
  if (it == reductMap.end()) {
    auto * entry = new RedNode(inst, chainPhi, RedKind::Bot);
    reductMap[&inst] = entry;
    return *entry;
  } else {
    return *it->second;
  }
}

static RedKind JoinKinds(RedKind A, RedKind B) {
  if (A == RedKind::Bot) return B;
  if (B == RedKind::Bot) return A;
  if (A != B) return RedKind::Top;
  return A;
}

void
ReductionAnalysis::visitHeaderPhi(RedNode & phiNode, NodeStack & nodeStack) {
  auto & BB = *phiNode.recPhi.getParent();
  auto * pLoop = loopInfo.getLoopFor(&BB);
  assert(pLoop);

  auto & headerPhi = phiNode.recPhi;
  auto & loop = *pLoop;

  for (size_t i = 0; i < headerPhi.getNumIncomingValues(); ++i) {

    auto & inVal = *headerPhi.getIncomingValue(i);
    auto * inInst = dyn_cast<Instruction>(&inVal);

    // loop carried input input
    if (inInst && loop.contains(inInst->getParent())) {
      auto & inputLoop = *loopInfo.getLoopFor(inInst->getParent());
      auto * inRed = getReductionInfo(*inInst);

      // input from a nested loop (already analyzed)
      if (&inputLoop != &loop) {
        if (!inRed) {
          // non-reduction input
          continue;
        } else {
        // reduction has to be compatible (for now)
          // TODO this is actually only the case if the reduction can not be completly reduced outside of that loop
          auto joined = JoinKinds(phiNode.kind, inRed->kind);
          updateKind(phiNode, joined);
          continue;
        }
      }

      if (inRed) {
        auto & headerPhi = inRed->getHeaderPhi();

        if (&headerPhi == inInst) {
          // PHI recurrence
          phiNode.kind = inRed->kind; // true?
          continue;

        } else {
          auto instKind = inferKindFromInst(*inInst);
          auto & instNode = requestNode(headerPhi, *inInst);

          // chain in the local kind of this operation
          auto joined = JoinKinds(instKind, phiNode.kind); // TODO define
          phiNode.kind = joined;
          instNode.kind = joined;

          // if this kind is incompatible abort this reduction chain
          if (joined == RedKind::Top) {
            // TODO we need to taint the entire reduction chain
          }

          instNode.kind = phiNode.kind;
          // uncharted input
        }

      } else {
        // TODO this is an unmapped instruction
      }

    } else {
      // the input is loop invariant
      continue;
    }
  }

  abort(); // TODO implement
}

void
ReductionAnalysis::visitInputNode(RedNode & phiNode, NodeStack & nodeStack) {
  abort(); // TODO implement
}

typedef std::pair<Instruction*, Reduction*> RedElem;
typedef std::vector<RedElem> NodeStack;

void
ReductionAnalysis::analyze(Loop & loop) {
  for (auto * childLoop : loop) analyze(*childLoop);

  NodeStack nodeStack;
  for (auto & inst : *loop.getHeader()) {
    auto * phi = dyn_cast<PHINode>(&inst);
    if (!phi) break;

    auto * node = new Reduction(loop, phi);
    reductMap[phi] = node;
    nodeStack.emplace_back(phi, node);
  }

  // TODO run work list algorithm
  while (!nodeStack.empty()) {
    auto * node = nodeStack.back();
    nodeStack.pop_back();

    if (node->isHeaderPhi()) {
      visitHeaderPhi(*node, nodeStack);
    } else {
      visitInputNode(*node, nodeStack);
    }
  }
}

void
ReductionAnalysis::analyze() {
  for (auto * loop : loopInfo) {
    analyze(*loop);
  }
}

RedNode *
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
  abort(); // TODO implement
#if 0
  std::vector<std::pair<Instruction*, RedNode*>> cloneVec;

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
#endif
}


} // namespace rv
