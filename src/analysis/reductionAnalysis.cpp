//*/===- reductionAnalysis.cpp ----------------*- C++ -*-===//
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

RedKind JoinKinds(RedKind A, RedKind B) {
  if (A == RedKind::Bot) return B;
  if (B == RedKind::Bot) return A;
  if (A != B) return RedKind::Top;
  return A;
}


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
RedKind
InferRedKind(Instruction & inst) {
  switch (inst.getOpcode()) {
  // actually operations folding a reduction input into the chian
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

  // preserving operations
    case Instruction::Select:
    case Instruction::PHI:
      return RedKind::Bot;

  // unkown -> unrecognized operation in reduction chain
    default:
      return RedKind::Top;
  }
  abort();
}

Constant&
GetNeutralElement(RedKind redKind, Type & chainTy) {
  switch(redKind) {
    case RedKind::Or:
      assert(chainTy.isIntegerTy());
      return *ConstantInt::getNullValue(&chainTy);
    case RedKind::And:
      assert(chainTy.isIntegerTy());
      return *ConstantInt::getAllOnesValue(&chainTy);

    case RedKind::Add:
      return *(chainTy.isFloatTy() ? ConstantFP::get(&chainTy, 0.0) : ConstantInt::getNullValue(&chainTy));

    case RedKind::Mul:
      return *(chainTy.isFloatTy() ? ConstantFP::get(&chainTy, 1.0) : ConstantInt::get(&chainTy, 1, false));

    default:
      IF_DEBUG_RED { errs() << "red: Unknown neutral element for " << to_string(redKind) << "\n"; }
      abort();
  }
}




// struct Reduction


void
Reduction::dump() const {
  print(errs());
}

void
Reduction::print(raw_ostream & out) const {
  std::string loopName =
    levelLoop ? "(" + std::to_string(levelLoop->getLoopDepth()) + ") " + levelLoop->getName().str()
              : "<none>";

   out << "Reduction { levelLoop = " << loopName << " redKind " << to_string(kind) << " elems:\n";
   for (const Instruction * elem : elements) {
     out << "- " << *elem << "\n";
   }
   out << "}\n";
}

bool
Reduction::matchStridedPattern(StridePattern & pat) const {
  abort(); // TODO implement

  if (kind != RedKind::Add) return false;
  if (elements.size() != 2) return false;

  auto it = elements.begin();
  auto * firstInst = *it++;
  auto * secInst =  *it;

  // match one phi node and one instruction (TODO allow strided recurrences)
  auto * headerPhi = isa<PHINode>(firstInst) ? cast<PHINode>(firstInst) : dyn_cast<PHINode>(secInst);
  Instruction * redInst = isa<PHINode>(firstInst) ? secInst : firstInst;
  if (!headerPhi || !redInst) return false;

  // oHeaderPhi = headerPhi;

// match reductor operand position
  int latchIdx = headerPhi->getIncomingValue(0) == redInst ? 0 : 1;
  int loopInitIdx = 1 - latchIdx;

  // match opCode
  auto oc = redInst->getOpcode();
  int64_t sign = 0;
  if (oc == Instruction::Add || oc == Instruction::FAdd) {
    sign = 1;
  } else if (oc == Instruction::Sub || oc == Instruction::FSub) {
    sign = -1;
  } else {
    return false;
  }

// parse constant (oInc)
  Constant* firstConst = dyn_cast<Constant>(redInst->getOperand(0));
  Constant* secConst = dyn_cast<Constant>(redInst->getOperand(1));

  // at least one op needs to be constant
  if (!firstConst && !secConst) {
    return false;
  }

  Constant * incConst = firstConst ? firstConst : secConst;
  int64_t inc;
  if (auto * intIncrement = dyn_cast<ConstantInt>(incConst)) {
    inc =  sign * intIncrement->getSExtValue();
  } else if (auto * fpInc = dyn_cast<ConstantFP>(incConst)) {
    return false; // TODO allow natural number fp increments in fast-math
  }

  // struct StridePattern {
  //   Reduction & red;
  //
  //   int loopInitIdx;
  //   int latchIdx;
  //   PHINode & phi;
  //
  //   Instruction & reductor;
  //   int64_t inc; // increment value (already accounts to sign change by Instruction::Sub)
  // };
  pat = StridePattern{this, loopInitIdx, latchIdx, headerPhi, redInst, inc};
  return true;

}

rv::VectorShape
Reduction::getShape(int vectorWidth) const {
  StridePattern pat;
  if (matchStridedPattern(pat)) {
    // TODO infer alignment from phi init argument
    return VectorShape::strided(pat.inc, pat.inc * vectorWidth);
  } else {
    return VectorShape::varying();
  }
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


#if 0
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
#endif

#if 0
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

voidt
ReductionAnalysis::visitInputNode(RedNode & phiNode, NodeStack & nodeStack) {
  abort(); // TODO implement
}
#endif

bool
ReductionAnalysis::addToGroup(Reduction & redGroup, Instruction & inst) {
  bool added = redGroup.add(inst);
  if (added) {
    assert(!reductMap.count(&inst) && "overwriting a mapping!!");
    reductMap[&inst] = &redGroup;
  }
  return added;
}

void
ReductionAnalysis::foldIntoGroup(Reduction & destGroup, Reduction & srcGroup) {
  for (auto * inst : srcGroup.elements) {
    destGroup.add(*inst);
    reductMap[inst] = &destGroup;
  }
  if (destGroup.levelLoop == srcGroup.levelLoop || !srcGroup.levelLoop) {
    return;
  } else if (!destGroup.levelLoop || srcGroup.levelLoop->contains(destGroup.levelLoop)) {
    destGroup.levelLoop = srcGroup.levelLoop;
    return;
  }

  if (destGroup.levelLoop->contains(srcGroup.levelLoop)) {
    return;
  }

  // neither loop contains the other -> do not follow this leed
  abort();
}

bool
ReductionAnalysis::isHeaderPhi(Instruction & inst, Loop & loop) const {
  if (!isa<PHINode>(inst)) return false;
  return loopInfo.getLoopFor(inst.getParent()) == &loop;
}

void
ReductionAnalysis::analyze() {
// init work list (loop header phis for now)
  std::vector<Loop*> loopStack;
  for (auto * l : loopInfo) {
    loopStack.push_back(l);
  }

  typedef std::vector<Instruction*> NodeStack;
  NodeStack nodeStack;
  while (!loopStack.empty()) {
    auto * loop = loopStack.back();
    loopStack.pop_back();

    // bootstrap with header phis
    for (auto & inst : *loop->getHeader()) {
      auto * phi = dyn_cast<PHINode>(&inst);
      if (!phi) break;

      auto * node = new Reduction(*loop, *phi);
      reductMap[phi] = node;
      nodeStack.push_back(phi);
    }

    for (Loop * childLoop : *loop) {
      loopStack.push_back(childLoop);
    }
  }


// work list
  while (!nodeStack.empty()) {
    auto * inst = nodeStack.back();
    nodeStack.pop_back();

    auto & redGroup = *getReductionInfo(*inst);
    auto chainKind = redGroup.kind;

    for (Value * opVal : inst->operands()) {
      // uint opIdx = itUse.getOperandNo();
      auto * opInst = dyn_cast<Instruction>(opVal);
      if (!opInst) {
        IF_DEBUG_RED { errs() << "non-inst op: " << *opVal << "\n"; }
      }

      // check if we are about to merge a chain
      auto * opGroup = getReductionInfo(*opInst);
      auto userRedKind = InferRedKind(*inst);

      // check whether this user is loop-carried
      auto *opLoop = loopInfo.getLoopFor(opInst->getParent());
      if (!opLoop || opLoop->contains(redGroup.levelLoop)) {
        IF_DEBUG_RED { errs() << "outside operand " << *opInst << ". skip.\n"; }
        continue;
      }

      IF_DEBUG_RED { errs() << "inspecting: " << *opInst << " ..\n\t"; }

      // otw the operation needs to be attached to one of the chains
      if (!opGroup) {
         auto commonKind = JoinKinds(userRedKind, chainKind);
        // first visit -> add to group
        addToGroup(redGroup, *opInst);
        // update chain properties
        redGroup.kind = commonKind;
        // loop level is preserved

      } else if (opGroup == &redGroup) {
        // we reached the header phi of this group -> keep
        if (opGroup->levelLoop && isHeaderPhi(*opInst, *opGroup->levelLoop)) {
          IF_DEBUG_RED { errs() << "reached header phi. keep.\n"; }
        } else if (userRedKind != RedKind::Bot) {
          // we reach this reduction operation on multiple operand position -> can not privatize
          redGroup.kind = RedKind::Top;
          IF_DEBUG_RED { errs() << "multiple paths to operation: -> top\n"; }
        }
        continue; // user already inspected

      } else if (opGroup != &redGroup) {
        // TODO allow recurrences and multilevel reductions with compatible operations
        IF_DEBUG_RED { errs() << "used in incopatible nested reduction ->top.\n"; }

        // otw, we are receiving multiple reduction inputs
        foldIntoGroup(redGroup, *opGroup);
        redGroup.kind = RedKind::Top;
        delete opGroup;

        continue; // already inspected
      }

      // inspect all users of this instruction
      nodeStack.push_back(opInst);
    }
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
  std::vector<std::pair<Instruction*, Reduction*>> updateVec;
  std::map<Reduction*, Reduction*> copyMap;

  // check for cloned phis and register new reductions for them
  for (auto itRed : reductMap) {
    auto it = cloneMap.find(itRed.first);

    // instruction wasn't cloned
    if (it == cloneMap.end()) {
      continue;
    }

    // there is a clonedInst
    auto * clonedInst = cast<Instruction>(cloneMap[it->first]);
    assert(!reductMap.count(clonedInst) && "instruction already mapped!");

    // request a cloned Reduction info object
    auto * origRed = itRed.second;
    auto itCopy = copyMap.find(origRed);
    Reduction * targetRed = nullptr;
    if (itCopy != copyMap.end()) {
      targetRed = itCopy->second;
    } else {
      Loop * clonedLoop = LI.getLoopFor(cast<BasicBlock>(cloneMap[origRed->levelLoop->getHeader()]));
      assert(clonedLoop && "cloned loop not mapped!");
      targetRed = new Reduction(*clonedLoop, origRed->kind);
    }

    // transfer node to targetRed (this is a defered addToGroup)
    targetRed->elements.insert(clonedInst);
    updateVec.emplace_back(clonedInst, targetRed);
  }

  // modify map after traversal
  for (auto it : updateVec) {
    reductMap[it.first] = it.second;
  }
}


} // namespace rv
