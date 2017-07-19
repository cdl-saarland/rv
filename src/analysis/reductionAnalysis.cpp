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
  // default bottom rules
  if (A == RedKind::Bot) return B;
  if (B == RedKind::Bot) return A;

  // fallback rule
  if (A != B) return RedKind::Top;
  return A; // A == B
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

// struct StridePattern
void
StridePattern::dump() const {
  errs() << "StridePattern { phi = " << *phi << ", redInst = " << *reductor << " }\n";
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

#if 0
bool
Reduction::matchStridedPattern(StridePattern & pat) const {

// #define RESON(M) { if (reason) *reason=M; }
#ifdef RV_DEBUG
#define REASON(M) { outs() << M << "\n"; }
#else
#define REASON(M) {}
#endif

  if (kind != RedKind::Add) { REASON("not an Add reduction") return false; }
  if (elements.size() != 2) { REASON("not two elements") return false; }

  auto it = elements.begin();
  auto * firstInst = *it++;
  auto * secInst =  *it;

  // match one phi node and one instruction (TODO allow strided recurrences)
  auto * headerPhi = isa<PHINode>(firstInst) ? cast<PHINode>(firstInst) : dyn_cast<PHINode>(secInst);
  Instruction * redInst = isa<PHINode>(firstInst) ? secInst : firstInst;
  if (!headerPhi || !redInst) { REASON("could not match header phi or reductor") return false; }

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
    REASON("unrecognized opcode")
    return false;
  }

// parse constant (oInc)
  Constant* firstConst = dyn_cast<Constant>(redInst->getOperand(0));
  Constant* secConst = dyn_cast<Constant>(redInst->getOperand(1));

  // at least one op needs to be constant
  if (!firstConst && !secConst) {
    REASON("neither reductor operand is a constant")
    return false;
  }

  Constant * incConst = firstConst ? firstConst : secConst;
  int64_t inc;
  if (auto * intIncrement = dyn_cast<ConstantInt>(incConst)) {
    inc =  sign * intIncrement->getSExtValue();
  } else if (auto * fpInc = dyn_cast<ConstantFP>(incConst)) {
    REASON("TODO implement floating point strides (fast math)")
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
#undef SET_MSG
}
#endif



// ReductionAnalysis


ReductionAnalysis::ReductionAnalysis(Function & _func, const LoopInfo & _loopInfo)
: loopInfo(_loopInfo)
{}

ReductionAnalysis::~ReductionAnalysis() {
  clear();
}

void
ReductionAnalysis::clear() {
  SmallPtrSet<void*, 32> seen;

  for (auto itSP : stridePatternMap) {
    if (seen.insert(itSP.second).second) {
      delete itSP.second;
    }
  }
  stridePatternMap.clear();

  for (auto itRed : reductMap) {
    if (seen.insert(itRed.second).second) {
      delete itRed.second;
    }
  }
  reductMap.clear();
}


StridePattern*
ReductionAnalysis::tryMatchStridePattern(PHINode & headerPhi) {
#ifdef RV_DEBUG
#define REASON(M) { outs() << "no stride pattern: " << M << "\n"; }
#else
#define REASON(M) {}
#endif

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
  auto * redInst = dyn_cast<Instruction>(headerPhi.getIncomingValue(loopIndex));
  if (!redInst) {
    IF_DEBUG_RED { errs() << "red: loop carried valus is not an instruction " << headerPhi.getName() << "\n"; }
    return nullptr;
  }

  // match opCode
  auto oc = redInst->getOpcode();
  int64_t sign = 0;
  if (oc == Instruction::Add || oc == Instruction::FAdd) {
    sign = 1;
  } else if (oc == Instruction::Sub || oc == Instruction::FSub) {
    sign = -1;
  } else {
    REASON("unrecognized opcode")
    return nullptr;
  }

// parse constant (oInc)
  Constant* firstConst = dyn_cast<Constant>(redInst->getOperand(0));
  Constant* secConst = dyn_cast<Constant>(redInst->getOperand(1));

  // at least one op needs to be constant
  if (!firstConst && !secConst) {
    REASON("neither reductor operand is a constant")
    return nullptr;
  }

// the header phi must be used directly (TODO allow Trunc/SExt/ZExt) by the reductor
  int phiIdx = firstConst == redInst->getOperand(0) ? 1 : 0;
  if (redInst->getOperand(phiIdx) != &headerPhi) {
    REASON("increment does not use phi node direcly")
    return nullptr;
  }

// is the increment constant a valid stride?
  Constant * incConst = firstConst ? firstConst : secConst;
  int64_t inc;
  if (auto * intIncrement = dyn_cast<ConstantInt>(incConst)) {
    inc =  sign * intIncrement->getSExtValue();
  } else if (auto * fpInc = dyn_cast<ConstantFP>(incConst)) {
    REASON("TODO implement floating point strides (fast math)")
    return nullptr; // TODO allow natural number fp increments in fast-math
  }

// match.
  IF_DEBUG_RED { errs() << "red: recognized: "; }

  auto *sp = new
    StridePattern{
        initIndex,
        loopIndex,
        &headerPhi,
        redInst,
        inc
    };

  stridePatternMap[&headerPhi] = sp;
  stridePatternMap[redInst] = sp;

  IF_DEBUG_RED { sp->dump(); }

  return sp;

#undef REASON
}

#if 0
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

typedef std::vector<Instruction*> NodeStack;

static void
FilterForwardUses(Reduction & red, PHINode & seed) {
  NodeStack stack;
  stack.push_back(&seed);
  std::set<Instruction*> seen;

  while (!stack.empty()) {
    auto * inst = stack.back();
    stack.pop_back();
    if (!seen.insert(inst).second) continue; // already visited

    for (auto & itUse : inst->uses()) {
      auto * userInst = cast<Instruction>(itUse.getUser());
      stack.push_back(userInst);
    }
  }

  // filter out unseen elements
  decltype(seen)::iterator it = red.elements.begin(), itEnd = red.elements.end();
  for (; it != itEnd; ) {
    auto * inst = *it;
    if (!seen.count(inst)) {
      it = red.elements.erase(it);
    } else {
      ++it;
    }
  }
}

static RedKind
ClassifyReduction(Reduction & red) {
  RedKind kind = RedKind::Bot;
  for (auto * inst : red.elements) {
    auto nodeKind = InferRedKind(*inst);

    if (nodeKind != RedKind::Bot) {
      // verify that there is exactly one incoming operand from the chain
      bool foundChainOperand = false;
      for (size_t i = 0; i < inst->getNumOperands(); ++i) {
        auto * opInst = dyn_cast<Instruction>(inst->getOperand(i));
        if (opInst && red.elements.count(opInst)) {
          if (foundChainOperand) {
            nodeKind = RedKind::Top; // multiple chain inputs -> jump to top
            break;
          } else {
            foundChainOperand = true;
          }
        }
      }
    }

    // TODO verify that reduction
    kind = JoinKinds(kind, nodeKind);
    if (kind == RedKind::Top) return kind;
  }
  return kind;
}

void
ReductionAnalysis::analyze() {
  clear();

// init work list (loop header phis for now)
  std::vector<Loop*> loopStack;
  for (auto * l : loopInfo) {
    loopStack.push_back(l);
  }

  std::vector<PHINode*> seedNodes;
  NodeStack nodeStack;
  while (!loopStack.empty()) {
    auto * loop = loopStack.back();
    loopStack.pop_back();

    // bootstrap with header phis
    for (auto & inst : *loop->getHeader()) {
      auto * phi = dyn_cast<PHINode>(&inst);
      if (!phi) break;

      // try to match a known recurrence pattern
      if (tryMatchStridePattern(*phi)) {
        continue;
      }

      // this phi node is not part of an inductive pattern
      seedNodes.push_back(phi);

      auto * node = new Reduction(*loop, *phi);
      reductMap[phi] = node;
      nodeStack.push_back(phi);
    }

    for (Loop * childLoop : *loop) {
      loopStack.push_back(childLoop);
    }
  }

// work list for general value reduction (backward scan)
  while (!nodeStack.empty()) {
    auto * inst = nodeStack.back();
    nodeStack.pop_back();

    auto & redGroup = *getReductionInfo(*inst);

    for (Value * opVal : inst->operands()) {
      // uint opIdx = itUse.getOperandNo();
      auto * opInst = dyn_cast<Instruction>(opVal);
      if (!opInst) {
        IF_DEBUG_RED { errs() << "non-inst op: " << *opVal << "\n"; }
        continue;
      }

      // check whether this node has a stride pattern
      if (canReconstructInductively(*opInst)) {
        IF_DEBUG_RED { errs() << "inductive operand " << *opInst << ". skip.\n"; }
        continue;
      }

      // check if we are about to merge a chain
      auto * opGroup = getReductionInfo(*opInst);

      // check whether this user is loop-carried
      auto *opLoop = loopInfo.getLoopFor(opInst->getParent());
      if (!opLoop || // clear outside user
          (opLoop != redGroup.levelLoop && opLoop->contains(redGroup.levelLoop))) { // loop above levelLoop
        IF_DEBUG_RED { errs() << "outside operand " << *opInst << ". skip.\n"; }
        continue;
      }

      IF_DEBUG_RED { errs() << "inspecting: " << *opInst << " ..\n\t"; }

      // otw the operation needs to be attached to one of the chains
      if (!opGroup) {
        // first visit -> add to group
        addToGroup(redGroup, *opInst);
        // loop level is preserved
        IF_DEBUG_RED { errs() << "added.\n"; }

      } else if (opGroup == &redGroup) {
        // we reached the header phi of this group -> keep
        if (opGroup->levelLoop && isHeaderPhi(*opInst, *opGroup->levelLoop)) {
          IF_DEBUG_RED { errs() << "reached header phi. keep.\n"; }
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

// general reductions (forward scan)
  std::set<Reduction*> seen;
  for (auto * phi : seedNodes) {
    auto * red = getReductionInfo(*phi);
    assert(red);
    if (!seen.insert(red).second) continue;
    FilterForwardUses(*red, *phi);

    // TODO classify reduction set after filtering
    red->kind = ClassifyReduction(*red);
  }
}

StridePattern *
ReductionAnalysis::getStrideInfo(Instruction & inst) const {
  auto itSP = stridePatternMap.find(&inst);
  if (itSP == stridePatternMap.end()) {
    return nullptr;
  } else {
    return itSP->second;
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
// clone stride patterns
  std::vector<StridePattern*> spUpdateVec;
  for (auto itSP : stridePatternMap) {
    auto * sp = itSP.second;

    if (!cloneMap.count(sp->reductor)) continue;

    auto * clonedReduct = cast<Instruction>(cloneMap[sp->reductor]);
    if (!clonedReduct) continue;

    auto * clonedPhi = cast<PHINode>(cloneMap[sp->phi]);
    assert(clonedPhi);

    auto *clonedSP = new
      StridePattern{
          sp->loopInitIdx,
          sp->latchIdx,
          clonedPhi,
          clonedReduct,
          sp->inc
    };
    spUpdateVec.push_back(clonedSP);
  }

  for (auto * sp : spUpdateVec) {
    stridePatternMap[sp->phi] = sp;
    stridePatternMap[sp->reductor] = sp;
  }

// clone complex reduction patterns
  std::map<Reduction*, Reduction*> copyMap;
  std::vector<std::pair<Instruction*, Reduction*>> updateVec;
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
      auto clonedHeader = cloneMap[origRed->levelLoop->getHeader()];
      assert(clonedHeader); // FIXME this must not break
#if 0
      if (!clonedHeader) IF_DEBUG_RED { errs() << "red: warning: levelLoop was not cloned. Keeping original levelLoop!\n"; }
#endif
      Loop * clonedLoop = clonedHeader ? LI.getLoopFor(cast<BasicBlock>(clonedHeader)) : origRed->levelLoop;
      targetRed = new Reduction(*clonedLoop, origRed->kind);
      copyMap[origRed] = targetRed;
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

void
ReductionAnalysis::print(raw_ostream & out) const {
  out << "-- Recurrence Analysis --\n";

  out << "# Induction recurrences:\n";
  {
    std::set<StridePattern*> seen;
    for (auto it : stridePatternMap) {
      if (!seen.insert(it.second).second) continue;
      it.second->dump();
    }
  }

  out << "# General reductions\n";
  {
    std::set<Reduction*> seen;
    for (auto it : reductMap) {
      if (!seen.insert(it.second).second) continue;
      it.second->dump();
    }
  }
  out << "-- End of Recurrence Analysis --\n";
}


} // namespace rv
