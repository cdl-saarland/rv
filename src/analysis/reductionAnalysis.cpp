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

#if -1
#define IF_DEBUG_RED IF_DEBUG
#else
#define IF_DEBUG_RED if (true)
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
    case RedKind::Max: return "Max";
    case RedKind::Min: return "Min";
    default:
      abort();
  }
}

// try to infer the reduction kind of the operator implemented by inst
static RedKind
InferRedKind(Instruction & inst, Reduction & red) {
  switch (inst.getOpcode()) {
  // actually operations folding a reduction input into the chian
    case Instruction::FAdd:
    case Instruction::Add:
      return RedKind::Add;

    case Instruction::FSub:
    case Instruction::Sub: {
      auto * rhsInst = dyn_cast<Instruction>(inst.getOperand(1));
      if (rhsInst && red.contains(*rhsInst)) {
        return RedKind::Top; // TODO Sub reductions
      } else {
        return RedKind::Add;
      }
    }

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
StridePattern::print(raw_ostream & out) const {
  out << "StridePattern { phi = " << *phi << ", redInst = " << *reductor << " }\n";
}

void StridePattern::dump() const { print(errs()); }



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

bool
ReductionAnalysis::addToGroup(Reduction & redGroup, Instruction & inst) {
  bool added = redGroup.add(inst);
  if (added) {
    assert(!reductMap.count(&inst) && "overwriting a mapping!!");
    reductMap[&inst] = &redGroup;
  }
  return added;
}

bool
ReductionAnalysis::changeGroup(Reduction & redGroup, Instruction & inst) {
  auto itRed = reductMap.find(&inst);
  if (itRed != reductMap.end()) {
    if (itRed->second == &redGroup) return false; // unchanged

    itRed->second->erase(inst);
    reductMap.erase(itRed);
  }

  return addToGroup(redGroup, inst);
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

using InstInt = std::pair<Instruction*, int>;
using NodeStack = std::vector<InstInt>;

static RedKind
ClassifyReduction(Reduction & red) {
  RedKind kind = RedKind::Bot;
  for (auto * inst : red.elements) {
    auto nodeKind = InferRedKind(*inst, red);

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
ReductionAnalysis::analyze(Loop & hostLoop) {
  clear();

// init work list (loop header phis for now)
  std::vector<Loop*> loopStack;
  loopStack.push_back(&hostLoop);
#if 0
  for (auto * l : loopInfo) {
    loopStack.push_back(l);
  }
#endif

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
    }
  }

  // scc scan
  IF_DEBUG_RED { errs() << " -- reda: SCC scan --\n"; }

  for (auto * seedPhi : seedNodes) {
    std::set<Instruction*> visited;
    InstSet elements;

    nodeStack.emplace_back(seedPhi, 0);
    elements.insert(seedPhi);

    if (getReductionInfo(*seedPhi) != nullptr) {
      continue; // already part of some SCC
    }
    while (!nodeStack.empty()) {
      auto node = nodeStack.back();

      auto * inst = node.first;
      int instIdx = nodeStack.size() - 1;
      if (!visited.insert(inst).second) {
        nodeStack.pop_back();
        continue;
      }

      IF_DEBUG_RED { errs() << "inspecting: " << *inst << " ..\n\t"; }

      for (Value * opVal : inst->operands()) {
        // uint opIdx = itUse.getOperandNo();
        auto * opInst = dyn_cast<Instruction>(opVal);
        if (!opInst) {
          IF_DEBUG_RED { errs() << "\tnon-inst op: " << *opVal << "\n"; }
          continue;
        }

        // outside of relevant scope
        if (!hostLoop.contains(opInst->getParent())) {
          IF_DEBUG_RED { errs() << "\tnot carried by hostLoop: " << *opVal << "\n"; }
          continue;
        }

        // already part of some SCC
        if (getReductionInfo(*opInst) != nullptr) {
          IF_DEBUG_RED { errs() << "\talready part of an reduction: " << *opVal << "\n"; }
          continue;
        }

        // check whether this node has a stride pattern
        if (canReconstructInductively(*opInst)) {
          IF_DEBUG_RED { errs() << "\tinductive operand " << *opInst << ". skip.\n"; }
          continue;
        }

        if (elements.count(opInst)) {
          IF_DEBUG_RED { errs() << "\tcycle!: " << *opInst << " ..\n\t"; }
          // add all instructions that are on this path to the SCC
          for (int p = instIdx; p > 0; ) {
            auto pathNode = nodeStack[p];
            elements.insert(pathNode.first);
            p = pathNode.second;
          }
        } else {
          // descend further into operands
          nodeStack.emplace_back(opInst, instIdx);
        }
      }
    }

    // convert the SEE into a reduction object
    auto * red = new Reduction(elements);
    red->kind = ClassifyReduction(*red);
    red->levelLoop = &hostLoop;

    // register with the analysis
    for (auto * inst : red->elements) {
      reductMap[inst] = red;
    }

    red->dump();
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
ReductionAnalysis::dump() const { print(errs()); }

void
ReductionAnalysis::print(raw_ostream & out) const {
  out << "-- Recurrence Analysis --\n";

  out << "# Induction recurrences:\n";
  {
    std::set<StridePattern*> seen;
    for (auto it : stridePatternMap) {
      if (!seen.insert(it.second).second) continue;
      it.second->print(out);
    }
  }

  out << "# General reductions\n";
  {
    std::set<Reduction*> seen;
    for (auto it : reductMap) {
      if (!seen.insert(it.second).second) continue;
      it.second->print(out);
    }
  }
  out << "-- End of Recurrence Analysis --\n";
}


} // namespace rv
