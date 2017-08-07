//===- MemoryAccessGrouper.cpp -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @author montada

#include "MemoryAccessGrouper.h"

#include <llvm/Analysis/ScalarEvolutionExpressions.h>
#include <llvm/Analysis/MemoryDependenceAnalysis.h>

#include "rvConfig.h"

#define IF_DEBUG_MG if (true)

const int64_t groupLimit = 64;

using namespace llvm;
using namespace rv;

// MemoryGroup_BEGIN

typedef const SCEV *Element;

void MemoryGroup::dump() const { print(errs()); }

void
MemoryGroup::print(raw_ostream & out) const {
  out << "MemGroup {\n";
  for (size_t i = 0; i < elements.size(); ++i) {
    if (!elements[i])
      continue;
    out << i << " : " << *elements[i] << "\n";
  }
  out << "}\n";
}

MemoryGroup::MemoryGroup(const SCEV *scev) :
  topIdx(1),
  elements(1, scev) {}

MemoryGroup::MemoryGroup() :
  topIdx(0),
  elements() {}

void MemoryGroup::insert(const SCEV *scev, int offset) {
  if (elements.empty()) {
    topIdx = 1;
    elements.push_back(scev);

  } else if (offset >= 0) {
    // resize if necessary
    if(offset >= static_cast<int>(topIdx))
      elements.resize(std::max<unsigned>((unsigned) (elements.size() * 2), (unsigned) offset + 1), nullptr);

    elements[offset] = scev;
    topIdx = std::max<unsigned>(topIdx, (unsigned) offset + 1);

  } else if (offset < 0) {
    // shift everything to fit the element at the front
    unsigned shiftValue = (unsigned) -offset;
    unsigned oldSize = (unsigned) elements.size();
    elements.resize(elements.size() + shiftValue, nullptr);
    const SCEV **dataPtr = elements.data();
    memmove(dataPtr + shiftValue, dataPtr, oldSize * sizeof(Element));
    for (unsigned i = 1; i < shiftValue; ++i) {
      elements[i] = nullptr;
    }

    elements[0] = scev;
    topIdx = topIdx + shiftValue;
  }
}

// MemoryGroup_END

// MemoryAccessGrouper_BEGIN

MemoryAccessGrouper::MemoryAccessGrouper(ScalarEvolution &SE, unsigned laneByteSize) :
  SE(SE),
  laneByteSize(laneByteSize)
{}

const SCEV *MemoryAccessGrouper::add(Value *addrVal) {
  const SCEV *addrSCEV = SE.getSCEV(addrVal);
  assert(addrSCEV && "can't compute SCEV");

  // try to find existing group with constant offset to addrSCEV
  for (MemoryGroup &group : memoryGroups) {
    int64_t offset = 0;
    IF_DEBUG_MG errs() << "DIFFING " << *addrSCEV << " and " << *group[0] <<"\n";
    if (!getConstantDiff(addrSCEV, group[0], offset)) {
      IF_DEBUG_MG errs() << "\tnon const!\n";
      continue;
    }

    IF_DEBUG_MG errs() << "\tresult: " << offset << "\n";

    if (abs(offset) >= groupLimit) continue;

    IF_DEBUG_MG errs() << "== " << offset << "\n";

    group.insert(addrSCEV, offset);
    return addrSCEV;
  }

  // new group
  MemoryGroup freshGroup(addrSCEV);
  memoryGroups.push_back(freshGroup);
  return addrSCEV;
}

static int64_t
GetConstValue(const SCEV & cScev) {
  return cast<const SCEVConstant>(cScev).getAPInt().getSExtValue();
}

bool
MemoryAccessGrouper::equals(const llvm::SCEV * A, const llvm::SCEV * B) {
  int64_t delta;
  return (getConstantDiff(A, B, delta) && delta == 0);
}

bool
MemoryAccessGrouper::getConstantDiff(const llvm::SCEV * A, const llvm::SCEV * B, int64_t & oDelta) {
  IF_DEBUG_MG errs() << "MATCH: " << *A << " and " << *B << "\n";

  if (A == B) {
    oDelta = 0;
    return true;
  }
  auto aScTy = A->getSCEVType();
  auto bScTy = B->getSCEVType();

  // match "(C + x_0+..+x_n)" with "(x_0+..+x_n)"
  if (aScTy == scAddExpr) {
    const auto * lhsAdd = cast<const SCEVAddExpr>(A);
    const auto * lhsLhsConst = dyn_cast<SCEVConstant>(lhsAdd->getOperand(0));

    if (bScTy == scAddExpr) {
      const auto * rhsAdd = cast<const SCEVAddExpr>(B);
      // lhs and rhs are both adds
      if (lhsLhsConst) {
        // ops(lhs) + 1 > ops(rhs) -> fip
        if (lhsAdd->getNumOperands() + 1 == rhsAdd->getNumOperands()) {
          int64_t flippedDiff;
          bool ok = getConstantDiff(B, A, flippedDiff);
          oDelta = -flippedDiff;
          return ok;

        // ops(rhs) + 1 == ops(lhs)
        } else if (lhsAdd->getNumOperands() == rhsAdd->getNumOperands() + 1) {
          // lhs: C + x_1+..+x_n   , rhs: x'_1+..+x'_n
          // check  that x_i == x'_i
          IF_DEBUG_MG errs() << "multi ADD: " << *lhsAdd << "  " << *lhsAdd->getOperand(0) << "   " << *lhsAdd->getOperand(1) << "\n";
          for (size_t i = 1; i < lhsAdd->getNumOperands(); ++i) {
            if (!equals(lhsAdd->getOperand(i), rhsAdd->getOperand(i-1))) {
              IF_DEBUG_MG errs() << "\tmismatch @ " << i << *lhsAdd->getOperand(i) << " VS " << *rhsAdd->getOperand(i-1) << "\n";
              return false;
            }
          }

          oDelta = GetConstValue(*lhsLhsConst);
          return true;
        }
      }
    }

    // lhs: "C + X" and rhs is "X"
    if (lhsLhsConst && lhsAdd->getNumOperands() == 2) {
      IF_DEBUG_MG errs() << "Const ADD: " << *lhsAdd << "  " << *lhsAdd->getOperand(0) << "   " << *lhsAdd->getOperand(1) << "\n";
      if (equals(lhsAdd->getOperand(1), B)) {
        oDelta = GetConstValue(*lhsLhsConst);
        return true;
      }
    }
  }

  // identical operation
  if (aScTy != bScTy) {
    return false;
  }

  // structurally different but same root scev type
  switch (aScTy) {
    case scConstant: {
      oDelta = GetConstValue(*A) - GetConstValue(*B);
      return true;
    }

  // look through casts
    case scTruncate:
    case scZeroExtend:
    case scSignExtend: {
      auto * aCast = cast<SCEVCastExpr>(A);
      auto * bCast = cast<SCEVCastExpr>(B);
      return getConstantDiff(aCast->getOperand(), bCast->getOperand(), oDelta);
    }

  // transparently pass delta through adds
    case scAddExpr: {
      auto * aAdd = cast<SCEVAddExpr>(A);
      auto * bAdd = cast<SCEVAddExpr>(B);

      if (aAdd->getNumOperands() != bAdd->getNumOperands()) return false;

      // sum up constant differences of operands
      int64_t sum = 0;
      for (size_t i = 0; i < aAdd->getNumOperands(); ++i) {
        int64_t opDiff;
        if (!getConstantDiff(aAdd->getOperand(i), bAdd->getOperand(i), opDiff)) {
            return false;
        }
        sum += opDiff;
      }
      oDelta = sum;
      return true;
    }

  // match multiply by constants
    case scMulExpr: {
      auto * aMul = cast<SCEVMulExpr>(A);
      auto * bMul = cast<SCEVMulExpr>(B);
      auto *aConst = dyn_cast<SCEVConstant>(aMul->getOperand(0));
      auto *bConst = dyn_cast<SCEVConstant>(bMul->getOperand(0));
      if (!aConst || !bConst) return false;
      if (aConst != bConst) return false;

      int64_t mulDiff;
      if (!getConstantDiff(aMul->getOperand(1), bMul->getOperand(1), mulDiff)) {
        IF_DEBUG_MG errs() << "mul, rhs not const!\n";
        return false;
      }

      oDelta = GetConstValue(*aConst) * mulDiff;
      return true;
    }

    case scUDivExpr:
    case scAddRecExpr:
    case scUMaxExpr:
    case scSMaxExpr:
    case scUnknown:
    case scCouldNotCompute:
      return false;

    default:
      abort(); // unrecognized SCEVType
  }

  // Otw, start decomposing the SCEVs
  // return zeroScev; // identical
  return false; // could not diff SCEVs
}

const MemoryGroup & MemoryAccessGrouper::getMemoryGroup(const SCEV *scev) {
  static MemoryGroup emptyGroup;

  for (MemoryGroup &group : memoryGroups) {
    auto findIt = std::find(group.begin(), group.end(), scev);
    if (findIt != group.end())
      return group;
  }
  return emptyGroup;
}

// MemoryAccessGrouper_END

// InstructionGroup_BEGIN

InstructionGroup::InstructionGroup(Instruction *element) :
  isStoreGroup(isa<StoreInst>(element)),
  groupType(isStoreGroup ? cast<StoreInst>(element)->getValueOperand()->getType()
                         : cast<LoadInst>(element)->getType()),
  elements(1, element),
  passedMemoryAndCallInstructions(0) {}

InstructionGroup::InstructionGroup() :
  isStoreGroup(false),
  groupType(0),
  elements(0),
  passedMemoryAndCallInstructions(0) {}

bool InstructionGroup::insert(Instruction *element, MemoryDependenceResults &MDR) {
  const LoadInst *load = dyn_cast<LoadInst>(element);
  const CallInst *call = dyn_cast<CallInst>(element);
  if ((load && isStoreGroup) || (!load && !isStoreGroup) || call) {
    passedMemoryAndCallInstructions.push_back(element);
    return false;
  }

  Type *elementType = isStoreGroup ? cast<StoreInst>(element)->getValueOperand()->getType()
                                   : cast<LoadInst>(element)->getType();
  if (groupType != elementType)
    return false;

  Instruction *dependentInst = MDR.getDependency(element).getInst();
  auto findIt = std::find(passedMemoryAndCallInstructions.begin(), passedMemoryAndCallInstructions.end(), dependentInst);
  if (findIt != passedMemoryAndCallInstructions.end())
    return false;

  elements.push_back(element);
  return true;
}

std::vector<llvm::Instruction *>::iterator InstructionGroup::begin() {
  return elements.begin();
}

std::vector<llvm::Instruction *>::iterator InstructionGroup::end() {
  return elements.end();
}

// InstructionGroup_END

// InstructionGrouper_BEGIN

void InstructionGrouper::add(Instruction *instr, MemoryDependenceResults &MDR) {
  // try to find existing group
  for (InstructionGroup &instrGroup : instructionGroups) {
    if (instrGroup.insert(instr, MDR))
      return;
  }

  if (isa<CallInst>(instr))
    return;

  // new group
  InstructionGroup freshGroup(instr);
  instructionGroups.push_back(freshGroup);
}

void InstructionGrouper::clearAll() {
  instructionGroups = std::vector<InstructionGroup>();
}

bool InstructionGrouper::empty() {
  return instructionGroups.empty();
}

const InstructionGroup & InstructionGrouper::getInstructionGroup(llvm::Instruction *instr) {
  static InstructionGroup emptyGroup;

  for (InstructionGroup &group: instructionGroups) {
    auto findIt = std::find(group.begin(), group.end(), instr);
    if (findIt != group.end())
      return group;
  }
  return emptyGroup;
}

// InstructionGrouper_END
