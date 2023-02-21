//===- src/analysis/UndeadMaskAnalysis.cpp - at-least-one-thread-live analysis\
//--*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "rv/analysis/UndeadMaskAnalysis.h"

#include "rv/region/Region.h"
#include "rv/rvDebug.h"
#include "rv/vectorizationInfo.h"
#include <llvm/IR/CFG.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/PatternMatch.h>

#include "rv/intrinsics.h"
#include "rvConfig.h"

using namespace llvm;

#if 1
#define IF_DEBUG_UDM IF_DEBUG
#else
#define IF_DEBUG_UDM if (true)
#endif

using namespace llvm::PatternMatch;

namespace rv {

static bool IsConstMask(const Value &val, bool allTrue) {
  if (!isa<ConstantInt>(val))
    return false;
  return allTrue == (cast<ConstantInt>(val).getZExtValue() != 0);
}

static Value *MatchNegation(const Value &val) {
  Value *Elem = nullptr;
  if (match(&val, m_Not(m_Value(Elem)))) {
    return Elem;
  }
  return nullptr;
}

bool UndeadMaskAnalysis::implies(const Value &lhs, bool lhsNegated,
                                 const Value &rhs, bool rhsNegated) {
  IF_DEBUG_UDM {
    errs() << "UDM: whether " << lhs << ", lhsNegated=" << lhsNegated
           << " == implies ==> rhs " << rhs << ", rhsNegated=" << rhsNegated
           << "\n";
  }

  // trivial cases
  if ((lhsNegated == rhsNegated) && (&lhs == &rhs)) {
    IF_DEBUG_UDM { errs() << " yes! A => A\n"; }
    return true;
  }
  if (IsConstMask(rhs, !rhsNegated))
    return true; // <everything> => true
  if (IsConstMask(lhs, lhsNegated))
    return true; // false => <everything> OR !true => <everything>

  // unwind negations in @lhs and @rhs
  const Value *negatedLhs = MatchNegation(lhs);
  const Value *negatedRhs = MatchNegation(rhs);
  if (negatedLhs || negatedRhs) {
    return implies(
        negatedLhs ? *negatedLhs : lhs, ((bool)negatedLhs) ^ lhsNegated,
        negatedRhs ? *negatedRhs : rhs, ((bool)negatedRhs) ^ rhsNegated);
  }

  // see through LHS conjunctions
  Value *A, *B;
  if ((!lhsNegated && match(&lhs, m_And(m_Value(A), m_Value(B)))) ||
      (lhsNegated && match(&lhs, m_Or(m_Value(A), m_Value(B))))) {
    IF_DEBUG_UDM { errs() << "\tlhs conjunction case\n"; }
    return implies(*A, lhsNegated, rhs, rhsNegated) ||
           implies(*B, lhsNegated, rhs, rhsNegated);
  }

  // see through RHS disjunctions
  if ((!rhsNegated && match(&rhs, m_Or(m_Value(A), m_Value(B)))) ||
      (rhsNegated && match(&rhs, m_And(m_Value(A), m_Value(B))))) {
    IF_DEBUG_UDM { errs() << "\trhs disjunction case\n"; }
    return implies(lhs, lhsNegated, *A, rhsNegated) ||
           implies(rhs, lhsNegated, *B, rhsNegated);
  }

  // mask predicate
  auto maskIntrinsicID = GetIntrinsicID(lhs);
  if (maskIntrinsicID == RVIntrinsic::Unknown) {
    return false;
  }

  const auto *maskArg = cast<const CallInst>(lhs).getArgOperand(0);

  switch (maskIntrinsicID) {
  case RVIntrinsic::Any:
  case RVIntrinsic::All: {
    return implies(*maskArg, lhsNegated, rhs, rhsNegated);
  }

  default:
    return false;
  }

  return false;
}

UndeadMaskAnalysis::UndeadMaskAnalysis(VectorizationInfo &VecInfo,
                                       FunctionAnalysisManager &FAM)
    : vecInfo(VecInfo), domTree(FAM.getResult<DominatorTreeAnalysis>(
                            vecInfo.getScalarFunction())) {}

const BasicBlock *GetUniquePredecessor(const BasicBlock &block) {
  // find a unique predecessor
  const BasicBlock *uniquePred = nullptr;
  const_pred_iterator itPred, itEnd;
  for (itPred = pred_begin(&block), itEnd = pred_end(&block); itPred != itEnd;
       ++itPred) {
    if (uniquePred)
      return nullptr;
    uniquePred = *itPred;
  }

  if (!uniquePred)
    return nullptr;

  const auto *uniqueBranch = dyn_cast<BranchInst>(uniquePred->getTerminator());
  if (!uniqueBranch)
    return nullptr; // FIXME

  return uniquePred;
}

static bool IsTargetOnFalse(const BranchInst &branch, const BasicBlock &Dest) {
  assert(branch.isConditional());
  return branch.getSuccessor(1) == &Dest;
}

bool UndeadMaskAnalysis::isUndead(const Value &mask, const BasicBlock &where) {
  // IF_DEBUG_UDM { DumpValue(*where.getParent()); }

  // use cached result (where available_
  auto it = liveDominatorMap.find(&mask);
  if (it != liveDominatorMap.end()) {
    const auto *liveDomBlock = it->second;
    if (liveDomBlock)
      return domTree.dominates(liveDomBlock, &where);
    else
      return false; // mask has no live dominator
  }

  IF_DEBUG_UDM {
    errs() << "UDM: query for " << mask.getName() << " at block "
           << where.getName() << "\n";
  }

  // descend down the dominator tree to find a dominating known-unead branch
  // condition that implies @mask
  auto *domNode = domTree.getNode(const_cast<BasicBlock *>(&where));
  assert(domNode);

  while (domNode) {
    const auto *block = domNode->getBlock();
    if (!vecInfo.getRegion().contains(block))
      return false; // TODO query the entry predicate

    // whether the unique predecessor of this block has an edge predicate that
    // implies that at least one lane is live in the mask predicate
    const auto *predBlock = GetUniquePredecessor(*block);
    if (predBlock) {

      IF_DEBUG_UDM {
        errs() << "UDM:\t has unique pred " << predBlock->getName() << ".\n";
      }
      auto *predTerm = predBlock->getTerminator();
      auto *predBranch = dyn_cast<BranchInst>(predTerm);
      if (predBranch && predBranch->isConditional()) {
        const auto *predCond = predBranch->getCondition();

        // check that the predicate of the controlling branch ia undead
        auto *predMask = vecInfo.getPredicate(*predBlock);
        IF_DEBUG_UDM {
          if (predMask)
            errs() << "Checking that the pred mask is undead " << *predMask
                   << "\n";
        }
        if (predMask && !IsConstMask(*predMask, true) &&
            !isUndead(*predMask, *predBlock)) {
          liveDominatorMap[&mask] = nullptr;
          return false;
        }

        // whether the branch predicate implies that at least one lane in @mask
        // is live
        if (implies(*predCond, IsTargetOnFalse(*predBranch, *block), mask,
                    false)) {
          liveDominatorMap[&mask] = block;
          return true;
        }
      }
    }

    // Otw, check if any dominator has this property
    domNode = domNode->getIDom();
  }

  liveDominatorMap[&mask] = nullptr;
  return false;
}

void UndeadMaskAnalysis::print(raw_ostream &Out) {
  Out << "UDM {\n";
  vecInfo.getRegion().for_blocks_rpo([&](const BasicBlock &BB) {
    auto *Mask = vecInfo.getPredicate(BB);

    if (Mask && !isUndead(*Mask, BB))
      return true;
    Out << BB.getName().str() << ":  undead\n";
    return true;
  });
  Out << "}\n";
}

} // namespace rv
