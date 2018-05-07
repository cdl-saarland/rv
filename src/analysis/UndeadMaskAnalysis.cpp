#include "rv/analysis/UndeadMaskAnalysis.h"

#include <llvm/IR/CFG.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Constants.h>
#include "rv/region/Region.h"
#include "rv/vectorizationInfo.h"

#include "rv/intrinsics.h"
#include "rvConfig.h"

using namespace llvm;

#if 1
#define IF_DEBUG_UDM IF_DEBUG
#else
#define IF_DEBUG_UDM if (true)
#endif

namespace rv {

static bool
IsConstMask(const Value & val, bool allTrue) {
  if (!isa<ConstantInt>(val)) return false;
  return allTrue == (cast<ConstantInt>(val).getZExtValue() != 0);
}

static Value*
MatchNegation(const Value & val) {
  const auto * inst = dyn_cast<Instruction>(&val);
  if (!inst) return nullptr;
  if (inst->getOpcode() != Instruction::Xor) return nullptr;

  if (IsConstMask(*inst->getOperand(0), true)) return inst->getOperand(1); // true XOR V
  if (IsConstMask(*inst->getOperand(1), true)) return inst->getOperand(0); // V XOR true

  return nullptr;
}

bool
UndeadMaskAnalysis::implies(const Value & lhs, bool lhsNegated, const Value & rhs, bool rhsNegated) {
  IF_DEBUG_UDM { errs() << "UDM: implies " << lhs << ", lhsNegated=" << lhsNegated << ", rhs " << rhs << ", rhsNegated=" << rhsNegated << "\n"; }

// trivial cases
  if ((lhsNegated == rhsNegated) && (&lhs == &rhs)) {
    IF_DEBUG_UDM { errs() << " yes! A => A\n"; }
    return true;
  }
  if (IsConstMask(rhs, !rhsNegated)) return true; // <everything> => true
  if (IsConstMask(lhs, lhsNegated)) return true; // false => <everything> OR !true => <everything>

// unwind negations in @lhs and @rhs
  const Value * negatedLhs = MatchNegation(lhs);
  const Value * negatedRhs = MatchNegation(rhs);
  if (negatedLhs || negatedRhs) {
    return implies(negatedLhs ? *negatedLhs : lhs, ((bool) negatedLhs) ^ lhsNegated,
                   negatedRhs ? *negatedRhs : rhs, ((bool) negatedRhs) ^ rhsNegated);
  }

// mask predicate
  auto maskIntrinsicID = GetIntrinsicID(lhs);
  if (maskIntrinsicID == RVIntrinsic::Unknown) {
    return false;
  }

  const auto * maskArg = cast<const CallInst>(lhs).getArgOperand(0);

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

UndeadMaskAnalysis::UndeadMaskAnalysis(const DominatorTree & _domTree, VectorizationInfo & _vecInfo)
: domTree(_domTree)
, vecInfo(_vecInfo)
{}

const BasicBlock*
GetUniquePredecessor(const BasicBlock & block, int & oSuccIdx) {
  // find a unique predecessor
  const BasicBlock * uniquePred = nullptr;
  const_pred_iterator itPred, itEnd;
  int opIdx = 0;
  for (itPred = pred_begin(&block), itEnd = pred_end(&block);
      itPred != itEnd;
      ++itPred) {
    if (uniquePred) return nullptr;
    uniquePred = *itPred;
    opIdx = itPred.getUse().getOperandNo();
  }

  if (!uniquePred) return nullptr;

  const auto * uniqueBranch = dyn_cast<BranchInst>(uniquePred->getTerminator());
  if (!uniqueBranch) return nullptr; // FIXME

  // decode successor idx from operand idx
  if (uniqueBranch->isConditional()) oSuccIdx = opIdx - 1;
  else {
    assert(opIdx == 0);
    oSuccIdx = 0;
  }

  return uniquePred;
}


bool
UndeadMaskAnalysis::isUndead(const Value & mask, const BasicBlock & where) {
  IF_DEBUG_UDM { where.getParent()->dump(); }

  // use cached result (where available_
  auto it = liveDominatorMap.find(&mask);
  if (it != liveDominatorMap.end()) {
    const auto * liveDomBlock = it->second;
    if (liveDomBlock) return domTree.dominates(liveDomBlock, &where);
    else return false; // mask has no live dominator
  }

  IF_DEBUG_UDM { errs() << "UDM: query for " << mask.getName() << " at block " << where.getName() << "\n"; }

  // descend down the dominator tree to find a dominating known-unead branch condition that implies @mask
  auto * domNode = domTree.getNode(const_cast<BasicBlock*>(&where));
  assert(domNode);

  while (domNode) {
    const auto * block = domNode->getBlock();
    if (!vecInfo.getRegion()->contains(block)) return false; // TODO query the entry predicate

  // whether the unique predecessor of this block has an edge predicate that implies that at least one lane is live in the mask predicate
    int succIdx;
    const auto * predBlock = GetUniquePredecessor(*block, succIdx);
    if (predBlock) {

      IF_DEBUG_UDM { errs() << "UDM:\t has unique pred " << predBlock->getName() << " at idx " << succIdx << "\n"; }
      auto * predTerm = predBlock->getTerminator();
      auto * predBranch = dyn_cast<BranchInst>(predTerm);
      if (predBranch && predBranch->isConditional()) {
        const auto * predCond = predBranch->getCondition();

        // check that the predicate of the controlling branch ia undead
        auto * predMask = vecInfo.getPredicate(*predBlock);
        IF_DEBUG_UDM { if (predMask) errs() << "Checking that the pred mask is undead " << *predMask << "\n"; }
        if (predMask && !IsConstMask(*predMask, true) && !isUndead(*predMask, *predBlock)) {
          liveDominatorMap[&mask] = nullptr;
          return false;
        }

        // whether the branch predicate implies that at least one lane in @mask is live
        if (implies(*predCond, succIdx == 2, mask, false)) {
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

} // namespace rv
