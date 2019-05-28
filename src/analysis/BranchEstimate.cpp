
#include <vector>

#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Dominators.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Analysis/BranchProbabilityInfo.h>
#include <llvm/Support/Format.h>

#include <llvm/ADT/SmallSet.h>

#include <llvm/Transforms/Utils/ValueMapper.h>

#include "rv/vectorizationInfo.h"
#include "rv/PlatformInfo.h"
#include "rv/shape/vectorShape.h"

#include "rv/rvDebug.h"
#include "rvConfig.h"
#include "rv/analysis/BranchEstimate.h"


#if 1
#define IF_DEBUG_DISP IF_DEBUG
#else
#define IF_DEBUG_DISP if (true)
#endif

using namespace llvm;

namespace rv {

size_t
BranchEstimate::getBlockScore(BasicBlock & entry) {
  size_t score = 0;

  for (auto & inst : entry) {
    auto * store = dyn_cast<StoreInst>(&inst);
    auto * load = dyn_cast<LoadInst>(&inst);
    auto * call = dyn_cast<CallInst>(&inst);

    Value * ptrOperand = nullptr;
    if (load) ptrOperand = load->getPointerOperand();
    else if (store) ptrOperand = store->getPointerOperand();

    if (ptrOperand) {
      if (vecInfo.getVectorShape(*ptrOperand).isVarying()) {
        score += 8;
      } else if (vecInfo.getVectorShape(*ptrOperand).isUniform()) {
        score += 1;
      } else { // strided
        score += 2;
      }
      continue;
    }

    if (call) {
      auto * callee = call->getCalledFunction();
      if (!callee) { score += 8; continue; }

      VectorShapeVec argShapeVec;
      for (const auto & arg : call->arg_operands()) {
        argShapeVec.push_back(vecInfo.getVectorShape(*arg.getUser()));
      }

      bool hasCallPredicate = false;
      if (!platInfo.getResolver(callee->getName(), *callee->getFunctionType(), argShapeVec, vecInfo.getVectorWidth(), hasCallPredicate)) {
        score += 2;
      }
      score += 1;
      continue;
    }

    score += 1;
  }

  return score;
}

size_t
BranchEstimate::getDomRegionValue(BasicBlock & entry, BasicBlock & subBlock, SmallSet<BasicBlock*, 32> & seenBlocks) {
  if (&entry != &subBlock && !domTree.dominates(&entry, &subBlock)) {
    return 0; // not in our dominance region
  }
  if (!seenBlocks.insert(&subBlock).second) return 0; // already factores in

  // compute own score
  size_t score = getBlockScore(subBlock);

  // compute score of other (dominated) reachable blocks in the region
  auto * termInst = subBlock.getTerminator();
  for (size_t i = 0; i < termInst->getNumSuccessors(); ++i) {
    auto & nextBlock = *termInst->getSuccessor(i);
    score += getDomRegionValue(entry, nextBlock, seenBlocks);
  }
  return score;
}

size_t
BranchEstimate::getDomRegionScore(BasicBlock & entry) {
  SmallSet<BasicBlock*, 32> seenBlocks;
  return getDomRegionValue(entry, entry, seenBlocks);
}

double
BranchEstimate::GetEdgeProb(BasicBlock & start, BasicBlock & end) {
  // use BranchProbabilityInfo if available
  if (pbInfo) {
    auto prob = pbInfo->getEdgeProbability(&start, &end);
    if (prob.isZero()) {
      return 0.0;
    } else if (!prob.isUnknown()) {
      return prob.getNumerator() / (double) prob.getDenominator();
    }
  }

  // otw use uniform distribution
  return 1.0 / start.getTerminator()->getNumSuccessors();
}

void
BranchEstimate::computeDispersion(std::map<BasicBlock*,double> & dispMap) {
  std::vector<BasicBlock*> stack;

  // bootstrap with region entry (executed by all ratio == 1.0)
  auto & entry = vecInfo.getEntry();
  dispMap[&entry] = 1.0;
  for (auto * succ : successors(&entry)) {
    if (succ == &entry)  continue;
    if (!vecInfo.inRegion(*succ)) continue;
    stack.push_back(succ);
  }

  IF_DEBUG_DISP errs() << "--- compute dispersion ---\n";

  // disperse down branches
  while (!stack.empty()) {
    auto * block = stack.back();
    stack.pop_back();

    bool hadRatio = dispMap.count(block);
    double oldRatio = dispMap[block]; // initialize to zero

    Loop * blockLoop = loopInfo.getLoopFor(block);
    bool isHeader = blockLoop && blockLoop->getHeader() == block;

    // join incoming fractions
    bool validRatio = true;
    double ratio = 0.0;
    SmallPtrSet<const BasicBlock*, 6> seenPreds;
    for (auto * pred : predecessors(block)) {
      if (!seenPreds.insert(pred).second) continue;
      if (!vecInfo.inRegion(*pred)) {
        continue;
      }
      if (isHeader && blockLoop->contains(pred)) {
        // do not look into latches
        continue;
      }

      if (!dispMap.count(pred)) {
        // missing operand -> do not update ratio yet
        stack.push_back(pred);
        validRatio = false;
      }
      if (!validRatio) continue;

      // keep computing a new result from the blocks predecessors
      double inProb;
      if (vecInfo.getVectorShape(*pred->getTerminator()).isUniform()) {
        // there is no dispersion at uniform branches -> keep predecessor ratio
        // this is an overapproximation that may lead to block ratios >> 1.0
        inProb = dispMap[pred];
      } else {
        // the predecessor disperses control ratios
        inProb = dispMap[pred] * GetEdgeProb(*pred, *block);
      }
      ratio += inProb;
    }

    // the ratio can exceed 1.0 if we have multiple reaching uniform paths
    ratio = std::min<double>(ratio, 1.0); // cap at 1.0

    // process operands first
    if (!validRatio) continue;

    auto & term = *block->getTerminator();
    if (!hadRatio || (std::fabs(oldRatio - ratio) > 0.000001)) {
      IF_DEBUG_DISP errs() << block->getName() << "  old " << oldRatio << "  new " << ratio << "\n";

      // set new ratio
      dispMap[block] = ratio;

      // push all successors
      for (size_t i = 0; i < term.getNumSuccessors(); ++i) {
        auto * succ = term.getSuccessor(i);
        if (!vecInfo.inRegion(*succ)) continue; // leaving the region -> don't care
        if (blockLoop && succ == blockLoop->getHeader()) continue; // latches not taken
        stack.push_back(succ);
      }
    }
  }
}

bool
BranchEstimate::analyze(BranchInst &branch, double &trueRatio, double &falseRatio, size_t &onTrueScore, size_t &onFalseScore)
{
  // run legality checks
  BasicBlock * onTrueBlock = branch.getSuccessor(0);
  BasicBlock * onFalseBlock = branch.getSuccessor(1);

  if (!vecInfo.inRegion(*onTrueBlock) || !vecInfo.inRegion(*onFalseBlock))
    return false;

  // compute approximate branch probability
  std::map<BasicBlock*,double> dispMap;
  computeDispersion(dispMap);

  assert(dispMap.count(onTrueBlock));
  trueRatio = dispMap.at(onTrueBlock);
  assert(dispMap.count(onFalseBlock));
  falseRatio = dispMap.at(onFalseBlock);

  //compute branch cost
  onTrueScore = getDomRegionScore(*onTrueBlock);
  onFalseScore = getDomRegionScore(*onFalseBlock);

  return true;
}

int
GetNumPredecessors(BasicBlock &block) {
  int numPred = 0;
  for (auto * user : block.users()) {
    auto * inst = dyn_cast<Instruction>(user);
    if (inst && inst->isTerminator()) {
      ++numPred;
    }
  }
  return numPred;
}

}//end of namespace rv
