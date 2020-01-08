//===- src/transform/bosccTransform.cpp - BOSCC-by-CFG-gadget --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#include "rv/transform/bosccTransform.h"
#include "rv/analysis/BranchEstimate.h"

#include <vector>
#include <sstream>

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
#include <llvm/ADT/PostOrderIterator.h>

#include <llvm/Analysis/PostDominators.h>
#include <llvm/Transforms/Utils/ValueMapper.h>

#include "rv/vectorizationInfo.h"
#include "rv/PlatformInfo.h"
#include "rv/transform/maskExpander.h"

#include "rv/rvDebug.h"
#include <rvConfig.h>
#include "report.h"

using namespace llvm;
using namespace rv;

#if 1
#define IF_DEBUG_BOSCC IF_DEBUG
#else
#define IF_DEBUG_BOSCC if (true)
#endif

using BlockSet = std::set<const BasicBlock*>;
using PHIMap = std::map<const PHINode*, PHINode*>;
using BlockMap = std::map<const BasicBlock*, BasicBlock*>;

struct Impl {
  VectorizationInfo & vecInfo;
  PlatformInfo & platInfo;
  MaskExpander & maskEx;
  DominatorTree & domTree;
  PostDominatorTree & postDomTree;
  LoopInfo & loopInfo;
  Module & mod;
  BranchProbabilityInfo *pbInfo;

  BranchEstimate BranchEst;

  // BOSCC region exit blocks (containing merge phis)
  BlockSet bosccExitBlocks;


Impl(VectorizationInfo & _vecInfo, PlatformInfo & _platInfo,  MaskExpander & _maskEx, DominatorTree & _domTree, PostDominatorTree & _postDomTree, LoopInfo & _loopInfo, BranchProbabilityInfo * _pbInfo)
: vecInfo(_vecInfo)
, platInfo(_platInfo)
, maskEx(_maskEx)
, domTree(_domTree)
, postDomTree(_postDomTree)
, loopInfo(_loopInfo)
, mod(*vecInfo.getScalarFunction().getParent())
, pbInfo(_pbInfo)
, BranchEst(vecInfo, platInfo, maskEx, domTree, loopInfo, pbInfo)
, bosccExitBlocks()
{}


// mergePhis: map original phis to BOSCC region exit merge phis
// mergeBlocks: map blocks containing original phis to BOSCC exit blocks
// bosccEntry: entryBlock to the bosccRegion (invariant)
// block: current block whose exit edges are under inspection
// seenBlocks: already inspected blocks
void
rec_createMergeBlock(BasicBlock & bosccEntry, BasicBlock & block, PHIMap & mergePhis, BlockMap & mergeBlocks, BlockSet & seenBlocks) {
  if (!seenBlocks.insert(&block).second) return; // already went down this path

  auto & term = *block.getTerminator();
  for (size_t i = 0; i < term.getNumSuccessors(); ++i) {
    auto & succ = *term.getSuccessor(i);

    if (!domTree.dominates(&bosccEntry, &succ)) {
      IF_DEBUG_BOSCC errs() << "BOSCC EXIT " << bosccEntry.getName() << "  to  " << succ.getName() << "\n";
      // we reached a merge path (boscced control region merges with non-boscced control)
      for (auto & inst : succ) {
        auto * phi = dyn_cast<PHINode>(&inst);
        if (!phi) break;

        // request a merge phis node (in a dedicated BOSCC exit block)
        auto itPhi = mergePhis.find(phi);
        PHINode * mergePhi = nullptr;
        bool newMergeBlock = false;
        BasicBlock * mergeBlock = nullptr;
        if (itPhi != mergePhis.end()) {
          mergePhi = itPhi->second;
          mergeBlock = mergePhi->getParent();
        } else {
          // is there already a merge block?
          auto itBlock = mergeBlocks.find(&succ);
          if (itBlock != mergeBlocks.end()) {
            mergeBlock = itBlock->second;
          } else {
            newMergeBlock = true;
            mergeBlock = BasicBlock::Create(block.getContext(), bosccEntry.getName() + ".bsc_exit", block.getParent(), &succ);
            bosccExitBlocks.insert(mergeBlock);
            mergeBlocks[&succ] = mergeBlock;

            auto * loop = loopInfo.getLoopFor(&succ);
            if (loop) {
              loop->addBasicBlockToLoop(mergeBlock, loopInfo);
              // TODO domTree, postDomTree
            }

            // embed merge block
            auto & brInst = *BranchInst::Create(&succ, mergeBlock);
            vecInfo.setVectorShape(brInst, VectorShape::uni());
          }

          // create a new merge phi
          mergePhi = PHINode::Create(phi->getType(), 4, phi->getName() + ".bsc_merge", &*mergeBlock->getFirstInsertionPt());
          vecInfo.setVectorShape(*mergePhi, vecInfo.getVectorShape(*phi)); // TODO this is an overapproximation
          phi->addIncoming(mergePhi, mergeBlock);
          mergePhis[phi] = mergePhi;
        }

        // rereoute incoming value through merge phi
        int idx = phi->getBasicBlockIndex(&block);
        auto * inVal = phi->getIncomingValue(idx);

        mergePhi->addIncoming(inVal, &block);
        term.setSuccessor(i, mergePhi->getParent());
        phi->removeIncomingValue(idx, false);

        if (newMergeBlock) {
          domTree.addNewBlock(mergeBlock, &block);

        } else {
          // join in new reaching edge from @entry to @medgeBlock
          auto * mergeNode = domTree.getNode(mergeBlock);
          auto * mergeIDomNode = mergeNode->getIDom();
          BasicBlock * oldDom = nullptr;
          if (mergeIDomNode) {
            oldDom = mergeIDomNode->getBlock();
          } else {
            oldDom = &block;
          }

          auto * newIDom = domTree.findNearestCommonDominator(oldDom, &block);
          domTree.changeImmediateDominator(mergeNode, domTree.getNode(newIDom));
        }
      }
    } else {
      IF_DEBUG_BOSCC errs() << "BOSCC block " << block.getName() << "  below entry  " << bosccEntry.getName() << "\n";
      // still inside the region -> descend
      rec_createMergeBlock(bosccEntry, succ, mergePhis, mergeBlocks, seenBlocks);
    }
  }
}

void
createMergeBlock(BranchInst & branch, int succIdx) {
  auto & bosccEntry = *branch.getSuccessor(succIdx);
  PHIMap mergePhis;
  BlockMap mergeBlocks;
  BlockSet seenBlocks;
  rec_createMergeBlock(bosccEntry, bosccEntry, mergePhis, mergeBlocks, seenBlocks);

  // simplify merge phis
  for (auto itPhi : mergePhis) {
    auto * phi = itPhi.second;
    if (phi->getNumIncomingValues() == 1) {
      vecInfo.dropVectorShape(*phi);
      phi->replaceAllUsesWith(phi->getIncomingValue(0));
      phi->eraseFromParent();
    }
  }
  mergePhis.clear();
}


void
transformBranch(BranchInst & branch, int succIdx) {
  auto & context = branch.getContext();
  assert(0 <= succIdx && succIdx <= 1);
  assert(branch.isConditional());
  assert(!vecInfo.getVectorShape(branch).isUniform());

  auto * succBlock = branch.getSuccessor(succIdx);
  auto * exitBlock = branch.getSuccessor(1 - succIdx);
  auto * branchCond = branch.getCondition();
  bool branchOnTrue = succIdx == 0;


  assert(domTree.dominates(branch.getParent(), succBlock) && "can only BOSCC over dominated parts for now");

  auto it = branch.getParent()->getIterator();
  it++;
  auto * insertBlock = &*it;
// create a BOSCC condition block
  std::stringstream blockName;
  blockName << succBlock->getName().str() << "_boscc";
  auto * bosccBlock = BasicBlock::Create(context, blockName.str(), &vecInfo.getScalarFunction(), insertBlock);

// embed block in loopInfo
  auto * loop = loopInfo.getLoopFor(branch.getParent());
  if (loop) loop->addBasicBlockToLoop(bosccBlock, loopInfo);

  // register with domTree
  auto * bosccNode = domTree.addNewBlock(bosccBlock, branch.getParent());
  domTree.changeImmediateDominator(domTree.getNode(succBlock), bosccNode);

#if 0
  // bosccBlock has same predicate as BOSCC successor
  vecInfo.setPredicate(*bosccBlock, *vecInfo.getPredicate(*succBlock));
#endif

  IRBuilder<> builder(bosccBlock);

// flip the condition (if need be)
  auto * bosccMask = branchCond;
  if (!branchOnTrue) {
    bosccMask = builder.CreateNot(branchCond, "neg");
    vecInfo.setVectorShape(*bosccMask, vecInfo.getVectorShape(*branchCond));
  }

// create BOSCC condition check
  auto & anyFunc = platInfo.requestRVIntrinsicFunc(RVIntrinsic::Any);
  auto * bosccCond = builder.CreateCall(&anyFunc, bosccMask, "boscc_test");
  vecInfo.setVectorShape(*bosccCond, VectorShape::uni());

// create the BOSCC branch
  auto * bosccBr = builder.CreateCondBr(bosccCond, succBlock, exitBlock);
  vecInfo.setVectorShape(*bosccBr, VectorShape::uni());

// link bosccBranch into old branch
   branch.setSuccessor(succIdx, bosccBlock);

#if 0
// copy edge predicates
  // bosccBlock -> bosccRegion (same pred)
  auto * edgeTakenMask = maskEx.getEdgeMask(branch, succIdx);
  if (edgeTakenMask) {
    maskEx.setEdgeMask(*bosccBlock, 0, *edgeTakenMask);
  }

  // bosccBlock -> exit (same as old exit branch)
  auto * edgeSkippedMask = maskEx.getEdgeMask(branch,  1 - succIdx);
  if (edgeSkippedMask) {
    maskEx.setEdgeMask(*bosccBlock, 1, *edgeSkippedMask);
  }
#endif

// patch phis in succBlock
  for (auto & inst : *succBlock) {
    auto * phi =  dyn_cast<PHINode>(&inst);
    if (!phi) break;
    int branchBlockIdx = phi->getBasicBlockIndex(branch.getParent());
    phi->setIncomingBlock(branchBlockIdx, bosccBlock);
  }

// fix up PHI nodes in the exit block (we have a new incoming branch from the bosccBlock)
  for (auto & inst : *exitBlock) {
    auto * phi =  dyn_cast<PHINode>(&inst);
    if (!phi) break;
    auto * origExitVal = phi->getIncomingValueForBlock(branch.getParent());
    phi->addIncoming(origExitVal, bosccBlock);
  }
}

// 0  : do not TransformBranch
// -1 : TransformBranch onTrue
// 1 : TransformBranch onFalse
// currently only cope with BOSCC and CIF
int
PickSuccessorForBoscc(BranchInst & branch) {
  IF_DEBUG_BOSCC {
     errs () << "BOSCC: inspecting " << branch << "\n";
  }

  //run legality checks
  bool onTrueLegal, onFalseLegal;
  if (!BranchEst.CheckLegality(branch, onTrueLegal, onFalseLegal)) return 0;

  IF_DEBUG_BOSCC {
    errs () << "BOSCC: onTrueLegal to " << branch.getSuccessor(0)->getName() << " = " << onTrueLegal << "\n";
    errs () << "BOSCC: onFalseLegal to " << branch.getSuccessor(1)->getName() << " = " << onFalseLegal << "\n";
  }

  double trueRatio = 0.0;
  double falseRatio = 0.0;
  size_t onTrueScore = 0;
  size_t onFalseScore = 0;

  BranchEst.analyze(branch, trueRatio, falseRatio, onTrueScore, onFalseScore);

  const char * Ratio_T = "BOSCC_T";
  const char * Score_LIMIT = "BOSCC_LIMIT";

  const double maxRatio = GetValue<double>(Ratio_T, 0.40);
  const size_t minScore = GetValue<size_t>(Score_LIMIT, 100);

  IF_DEBUG_BOSCC { errs() << "BOSCC_T=" << maxRatio << ", BOSCC_LIMIT=" << minScore << "\n"; }

  IF_DEBUG_BOSCC {
    errs () << "BOSCC: onTrue Score/Ratio to " << branch.getSuccessor(0)->getName() << " = " << onTrueScore << " / " << trueRatio << "\n";
    errs () << "BOSCC: onFalse Legal/Ratio to " << branch.getSuccessor(1)->getName() << " = " << onFalseScore << " / " << falseRatio << "\n";
  }

  bool onTrueBeneficial = onTrueScore >= minScore && trueRatio < maxRatio;
  bool onFalseBeneficial = onFalseScore >= minScore && falseRatio < maxRatio;

  bool couldTransFalse = onFalseBeneficial && onFalseLegal;
  bool couldTransTrue = onTrueBeneficial && onTrueLegal;

  // otw try to skip the bigger dominated part
  // TODO could also give precedence by region size
  if (couldTransTrue && (!couldTransFalse || onTrueScore > onFalseScore)) {
    return -1;
  } else if (couldTransFalse) {
    return 1;
  }

  // can not distinguish --> don't TransformBranch
  // this holds e.g. if the branch does not dominate any of its successors
  return 0;
}


bool
run() {
  domTree.recalculate(vecInfo.getScalarFunction());

  size_t numBosccBranches = 0;

  ReversePostOrderTraversal<Function*> RPOT(&vecInfo.getScalarFunction());

  for (auto * BB : RPOT) {
    if (!vecInfo.inRegion(*BB)) continue;
    auto * term = BB->getTerminator();
    auto * branchInst = dyn_cast<BranchInst>(term);

    // consider divergent conditional branches
    if (!branchInst) continue;
    if (!branchInst->isConditional()) continue;
    if (vecInfo.getVectorShape(*branchInst).isUniform()) continue;

    // do not speculate across BOSCC exits
    if (bosccExitBlocks.count(branchInst->getSuccessor(0)) || bosccExitBlocks.count(branchInst->getSuccessor(1))) return 0;
    int score = PickSuccessorForBoscc(*branchInst);
    if (score == 0) continue;
    int succIdx = score < 0 ? 0 : 1;

    ++numBosccBranches;

    Report() << "boscc: skip succ " << branchInst->getSuccessor(succIdx)->getName() << " of block " << branchInst->getParent()->getName() << "\n";

    // pull out all incoming values in the going-to-be-BOSCCed region into their own phi nodes in a dedicated block (if the boscc branch is taken these blens will be skipped)
    createMergeBlock(*branchInst, succIdx);

    transformBranch(*branchInst, succIdx);
  }

  if (numBosccBranches > 0) Report() << "boscc: inserted " << numBosccBranches << " BOSCC branches\n";

  // recover
  postDomTree.recalculate(vecInfo.getScalarFunction());

  domTree.verify();

  IF_DEBUG_BOSCC {
    errs() << "--- FUNCTION AFTER BOSCC ---:\n";
    Dump(vecInfo.getScalarFunction());
  }

  return false;
}

};


bool
BOSCCTransform::run() {
  Impl impl(vecInfo, platInfo, maskEx, domTree, postDomTree, loopInfo, pbInfo);
  return impl.run();
}


BOSCCTransform::BOSCCTransform(VectorizationInfo & _vecInfo, PlatformInfo & _platInfo, MaskExpander & _maskEx, FunctionAnalysisManager &FAM)
: vecInfo(_vecInfo)
, platInfo(_platInfo)
, maskEx(_maskEx)
, domTree(FAM.getResult<DominatorTreeAnalysis>(vecInfo.getScalarFunction()))
, postDomTree(FAM.getResult<PostDominatorTreeAnalysis>(vecInfo.getScalarFunction()))
, loopInfo(*FAM.getCachedResult<LoopAnalysis>(vecInfo.getScalarFunction()))
, pbInfo(&FAM.getResult<BranchProbabilityAnalysis>(vecInfo.getScalarFunction()))
{}
