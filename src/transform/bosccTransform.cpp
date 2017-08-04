#include "rv/transform/bosccTransform.h"

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

#include <rvConfig.h>
#include "report.h"

using namespace rv;
using namespace llvm;

#if 1
#define IF_DEBUG_BOSCC IF_DEBUG
#else
#define IF_DEBUG_BOSCC if (true)
#endif

using RatioMap =  std::map<BasicBlock*,double>;
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
, bosccExitBlocks()
{}

Function &
requestMaskIntrinsic(std::string name) {
  return *platInfo.requestMaskReductionFunc("rv_any");
}


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
  auto & anyFunc = requestMaskIntrinsic("rv_any");
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

size_t
getDomRegionValue(BasicBlock & entry, BasicBlock & subBlock, SmallSet<BasicBlock*, 32> & seenBlocks) {
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
getBlockScore(BasicBlock & entry) {
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

      if (!platInfo.isFunctionVectorizable(callee->getName(), vecInfo.getVectorWidth())) {
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
getDomRegionScore(BasicBlock & entry) {
  SmallSet<BasicBlock*, 32> seenBlocks;
  return getDomRegionValue(entry, entry, seenBlocks);
}

int
GetNumPredecessors(BasicBlock & block) {
  int numPred = 0;
  for (auto * user : block.users()) {
    if (isa<TerminatorInst>(user)) {
      ++numPred;
    }
  }
  return numPred;
}

template<typename N>
static N
GetValue(const char * name, N defVal) {
  auto * text = getenv(name);
  if (!text) return defVal;
  else {
    std::stringstream ss(text);
    N res;
    ss >> res;
    return res;
  }
}
// 0  : do not BOSCC
// -1 : boscc onTrue
// 1 : boscc onFalse
int
bosccHeuristic(BranchInst & branch, double & regProb, size_t & regScore, const RatioMap & dispMap) {
// run legality checks
  BasicBlock * onTrueBlock = branch.getSuccessor(0);
  BasicBlock * onFalseBlock = branch.getSuccessor(1);

  if (!vecInfo.inRegion(*onTrueBlock) || !vecInfo.inRegion(*onFalseBlock)) return 0;

  auto * branchLoop = loopInfo.getLoopFor(branch.getParent());
  auto * onTrueLoop = loopInfo.getLoopFor(onTrueBlock);
  auto * onFalseLoop = loopInfo.getLoopFor(onFalseBlock);

// don't speculate over loop exits for now (TODO)
  if (onTrueLoop != branchLoop || branchLoop != onFalseLoop) return 0;
  if (onTrueLoop && onTrueLoop->getHeader() == onTrueBlock) return 0;
  if (onFalseLoop && onFalseLoop->getHeader() == onTrueBlock) return 0;
  if (maskEx.getBlockMask(*branch.getParent())) return 0; // FIXME this is a workaround transformed divergent loops (we may end up invalidating masks)
  // FIXME in divLoopTrans: use predicate futures where possible

  // do not speculate across BOSCC exits
  if (bosccExitBlocks.count(onTrueBlock) || bosccExitBlocks.count(onFalseBlock)) return 0;

  assert(dispMap.count(onTrueBlock));
  double trueRatio = dispMap.at(onTrueBlock);
  assert(dispMap.count(onFalseBlock));
  double falseRatio = dispMap.at(onFalseBlock);


// per sucess legality
  // legality checks for speculating over onTrue
  bool onTrueLegal = GetNumPredecessors(*onTrueBlock) == 1; //domTree.dominates(branch.getParent(), onTrueBlock);
  onTrueLegal &= !onTrueLoop || onTrueLoop->getLoopLatch() != onTrueBlock;

  // legality checks for speculating over onTrue
  bool onFalseLegal = GetNumPredecessors(*onFalseBlock) == 1; //domTree.dominates(branch.getParent(), onFalseBlock);
  onFalseLegal &= !onFalseLoop || onFalseLoop->getLoopLatch() != onFalseBlock;


// score the dominates parts of either branch target
  // only accept regions that are post dominated by the oposing branch
  size_t onTrueScore = 0;
  if (onTrueLegal) { // && postDomTree.dominates(onFalseBlock, onTrueBlock)) {
    onTrueScore = getDomRegionScore(*onTrueBlock);
  }

  size_t onFalseScore = 0;
  if (onFalseLegal) { //  && postDomTree.dominates(onTrueBlock, onFalseBlock)) {
    onFalseScore = getDomRegionScore(*onFalseBlock);
  }

  const double maxRatio = GetValue<double>("BOSCC_T", 0.14);
  const size_t minScore = GetValue<size_t>("BOSCC_LIMIT", 17);

  IF_DEBUG_BOSCC { errs() << "BOSCC_T " << maxRatio << " BOSCC_LIMIT " << minScore << "\n"; }

  if (falseRatio < 0.06) onFalseLegal = false; // DEBUG HACK
  if (trueRatio < 0.06) onTrueLegal = false; // DEBUG HACK

  IF_DEBUG_BOSCC { errs() << "score (" << onTrueLegal << ") " << onTrueBlock->getName() << "   " << onTrueScore << "\nscore  (" << onFalseLegal << ") " << onFalseBlock->getName() << "   " << onFalseScore << "\n"; }

  bool onTrueBeneficial = onTrueScore >= minScore && trueRatio < maxRatio;
  bool onFalseBeneficial = onFalseScore >= minScore && falseRatio < maxRatio;

  bool couldBosccFalse = onFalseBeneficial && onFalseLegal;
  bool couldBosccTrue = onTrueBeneficial && onTrueLegal;

// otw try to skip the bigger dominated part
  // TODO could also give precedence by region size
  if (couldBosccTrue && (!couldBosccFalse || trueRatio < falseRatio)) {
    regProb = trueRatio;
    regScore = onTrueScore;
    return -1;
  } else if (couldBosccFalse) {
    regProb = falseRatio;
    regScore = onFalseScore;
    return 1;
  }

  // can not distinguish --> don't BOSCC
  // this holds e.g. if the branch does not dominate any of its successors
  return 0;
}

double
GetEdgeProb(BasicBlock & start, BasicBlock & end) {
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

#if 1
#define IF_DEBUG_DISP IF_DEBUG_BOSCC
#else
#define IF_DEBUG_DISP if (true)
#endif
void
computeDispersion(RatioMap & dispMap) {
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
    for (auto * pred : predecessors(block)) {
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
run() {
  domTree.recalculate(vecInfo.getScalarFunction());

  // compute approximate execution ratios
  RatioMap dispMap;
  computeDispersion(dispMap);

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

    double regProb = 0.0;
    size_t regScore = 0;
    int score = bosccHeuristic(*branchInst, regProb, regScore, dispMap);
    if (score == 0) continue;
    int succIdx = score < 0 ? 0 : 1;

    ++numBosccBranches;

    Report() << "boscc: skip succ " << branchInst->getSuccessor(succIdx)->getName() << " of block " << branchInst->getParent()->getName() << "  dispProb: " << format("%1.4f", regProb) << ", score: " << regScore << "\n";

    // pull out all incoming values in the going-to-be-BOSCCed region into their own phi nodes in a dedicated block (if the boscc branch is taken these blens will be skipped)
    createMergeBlock(*branchInst, succIdx);

    transformBranch(*branchInst, succIdx);
  }

  if (numBosccBranches > 0) Report() << "boscc: inserted " << numBosccBranches << " BOSCC branches\n";

  // recover
  postDomTree.recalculate(vecInfo.getScalarFunction());

  domTree.verifyDomTree();

  IF_DEBUG_BOSCC {
    errs() << "--- FUNCTION AFTER BOSCC ---:\n";
    vecInfo.getScalarFunction().dump();
  }

  return false;
}

};


bool
BOSCCTransform::run() {
  Impl impl(vecInfo, platInfo, maskEx, domTree, postDomTree, loopInfo, pbInfo);
  return impl.run();
}


BOSCCTransform::BOSCCTransform(VectorizationInfo & _vecInfo, PlatformInfo & _platInfo, MaskExpander & _maskEx, llvm::DominatorTree & _domTree, llvm::PostDominatorTree & _postDomTree, llvm::LoopInfo & _loopInfo, BranchProbabilityInfo * _pbInfo)
: vecInfo(_vecInfo)
, platInfo(_platInfo)
, maskEx(_maskEx)
, domTree(_domTree)
, postDomTree(_postDomTree)
, loopInfo(_loopInfo)
, pbInfo(_pbInfo)
{}
