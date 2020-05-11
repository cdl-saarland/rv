//===- src/transform/CoherentIfTransform.cpp - Insertiong of all-true code paths --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#include "rv/transform/CoherentIFTransform.h"
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

#include "rv/PlatformInfo.h"
#include "rv/transform/maskExpander.h"
#include "rv/vectorizationInfo.h"

#include "rv/rvDebug.h"
#include <rvConfig.h>
#include "report.h"
#include "utils/llvmDuplication.h"
#include "rv/utils.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/GenericDomTree.h"
#include "llvm/Transforms/Utils/SSAUpdater.h"

using namespace llvm;
using namespace rv;


#if 1
#define IF_DEBUG_CIF IF_DEBUG
#else
#define IF_DEBUG_CIF if (true)
#endif

using Blockset = std::set<const BasicBlock*>;

struct CoherentIF{
  VectorizationInfo & vecInfo;
  PlatformInfo & platInfo;
  MaskExpander & maskEx;
  DominatorTree & domTree;
  PostDominatorTree & postDomTree;
  LoopInfo & loopInfo;
  Module & mod;
  BranchProbabilityInfo *pbInfo;
  BranchEstimate BranchEst;

CoherentIF(VectorizationInfo & _vecInfo, PlatformInfo & _platInfo,  MaskExpander & _maskEx, DominatorTree & _domTree, PostDominatorTree & _postDomTree, LoopInfo & _loopInfo, BranchProbabilityInfo * _pbInfo)
: vecInfo(_vecInfo)
, platInfo(_platInfo)
, maskEx(_maskEx)
, domTree(_domTree)
, postDomTree(_postDomTree)
, loopInfo(_loopInfo)
, mod(*vecInfo.getScalarFunction().getParent())
, pbInfo(_pbInfo)
, BranchEst(vecInfo, platInfo, maskEx, domTree, loopInfo, pbInfo)
{}

void MaintainCloneLoopwithHeader (Loop * clonedParentLoop, Loop & L, ValueToValueMapTy & valueMap) {
  // create a loop object
  auto * clonedHead = &LookUp(valueMap, *L.getHeader());
  auto * clonedLoop = loopInfo.AllocateLoop();

  // embed the loop object in the loop tree
  if (!clonedParentLoop) {
    loopInfo.addTopLevelLoop(clonedLoop);
  } else {
    clonedParentLoop->addChildLoop(clonedLoop);
  }
  // add the header first
  clonedLoop->addBasicBlockToLoop(clonedHead, loopInfo);
}

void CloneBasicBlockForDominatedBBs (BasicBlock *BB, SmallVector<BasicBlock *, 16> DominatedBBs, std::vector<std::pair<BasicBlock *, BasicBlock *>> &CopiedBBPairs, ValueToValueMapTy &cloneMap) {
  auto *inst = BB->getTerminator();

  unsigned int numsuc = inst->getNumSuccessors();
  assert( 1 <= numsuc && numsuc <= 2);

  //For each succeeding block
  for (unsigned int i = 0; i < numsuc; i++) {
    BasicBlock *child = inst->getSuccessor(i);

   //Check if child has already been copied
    auto it = std::find_if(CopiedBBPairs.begin(), CopiedBBPairs.end(), [&child](std::pair<BasicBlock *, BasicBlock *> const & elem) {
      return elem.second == child;
    });

    auto firstit = std::find_if(CopiedBBPairs.begin(), CopiedBBPairs.end(), [&child](std::pair<BasicBlock *, BasicBlock *> const & elem) {
      return elem.first == child;
    });

    if ((std::find(DominatedBBs.begin(), DominatedBBs.end(), child) == DominatedBBs.end()) && (it == CopiedBBPairs.end()))
      continue;

    BasicBlock *clonedchild;

    //Block is already copied, use the copy
    if (it != CopiedBBPairs.end()) {
      clonedchild = (*it).second;
    }
    else if (firstit != CopiedBBPairs.end()) {
      clonedchild = (*firstit).second;
    }
    else {
      //Block still has to be copied
      clonedchild = cloneBlockAndMapInstructions(child, cloneMap);
      cloneMap[child] = clonedchild;

      auto CSI = clonedchild->begin();
      for(auto I = child->begin(); I != child->end(); ++I, CSI++) {
        vecInfo.setVectorShape(*CSI,vecInfo.getVectorShape(*I));
      }

      auto * loop = loopInfo.getLoopFor(child);
      if (loopInfo.isLoopHeader(child)) {
        auto * Parentloop = loop->getParentLoop();
        llvm::Loop* ClonedParentloop = nullptr;
        if (Parentloop) {
          auto* clonedHead = &LookUp(cloneMap, *Parentloop->getHeader());
          ClonedParentloop = loopInfo.getLoopFor(clonedHead);
        }
        MaintainCloneLoopwithHeader(ClonedParentloop, *loop, cloneMap);
      } else if (loop) {
        auto * clonedHead = &LookUp(cloneMap, *loop->getHeader());
        if (clonedHead) {
          auto * Clonedloop = loopInfo.getLoopFor(clonedHead);
          Clonedloop->addBasicBlockToLoop(clonedchild, loopInfo);
        } else {
          loop->addBasicBlockToLoop(clonedchild, loopInfo);
        }
      }

      //Add copied block to the pairs
      CopiedBBPairs.push_back(std::make_pair(child,clonedchild));
      //Recursively copy the blocks successors
      CloneBasicBlockForDominatedBBs(clonedchild, DominatedBBs, CopiedBBPairs, cloneMap);
    }
    //Replace the succeeding block with its copy
    inst->setSuccessor(i,clonedchild);
    inst->replaceUsesOfWith(child,clonedchild);
    for (Instruction &inst : *clonedchild) {
      RemapInstruction(&inst, cloneMap, RF_IgnoreMissingLocals);
    }
  }
}

//Utilizing uttermost the coherent control flow and mimimize the masked instruction numbers
//Transform Warp-Coherent Condition
void
transformCoherentCF (BranchInst & branch, int succIdx) {
  auto & context = branch.getContext();
  assert(0 <= succIdx && succIdx <= 1);
  assert(branch.isConditional());
  assert(!vecInfo.getVectorShape(branch).isUniform());

  BasicBlock *ConBlock = branch.getSuccessor(succIdx);

  if (!domTree.dominates(branch.getParent(), ConBlock)) return;

  //First we split the block containing the branch into 2 blocks
  auto * loop = loopInfo.getLoopFor(branch.getParent());
  BasicBlock *StartBlock = branch.getParent();
  BasicBlock::iterator SplitPt = (BasicBlock::iterator(branch));
  BasicBlock *EndBlock = StartBlock->splitBasicBlock(SplitPt, StartBlock->getName()+".split");//TODO split may be occupied

  //Maintain the loopinfo after split the block
  if (loop) loop->addBasicBlockToLoop(EndBlock, loopInfo);

  //Delete the unconditional branch that was just created by the split, will generate new branch later
  StartBlock->getTerminator()->eraseFromParent();

  auto * branchCond = branch.getCondition();

  //Clone the first potential coherent block
  ValueToValueMapTy cloneMap;
  BasicBlock *clonedConBlock = cloneBlockAndMapInstructions(ConBlock, cloneMap);
  cloneMap[ConBlock] = clonedConBlock;

  auto CSI = clonedConBlock->begin();
  for(auto I = ConBlock->begin(), IE = ConBlock->end(); I != IE; ++I, CSI++) {
    vecInfo.setVectorShape(*CSI,vecInfo.getVectorShape(*I));
  }

  if(loopInfo.isLoopHeader(ConBlock)) {
    MaintainCloneLoopwithHeader(loop, *loopInfo.getLoopFor(ConBlock), cloneMap);
  }
  else
     if (loop) loop->addBasicBlockToLoop(clonedConBlock, loopInfo);

  if (isa<BranchInst>(clonedConBlock->getTerminator()))
    vecInfo.setVectorShape(*clonedConBlock->getTerminator(),vecInfo.getVectorShape(*ConBlock->getTerminator()));

  //Clone the remaining basic blocks and map
  SmallVector<BasicBlock *, 16> DominatedBBs;

  domTree.getDescendants(ConBlock, DominatedBBs);

  std::vector< std::pair< BasicBlock *, BasicBlock *> > CopiedBBPairs;
  CopiedBBPairs.push_back(std::make_pair(ConBlock,clonedConBlock));

  CloneBasicBlockForDominatedBBs(clonedConBlock, DominatedBBs, CopiedBBPairs, cloneMap);

  //Generate the condition check block
  auto * TrueBlock = BasicBlock::Create(context, ConBlock->getName()+ ".wcc", &vecInfo.getScalarFunction(), clonedConBlock);

  // Embede block in loopInfo
  if (loop) loop->addBasicBlockToLoop(TrueBlock, loopInfo);

  IRBuilder<> builder(TrueBlock);

  //Create warp-coherent condition check
  auto * wccMask  = branchCond;
  bool branchOnTrue = succIdx == 0;
  if (!branchOnTrue) {
    wccMask = builder.CreateNot(branchCond, "neg");
    vecInfo.setVectorShape(*wccMask, vecInfo.getVectorShape(*branchCond));
  }

  auto & anyFunc = platInfo.requestRVIntrinsicFunc(RVIntrinsic::All);
  auto * wccCond = builder.CreateCall(&anyFunc, wccMask, "wcc_test");
  vecInfo.setVectorShape(*wccCond, VectorShape::uni());

  // Create the runtime ckeck and insert the code variant
  auto * wccBr = builder.CreateCondBr(wccCond, clonedConBlock, EndBlock);
  vecInfo.setVectorShape(*wccBr, VectorShape::uni());

  auto * CstartBr = BranchInst::Create(TrueBlock, StartBlock);
  vecInfo.setVectorShape(*CstartBr,VectorShape::uni());

  //We have to make sure that all the live-out value from ThenBlock will be patched for clonedThenBlock
  //Live out value include the live in value from the previous block and not consumed by the current block and also include the new live out from this basic block
  //all the live-out value, we check all their use in the successor block and also maybe used in other blocks
  for (const auto &vecpair : CopiedBBPairs) {
    for (auto & Inst : *(vecpair.first)) {
      for (auto UI = Inst.use_begin(), E = Inst.use_end(); UI != E;) {
        Use &use = *UI++;
        auto * userInst = cast<Instruction>(use.getUser());

        bool needpatch = true;
        for (auto *BB:DominatedBBs) {
          if (userInst->getParent() == BB) {
            needpatch = false;
            break;
          }
        }
        if (needpatch == false) continue;

        auto clonedthenLiveOut = &LookUp(cloneMap, Inst);
        if (isa<PHINode>(userInst)){
          auto * userPhi = dyn_cast<PHINode>(userInst);
          //Tackle the case that the phi node basic block is not the same with the the inst.parent
          for (size_t inIdx = 0; inIdx < userPhi->getNumIncomingValues(); ++inIdx) {
            auto * inBlock = userPhi->getIncomingBlock(inIdx);
            auto * inVal = userPhi->getIncomingValue(inIdx);
            auto * inInst = dyn_cast<Instruction>(inVal);
            auto * Instin = dyn_cast<Instruction>(&Inst);
            if (inInst==Instin) {
              auto it = std::find_if(CopiedBBPairs.begin(), CopiedBBPairs.end(), [&inBlock](std::pair<BasicBlock *, BasicBlock *> const & elem) {
                return elem.first == inBlock;
              });
              userPhi->addIncoming(clonedthenLiveOut, (*it).second);
              break;
            }
            else
              continue;
          }

          if (userPhi){
            for (size_t inIdx = 0; inIdx < userPhi->getNumIncomingValues(); ++inIdx) {
              auto * inBlock = userPhi->getIncomingBlock(inIdx);
              auto * inVal = userPhi->getIncomingValue(inIdx);
              auto * inInst = dyn_cast<Instruction>(inVal);
              if (!inInst) continue;

              auto inShape = vecInfo.getVectorShape(*inInst);
              auto & defBlock = *inInst->getParent();
              //if(DT.dominates(&defBlock, inBlock)) continue; //TODO whether this is necessary or not

              //ssa repair
              SmallVector<PHINode*, 8> phiVec;
              SSAUpdater ssaUpdater(&phiVec);
              ssaUpdater.Initialize(inInst->getType(), "_prom");
              ssaUpdater.AddAvailableValue(&defBlock, inInst);

              auto & fixedDef = *ssaUpdater.GetValueAtEndOfBlock(inBlock);
              for (auto * phi : phiVec)
                vecInfo.setVectorShape(*phi, inShape);

              userPhi->setIncomingValue(inIdx, &fixedDef);
            }
          }
        }
      }
    }
  }

  //Here tackle all the constant value Phi nodes from the successor of the DominatedBB
  for (const auto & vecpair : CopiedBBPairs) {
    BasicBlock *predBlock = vecpair.first;
    for (llvm::succ_iterator itSucc = llvm::succ_begin(predBlock); itSucc != llvm::succ_end(predBlock); ++itSucc) {
      llvm::BasicBlock *succBlock = *itSucc;
      bool needpatch = true;
      for (auto *BB : DominatedBBs) {
        if(succBlock == BB){
          needpatch = false;
          break;
        }
      }
      if (needpatch == false) continue;

      llvm::BasicBlock::iterator itPHI;
      for (itPHI = succBlock->begin(); itPHI != succBlock->end(); itPHI++){
        if (isa<PHINode>(itPHI)){
          auto * userPhi = dyn_cast<PHINode>(itPHI);
          for (size_t inIdx = 0; inIdx < userPhi->getNumIncomingValues(); ++inIdx) {
            auto * inBlock = userPhi->getIncomingBlock(inIdx);
            if (inBlock == predBlock) {
              auto * inVal = userPhi->getIncomingValue(inIdx);
              auto * inInst = dyn_cast<Instruction>(inVal);
              if(inInst)
                continue;
              else
                userPhi->addIncoming(inVal, vecpair.second);
            }
            else
             continue;
          }
        }
      }
    }
  }

  domTree.recalculate(vecInfo.getScalarFunction());
  postDomTree.recalculate(vecInfo.getScalarFunction());
  domTree.verify();
  postDomTree.verify();

  IF_DEBUG_CIF {
  errs() << "LLVM IR after transformCoherentCF" << "\n";
  branch.getFunction()->print(errs());
  }
}

//TODO This is very conservative for now.
bool
IsAffine (Instruction* instruction)
{
  //Whether there is need to record the visited instructions to optimize it
  //std::set<Instruction*> visitedInstructions;

  //first check the opcode
  unsigned Opcode = instruction->getOpcode();
  switch (Opcode) {
    case Instruction::GetElementPtr: return false;
    case Instruction::Mul:
    case Instruction::Sub:
    case Instruction::Add:
    case Instruction::FMul:
    case Instruction::FSub:
    case Instruction::FAdd:
    case Instruction::And:
    case Instruction::ICmp:
    case Instruction::FCmp:
    case Instruction::Or: break;
    default:
    {
      return false;
    }
  }
  //Then check the operands
  bool affine = true;
  for (unsigned i = 0; i < instruction->getNumOperands(); i++) {
    auto* instructionOperand = instruction->getOperand(i);
    if (auto* instructionOperandAsInstruction = dyn_cast<Instruction>(instructionOperand))
    {
      auto condShape = vecInfo.getVectorShape(*instructionOperandAsInstruction);
      if(condShape.hasStridedShape() && !condShape.isUniform())
        continue;
      else
        affine &= IsAffine(instructionOperandAsInstruction);
    }
    else if (isa<ConstantData>(instructionOperand))
    {
      continue;
    }
    else if (isa<Argument>(instructionOperand))
    {
      if (instructionOperand->getType()->isIntegerTy() | instructionOperand->getType()->isFloatingPointTy ())
        continue;
      else
        return false;
    }
    else{
      return false;
    }
  }
  return affine;
}


// 0  : do not TransformBranch
// -1 : TransformBranch onTrue
// 1 : TransformBranch onFalse
// currently only cope with BOSCC and CIF
int
PickSuccessorForCIF(BranchInst & branch, bool onTrueLegal, bool onFalseLegal) {
  double trueRatio = 0.0;
  double falseRatio = 0.0;
  size_t onTrueScore = 0;
  size_t onFalseScore = 0;

  BranchEst.analyze(branch, trueRatio, falseRatio, onTrueScore, onFalseScore);

  const char * Ratio_T = "CIF_T";
  const char * Score_LIMIT = "CIF_LIMIT";

  const double maxminRatio = GetValue<double>(Ratio_T, 0.40);
  const size_t minScore = GetValue<size_t>(Score_LIMIT, 100);

  IF_DEBUG_CIF { errs() << *Ratio_T << maxminRatio << *Score_LIMIT << minScore << "\n"; }

  bool onTrueBeneficial = onTrueScore >= minScore && trueRatio < maxminRatio;
  bool onFalseBeneficial = onFalseScore >= minScore && falseRatio < maxminRatio;

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

  size_t numCIFBranches = 0;

  ReversePostOrderTraversal<Function*> RPOT(&vecInfo.getScalarFunction());

  for (auto * BB : RPOT) {
    if (!vecInfo.inRegion(*BB)) continue;
    auto * term = BB->getTerminator();
    auto * branchInst = dyn_cast<BranchInst>(term);

    //consider divergent conditional branches
    if (!branchInst) continue;
    if (!branchInst->isConditional()) continue;
    if (vecInfo.getVectorShape(*branchInst).isUniform()) continue;

    auto * branchCond = branchInst->getCondition();

    // run legality checks
    bool onTrueLegal, onFalseLegal;
    if (!BranchEst.CheckLegality(*branchInst, onTrueLegal, onFalseLegal)) continue;

    // check for SESE
    BasicBlock * onTrueBlock = branchInst->getSuccessor(0);
    BasicBlock * onFalseBlock = branchInst->getSuccessor(1);
    BasicBlock * exitOnTrueBlock = BranchEst.getExitBlock(onTrueBlock);
    if (!exitOnTrueBlock) continue; //return false;

    if (exitOnTrueBlock != onFalseBlock) { //There is else branch
      if (!BranchEst.getExitBlock(onFalseBlock))
        continue; // return false;
    }


    // if affine fails, then use high probability
    if (IsAffine(dyn_cast<Instruction>(branchCond))) {
      IF_DEBUG_CIF {errs()<< *branchCond << " is affine condition" << "\n";}
      ++numCIFBranches;
      transformCoherentCF(*branchInst, 0);
    }
    else {
      int score = PickSuccessorForCIF(*branchInst, onTrueLegal, onFalseLegal);
      if (score == 0) continue;
      int succIdx = score < 0 ? 0 : 1;

      ++numCIFBranches;

      Report() << "CIF: dynamic variant succ " << branchInst->getSuccessor(succIdx)->getName() << " of block " << "\n";

      IF_DEBUG_CIF {errs()<< *branchCond <<  " has high probabilty for CIF " << "\n";}
      transformCoherentCF(*branchInst, succIdx);
    }
  }

  if (numCIFBranches > 0) Report() << "CIF: inserted " << numCIFBranches << " CIF branches\n";

  // recover
  postDomTree.recalculate(vecInfo.getScalarFunction());

  domTree.verify();

  IF_DEBUG_CIF {
    errs() << "--- FUNCTION AFTER CIF ---:\n";
    Dump(vecInfo.getScalarFunction());
  }

  return false;
}

};

bool
CoherentIFTransform::run() {
  CoherentIF coherentif(vecInfo, platInfo, maskEx, domTree, postDomTree, loopInfo, pbInfo);
  return coherentif.run();
}

CoherentIFTransform::CoherentIFTransform (VectorizationInfo & _vecInfo, PlatformInfo & _platInfo, MaskExpander & _maskEx, llvm::FunctionAnalysisManager &FAM)
: vecInfo(_vecInfo)
, platInfo(_platInfo)
, maskEx(_maskEx)
, domTree(FAM.getResult<DominatorTreeAnalysis>(vecInfo.getScalarFunction()))
, postDomTree(FAM.getResult<PostDominatorTreeAnalysis>(vecInfo.getScalarFunction()))
, loopInfo(*FAM.getCachedResult<LoopAnalysis>(vecInfo.getScalarFunction()))
, pbInfo(&FAM.getResult<BranchProbabilityAnalysis>(vecInfo.getScalarFunction()))
{}
