#include "rv/transform/CoherentIFTransform.h"

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
#include "cns/llvmDuplication.h"
#include "rv/utils.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/GenericDomTree.h"
#include "llvm/Transforms/Utils/SSAUpdater.h"
#include "rv/analysis/BranchEstimate.h"

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

CoherentIF(VectorizationInfo & _vecInfo, PlatformInfo & _platInfo,  MaskExpander & _maskEx, DominatorTree & _domTree, PostDominatorTree & _postDomTree, LoopInfo & _loopInfo, BranchProbabilityInfo * _pbInfo)
: vecInfo(_vecInfo)
, platInfo(_platInfo)
, maskEx(_maskEx)
, domTree(_domTree)
, postDomTree(_postDomTree)
, loopInfo(_loopInfo)
, mod(*vecInfo.getScalarFunction().getParent())
, pbInfo(_pbInfo)
{}

void CloneBasicBlockForDominatedBBs(BasicBlock *BB, SmallVector<BasicBlock *, 16> DominatedBBs, std::vector<std::pair<BasicBlock *, BasicBlock *>> &CopiedBBPairs, Loop *loop, ValueToValueMapTy &cloneMap)
{
  auto *inst = BB->getTerminator();

  unsigned int numsuc = inst->getNumSuccessors();
  assert( 1 <= numsuc && numsuc <= 2);

  //For each succeeding block
  for (unsigned int i = 0; i < numsuc; i++)
  {
    BasicBlock *child = inst->getSuccessor(i);

   //Check if child has already been copied
    auto it = std::find_if(CopiedBBPairs.begin(), CopiedBBPairs.end(), [&child](std::pair<BasicBlock *, BasicBlock *> const & elem)
    {
      return elem.second == child;
    });

    auto firstit = std::find_if(CopiedBBPairs.begin(), CopiedBBPairs.end(), [&child](std::pair<BasicBlock *, BasicBlock *> const & elem)
    {
      return elem.first == child;
    });

    if ((std::find(DominatedBBs.begin(), DominatedBBs.end(), child) == DominatedBBs.end()) && (it == CopiedBBPairs.end()))
      continue;

    BasicBlock *clonedchild;

    //Block is already copied, use the copy
    if (it != CopiedBBPairs.end())
    {
      clonedchild = (*it).second;
    }
    else if (firstit != CopiedBBPairs.end())
    {
      clonedchild = (*firstit).second;
    }
    else
    {
      //Block still has to be copied
      clonedchild = cloneBlockAndMapInstructions(child, cloneMap);
      cloneMap[child] = clonedchild;

      auto CSI = clonedchild->begin();
      for(auto I = child->begin(); I != child->end(); ++I, CSI++) {
        vecInfo.setVectorShape(*CSI,vecInfo.getVectorShape(*I));
      }

      if (loop) loop->addBasicBlockToLoop(clonedchild, loopInfo);
      //Add copied block to the pairs
      CopiedBBPairs.push_back(std::make_pair(child,clonedchild));
      //Recursively copy the blocks successors
      CloneBasicBlockForDominatedBBs(clonedchild, DominatedBBs, CopiedBBPairs, loop, cloneMap);
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
transformCoherentCF(BranchInst & branch, int succIdx) {
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

  if (loop) loop->addBasicBlockToLoop(clonedConBlock, loopInfo);

  if (isa<BranchInst>(clonedConBlock->getTerminator()))
    vecInfo.setVectorShape(*clonedConBlock->getTerminator(),vecInfo.getVectorShape(*ConBlock->getTerminator()));

  //Clone the remaining basic blocks and map
  SmallVector<BasicBlock *, 16> DominatedBBs;

  domTree.getDescendants(ConBlock, DominatedBBs);

  std::vector< std::pair< BasicBlock *, BasicBlock *> > CopiedBBPairs;
  CopiedBBPairs.push_back(std::make_pair(ConBlock,clonedConBlock));

  CloneBasicBlockForDominatedBBs(clonedConBlock, DominatedBBs, CopiedBBPairs, loop, cloneMap);

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
  for (const auto &vecpair : CopiedBBPairs)
  {
    for (auto & Inst : *(vecpair.first)) {
      for (auto UI = Inst.use_begin(), E = Inst.use_end(); UI != E;) {
        Use &use = *UI++;
        auto * userInst = cast<Instruction>(use.getUser());

        bool needpatch = true;
        for (auto *BB:DominatedBBs)
        {
          if (userInst->getParent() == BB)
          {
            needpatch = false;
            break;
          }
        }
        if (needpatch == false) continue;

        auto clonedthenLiveOut = &LookUp(cloneMap, Inst);
        if (isa<PHINode>(userInst)){
          auto * userPhi = dyn_cast<PHINode>(userInst);
          //Tackle the case that the phi node basic block is not the same with the the inst.parent
          for (size_t inIdx = 0; inIdx < userPhi->getNumIncomingValues(); ++inIdx)
          {
            auto * inBlock = userPhi->getIncomingBlock(inIdx);
            auto * inVal = userPhi->getIncomingValue(inIdx);
            auto * inInst = dyn_cast<Instruction>(inVal);
            auto *Instin = dyn_cast<Instruction>(&Inst);
            if (inInst==Instin)
            {
              auto it = std::find_if(CopiedBBPairs.begin(), CopiedBBPairs.end(), [&inBlock](std::pair<BasicBlock *, BasicBlock *> const & elem)
              {
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
  for (const auto &vecpair : CopiedBBPairs){
    BasicBlock *predBlock = vecpair.first;
    for (llvm::succ_iterator itSucc = llvm::succ_begin(predBlock); itSucc != llvm::succ_end(predBlock); ++itSucc) {
      llvm::BasicBlock *succBlock = *itSucc;
      bool needpatch = true;
      for (auto *BB:DominatedBBs)
      {
        if(succBlock == BB){
          needpatch = false;
          break;
        }
      }
      if (needpatch == false) continue;

      llvm::BasicBlock::iterator itPHI;
      for (itPHI = succBlock->begin(); itPHI != succBlock->end(); itPHI++){
        if(isa<PHINode>(itPHI)){
          auto * userPhi = dyn_cast<PHINode>(itPHI);
          for (size_t inIdx = 0; inIdx < userPhi->getNumIncomingValues(); ++inIdx) {
            auto * inBlock = userPhi->getIncomingBlock(inIdx);
            if (inBlock == predBlock)
            {
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

  LoopInfo loopInfo1(domTree);
  loopInfo1.verify(domTree);
  loopInfo = std::move(loopInfo1);

  IF_DEBUG_CIF {
  errs() << "LLVM IR after transformCoherentCF" << "\n";
  branch.getFunction()->print(errs());
  }

}

//TODO This is very conservative for now.
/*
bool
IsAffine(Instruction* instruction)
{
  //Whether there is need to record the visited instructions
  //std::set<Instruction*> visitedInstructions;
  //first check the opcode
  unsigned Opcode = instruction->getOpcode();
  switch (Opcode) {
    case Instruction::GetElementPtr:
    {
      return false;
    }
    case Instruction::Mul:
    case Instruction::Sub:
    case Instruction::Add:
    case Instruction::FMul:
    case Instruction::FSub:
    case Instruction::FAdd:
    case Instruction::And:
    case Instruction::ICmp:
    case Instruction::FCmp:
    case Instruction::Or:break;
    case Instruction::Call:
    {
      auto CI = dyn_cast<CallInst>(instruction);
      auto called = CI->getCalledFunction();
      if (called) {
        if (called->isIntrinsic()) {
          switch (called->getIntrinsicID()) {
            case Intrinsic::pacxx_read_tid_x:
            case Intrinsic::pacxx_read_tid_y:
            case Intrinsic::pacxx_read_tid_z:
            case Intrinsic::pacxx_read_ctaid_x:
            case Intrinsic::pacxx_read_ctaid_y:
            case Intrinsic::pacxx_read_ctaid_z:
            case Intrinsic::pacxx_read_nctaid_x:
            case Intrinsic::pacxx_read_nctaid_y:
            case Intrinsic::pacxx_read_nctaid_z:
            case Intrinsic::pacxx_read_ntid_x:
            case Intrinsic::pacxx_read_ntid_y:
            case Intrinsic::pacxx_read_ntid_z: return true;
            default:
            {
              return false;
            }
          }
        }
      }
    }
    default:
    {
      return false;
    }
  }

  //Then check the operands
  bool affine = true;
  for (unsigned i = 0; i < instruction->getNumOperands(); i++) {
    auto* instructionOperand = instruction->getOperand(i);

    if (auto instructionOperandAsInstruction = dyn_cast<Instruction>(instructionOperand))
    {
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
*/

// 0  : do not CIF
// -1 : CIF onTrue
// 1 : CIF onFalse
// Reuse Boscc Heuristic mostly
int
CIFHeuristic(BranchInst & branch) {
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

  // per sucess legality
  // legality checks for speculating over onTrue
  bool onTrueLegal = GetNumPredecessors(*onTrueBlock) == 1; //domTree.dominates(branch.getParent(), onTrueBlock);
  onTrueLegal &= !onTrueLoop || onTrueLoop->getLoopLatch() != onTrueBlock;

  // legality checks for speculating over onTrue
  bool onFalseLegal = GetNumPredecessors(*onFalseBlock) == 1; //domTree.dominates(branch.getParent(), onFalseBlock);
  onFalseLegal &= !onFalseLoop || onFalseLoop->getLoopLatch() != onFalseBlock;

  double trueRatio =0.0;
  double falseRatio =0.0;
  size_t onTrueScore = 0;
  size_t onFalseScore = 0;

  BranchEstimate BranchEstim(vecInfo, platInfo, domTree, loopInfo, pbInfo);
  BranchEstim.analysis(branch, trueRatio, falseRatio, onTrueScore, onFalseScore);

  const double minRatio = GetValue<double>("CIF_T", 0.40);
  const size_t minScore = GetValue<size_t>("CIF_LIMIT", 100);

  IF_DEBUG_CIF { errs() << "CIF_T " << minRatio << " CIF_LIMIT " << minScore << "\n"; }

  //Is there  any need for this? delete by hack
  //if (falseRatio < 0.06) onFalseLegal = false; // DEBUG HACK
  //if (trueRatio < 0.06) onTrueLegal = false; // DEBUG HACK

  IF_DEBUG_CIF { errs() << "score (" << onTrueLegal << ") " << onTrueBlock->getName() << "   " << onTrueScore << "\nscore  (" << onFalseLegal << ") " << onFalseBlock->getName() << "   " << onFalseScore << "\n"; }
  IF_DEBUG_CIF {errs() << "trueRatio: " << trueRatio << " onTrueScore: " << onTrueScore  << " falseRatio: " << falseRatio  << " onFalseScore: " << onFalseScore << " minRatio:" << minRatio << " minScore:  " << minScore << "\n";}

  bool onTrueBeneficial = onTrueScore >= minScore && trueRatio >= minRatio;
  bool onFalseBeneficial = onFalseScore >= minScore && falseRatio >= minRatio;

  bool couldCIFFalse = onFalseBeneficial && onFalseLegal;
  bool couldCIFTrue = onTrueBeneficial && onTrueLegal;

  // otw try to skip the bigger dominated part
  // TODO could also give precedence by region size
  if (couldCIFTrue && (!couldCIFFalse || trueRatio < falseRatio)) {
    return -1;
  } else if (couldCIFFalse) {
    return 1;
  }

  // can not distinguish --> don't CIF
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
    //if affine fails, then use high probability
    //TODO find a RV compatible solution for this part
    /*if (IsAffine(dyn_cast<Instruction>(branchCond)))
    {
      IF_DEBUG_CIF {errs()<< *branchCond << " is affine condition" << "\n";}
      transformCoherentCF(*branchInst, 0);
    }
    else
    */
    {
      int score = CIFHeuristic(*branchInst);
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

CoherentIFTransform::CoherentIFTransform(VectorizationInfo & _vecInfo, PlatformInfo & _platInfo, MaskExpander & _maskEx, llvm::DominatorTree & _domTree, llvm::PostDominatorTree & _postDomTree, llvm::LoopInfo & _loopInfo, BranchProbabilityInfo * _pbInfo)
: vecInfo(_vecInfo)
, platInfo(_platInfo)
, maskEx(_maskEx)
, domTree(_domTree)
, postDomTree(_postDomTree)
, loopInfo(_loopInfo)
, pbInfo(_pbInfo)
{}
