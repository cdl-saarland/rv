#include "rv/transform/lowerDivergentSwitches.h"
#include "rv/vectorizationInfo.h"

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Analysis/LoopInfo.h>

#include <cassert>

using namespace llvm;

namespace rv {

void
LowerDivergentSwitches::replaceIncoming(BasicBlock & phiBlock, BasicBlock & oldIncoming, BasicBlock & newIncoming) {
  for (auto & inst : phiBlock) {
    if (!isa<PHINode>(inst)) break;
    auto & phi = cast<PHINode>(inst);
    for (int i = 0; i < (int) phi.getNumIncomingValues(); ++i) {
      if (phi.getIncomingBlock(i) != &oldIncoming) continue;
      phi.setIncomingBlock(i, &newIncoming);
      assert(vecInfo.getVectorShape(phi).isVarying() && "undetected divergent phi");
    }
  }
}

void
LowerDivergentSwitches::lowerSwitch(SwitchInst & switchInst) {
  auto & switchBlock = *switchInst.getParent();
  auto switchLoop = LI.getLoopFor(&switchBlock);

  BasicBlock * defaultBlock = nullptr;
  for (auto & itCase : switchInst.cases()) {
    auto * caseVal = itCase.getCaseValue();
    auto * caseBlock = itCase.getCaseSuccessor();
    auto * branchBlock = BasicBlock::Create(switchInst.getContext(), "divswitch", switchBlock.getParent(), switchInst.getDefaultDest());

    IRBuilder<> builder(branchBlock);
    // update LI
    if (switchLoop) {
      switchLoop->addBasicBlockToLoop(branchBlock, LI);
    }

    // first default block (actual switch default case)
    if (!defaultBlock) {
      defaultBlock = switchInst.getDefaultDest();
      replaceIncoming(*defaultBlock, switchBlock, *defaultBlock);
    }

    // create case branch
    auto * caseCond = builder.CreateICmpEQ(switchInst.getCondition(), caseVal);;
    auto * brInst = builder.CreateCondBr(caseCond, caseBlock, defaultBlock);
    vecInfo.setVectorShape(*brInst, VectorShape::varying());
    defaultBlock = caseBlock;

    // update shapes
    vecInfo.setVectorShape(*caseCond, VectorShape::varying());

    // update incoming edges from switchBlock -> branchBlock
    replaceIncoming(*caseBlock, switchBlock, *branchBlock);

    // new default
    defaultBlock = branchBlock;
  }

  // finally erase the switch
  auto * firstBr = BranchInst::Create(defaultBlock, &switchInst);
  vecInfo.setVectorShape(*firstBr, VectorShape::uni());
  switchInst.eraseFromParent();
}

LowerDivergentSwitches::LowerDivergentSwitches(VectorizationInfo & _vecInfo, LoopInfo & _LI)
: vecInfo(_vecInfo)
, LI(_LI)
{}

bool
LowerDivergentSwitches::run() {
  std::vector<SwitchInst*> switchInsts;

  vecInfo.getRegion()->for_blocks([&](const BasicBlock & block) {
      auto * swInst = dyn_cast<SwitchInst>(block.getTerminator());
      if (!swInst) return true;
      if (vecInfo.getVectorShape(*swInst->getCondition()).isUniform()) return true;

      switchInsts.push_back(const_cast<SwitchInst*>(swInst));
      return true;
  });

  for (auto * swInst : switchInsts) {
    lowerSwitch(*swInst);
  }

  return !switchInsts.empty();
}


} // namespace rv
