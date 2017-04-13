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

#include <llvm/ADT/SmallSet.h>
#include <llvm/ADT/PostOrderIterator.h>

#include <llvm/Transforms/Utils/ValueMapper.h>

#include "rv/vectorizationInfo.h"
#include "rv/PlatformInfo.h"

#include <rvConfig.h>
#include "report.h"

using namespace rv;
using namespace llvm;

#if 1
#define IF_DEBUG_BOSCC IF_DEBUG
#else
#define IF_DEBUG_BOSCC if (false)
#endif

struct Impl {
  VectorizationInfo & vecInfo;
  PlatformInfo & platInfo;
  DominatorTree & domTree;
  LoopInfo & loopInfo;
  Module & mod;


Impl(VectorizationInfo & _vecInfo, PlatformInfo & _platInfo, DominatorTree & _domTree, LoopInfo & _loopInfo)
: vecInfo(_vecInfo)
, platInfo(_platInfo)
, domTree(_domTree)
, loopInfo(_loopInfo)
, mod(*vecInfo.getScalarFunction().getParent())
{
}

Function &
requestMaskIntrinsic(std::string name) {
  return *platInfo.requestMaskReductionFunc("rv_any");
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

// create a BOSCC condition block
  std::stringstream blockName;
  blockName << succBlock->getName().str() << "_boscc";
  auto * bosccBlock = BasicBlock::Create(context, blockName.str(), &vecInfo.getScalarFunction());


  IRBuilder<> builder(bosccBlock);

// flip the condition (if need be)
   assert(branchOnTrue && "branch flipping not yet implemented");
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


// fix up PHI nodes in the exit block (we have a new incoming branch from the bosccBlock)
  for (auto & inst : *exitBlock) {
    auto * phi =  dyn_cast<PHINode>(&inst);
    if (!phi) break;
    phi->addIncoming(UndefValue::get(phi->getType()), bosccBlock);
  }
}

bool
run() {
  return false;
}

};


bool
BOSCCTransform::run() {
  Impl impl(vecInfo, platInfo, domTree, loopInfo);
  return impl.run();
}


BOSCCTransform::BOSCCTransform(VectorizationInfo & _vecInfo, PlatformInfo & _platInfo, llvm::DominatorTree & _domTree, llvm::LoopInfo & _loopInfo)
: vecInfo(_vecInfo)
, platInfo(_platInfo)
, domTree(_domTree)
, loopInfo(_loopInfo)
{}
