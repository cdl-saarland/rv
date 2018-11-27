//===- VectorizationAnalysis.h----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//

#ifndef RV_VECTORIZATIONANALYSIS_H_
#define RV_VECTORIZATIONANALYSIS_H_

#include <map>
#include <queue>
#include <string>
#include <unordered_set>

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/Analysis/SyncDependenceAnalysis.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Operator.h"
#include "llvm/IR/Use.h"
#include "llvm/IR/Value.h"
#include "llvm/Pass.h"
#include "llvm/Support/GenericDomTree.h"
#include "llvm/Support/raw_ostream.h"

#include "rv/PlatformInfo.h"
#include "rv/VectorizationInfoProxyPass.h"
#include "rv/analysis/AllocaSSA.h"
#include "rv/config.h"
#include "rv/region/FunctionRegion.h"
#include "rv/region/Region.h"
#include "rv/shape/vectorShapeTransformer.h"
#include "rv/vectorMapping.h"
#include "rv/vectorizationInfo.h"

namespace llvm {
class LoopInfo;
}

namespace rv {
using InstVec = const std::vector<const llvm::Instruction *>;

class VAWrapperPass : public llvm::FunctionPass {
  static char ID;
  Config config;

public:
  VAWrapperPass() : FunctionPass(ID), config() {}
  VAWrapperPass(Config _config) : FunctionPass(ID), config(_config) {}
  VAWrapperPass(const VAWrapperPass &) = delete;
  VAWrapperPass &operator=(VAWrapperPass) = delete;

  void getAnalysisUsage(llvm::AnalysisUsage &Info) const override;
  bool runOnFunction(llvm::Function &F) override;
};

class VectorizationAnalysis {
  Config config;
  PlatformInfo &platInfo;

  /// In- and output
  VectorizationInfo &vecInfo;

  /// Next instructions to handle
  std::queue<const llvm::Instruction *> mWorklist;
  std::unordered_set<const llvm::Instruction *> mOnWorklist;

  const llvm::DataLayout &layout;
  const llvm::LoopInfo &LI; // Preserves LoopInfo
  const llvm::DominatorTree &DT;

  // Divergence computation:
  llvm::SyncDependenceAnalysis SDA;

  FunctionRegion funcRegion;
  Region funcRegionWrapper;
  AllocaSSA allocaSSA;

  llvm::DenseSet<const llvm::BasicBlock *> mControlDivergentBlocks;

  bool updateTerminator(const llvm::Instruction &Term) const;

  /// update disjoin paths divergence (and push PHIs)
  // return whether @JoinBlock is a divergent loop exit from @BranchLoop
  bool propagateJoinDivergence(const llvm::BasicBlock &JoinBlock,
                               const llvm::Loop *BranchLoop);
  void taintLoopLiveOuts(const llvm::BasicBlock &LoopHeader);

public:
  VectorizationAnalysis(Config config, PlatformInfo &platInfo,
                        VectorizationInfo &VecInfo,
                        const llvm::DominatorTree &domTree,
                        const llvm::PostDominatorTree &postDomTree,
                        const llvm::LoopInfo &LoopInfo);

  VectorizationAnalysis(const VectorizationAnalysis &) = delete;
  VectorizationAnalysis &operator=(VectorizationAnalysis) = delete;

  void analyze();
  void updateAnalysis(InstVec &updateList);

  void addInitial(const llvm::Instruction *inst, VectorShape shape);

private:
  // worklist manipulation
  // insert @inst into the worklist if its not already not the list
  bool putOnWorklist(const llvm::Instruction &inst);
  // take an element off the worklist, return nullptr if the worklist is empty
  const llvm::Instruction *takeFromWorklist();

  /// Get the shape for a value
  //  if loop carried, this is the shape observed within the loop that defines
  //  @V
  VectorShape getShape(const llvm::Value &V);

  // Initialize all statically known shapes (constants, arguments via argument
  // mapping, shapes set by the user)
  void init(const llvm::Function &F);

  // adjust missing shapes to undef, optimize pointer shape alignments
  void adjustValueShapes(const llvm::Function &F);

  // Run Fix-Point-Iteration after initialization
  void compute(const llvm::Function &F);

  // specialized transfer functions
  VectorShape computePHIShape(const llvm::PHINode &phi);

  // Returns true iff the shape has been changed
  bool updateShape(const llvm::Value &V, VectorShape AT);
  void analyzeDivergence(const llvm::Instruction &termInst);

  // re-schedule al phi nodes in @Block
  void pushPHINodes(const llvm::BasicBlock &Block);

  // Returns true iff all operands currently have a computed shape
  // This is essentially a negated check for bottom
  bool pushMissingOperands(const llvm::Instruction &I);

  // push all users of @V to the worklist.
  void pushUsers(const llvm::Value &V);

  // control divergence propagation
  void propagateBranchDivergence(const llvm::Instruction &Term);
  void propagateLoopDivergence(const llvm::Loop &ExitingLoop);

  // Cast undefined instruction shapes to uniform shapes
  void promoteUndefShapesToUniform(const llvm::Function &F);
};

llvm::FunctionPass *createVectorizationAnalysisPass(Config config = Config());

} // namespace rv

#endif
