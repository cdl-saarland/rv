//===- VectorizationAnalysis.h----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//

#ifndef RV_VECTORIZATIONANALYSIS_H_
#define RV_VECTORIZATIONANALYSIS_H_

#include <string>
#include <map>
#include <queue>

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/InstVisitor.h"
#include "llvm/IR/Operator.h"
#include "llvm/IR/Use.h"
#include "llvm/IR/Value.h"
#include "llvm/Pass.h"
#include "llvm/Support/GenericDomTree.h"
#include "llvm/Support/raw_ostream.h"

#include "DFG.h"

#include "rv/vectorizationInfo.h"
#include "rv/vectorMapping.h"
#include "rv/VectorizationInfoProxyPass.h"
#include "rv/region/Region.h"
#include "rv/PlatformInfo.h"
#include "rv/analysis/BranchDependenceAnalysis.h"

namespace llvm {
  class LoopInfo;
}

namespace rv {

class VAWrapperPass : public llvm::FunctionPass {
  static char ID;
public:
  VAWrapperPass() : FunctionPass(ID) { }
  VAWrapperPass(const VAWrapperPass&) = delete;
  VAWrapperPass& operator=(VAWrapperPass) = delete;

  void getAnalysisUsage(AnalysisUsage& Info) const override;
  bool runOnFunction(Function& F) override;
};

class VectorizationAnalysis {
public:
  VectorizationAnalysis(PlatformInfo & platInfo,
                        VectorizationInfo& VecInfo,
                        const CDG& cdg,
                        const DFG& dfg,
                        const LoopInfo& LoopInfo,
                        const DominatorTree & domTree,
                        const PostDominatorTree & postDomTree);

  VectorizationAnalysis(const VectorizationAnalysis&) = delete;
  VectorizationAnalysis& operator=(VectorizationAnalysis) = delete;

  void analyze(Function& F);

private:
  std::set<const Value*> overrides;
  const DataLayout& layout;

  VectorizationInfo& mVecinfo;  // This will be the output
  const CDG& mCDG;      // Preserves CDG
  const DFG& mDFG;      // Preserves DFG
  BranchDependenceAnalysis BDA;
  const LoopInfo& mLoopInfo; // Preserves LoopInfo
  const VectorFuncMap& mFuncinfo;

  Region* mRegion;

  std::queue<const Instruction*> mWorklist;       // Next instructions to handle

  // VectorShape analysis logic

  /// Get the shape for a value
  //  if loop carried, this is the shape observed within the loop that defines @V
  VectorShape getShape(const Value* const V);

  // Initialize all statically known shapes (constants, arguments via argument mapping,
  // shapes set by the user)
  void init(Function& F);

  // adjust missing shapes to undef, optimize pointer shape alignments
  void adjustValueShapes(Function& F);

  // Run Fix-Point-Iteration after initialization
  void compute(Function& F);

  // Returns true if this block is contained in the region we want to analyze
  bool isInRegion(const BasicBlock& BB);
  bool isInRegion(const Instruction& inst);

  // specialized transfer functions
  VectorShape computePHIShape(const PHINode& phi);

  // only call these if all operands have defined shape
  VectorShape computeShapeForInst(const Instruction* I);
  VectorShape computeShapeForBinaryInst(const BinaryOperator* I);
  VectorShape computeShapeForCastInst(const CastInst* I);

  // generic (fallback) transfer function for instructions w/o side effects
  VectorShape computeGenericArithmeticTransfer(const Instruction& I);

  // Update a value with its computed shape, adding users to the WL if a change occured
  void update(const Value* const V, VectorShape AT);

  // Returns true iff the shape has been changed
  bool updateShape(const Value* const V, VectorShape AT);
  void analyzeDivergence(const BranchInst* const branch);

  // Calls update on every user of this PHI that is not in its loop
  void updateLCSSAPhisVarying(const Loop* divLoop);

  // Adds all dependent values of V to the worklist:
  // - Any user of this value in the region (minus void-returning calls)
  // - Any alloca used by this value if it is not of uniform shape
  void addDependentValuesToWL(const Value* V);

  // Return true iff all of loop's exit terminators have a uniform shape
  bool allExitsUniform(const Loop* loop);

  VectorShape joinOperands(const Instruction& I);

  // Returns true iff all operands currently have a computed shape
  // This is essentially a negated check for bottom
  bool pushMissingOperands(const Instruction* I);

  unsigned getAlignment(const Constant* c) const;

  // Cast undefined instruction shapes to uniform shapes
  void fixUndefinedShapes(Function& F);
};

FunctionPass* createVectorizationAnalysisPass();

}

#endif
