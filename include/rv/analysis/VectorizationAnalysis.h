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

#include "rv/config.h"
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
  Config config;

public:
  VAWrapperPass() : FunctionPass(ID), config() { }
  VAWrapperPass(Config _config) : FunctionPass(ID), config(_config) { }
  VAWrapperPass(const VAWrapperPass&) = delete;
  VAWrapperPass& operator=(VAWrapperPass) = delete;

  void getAnalysisUsage(llvm::AnalysisUsage& Info) const override;
  bool runOnFunction(llvm::Function& F) override;
};

class VectorizationAnalysis {
  Config config;

  /// In- and output
  VectorizationInfo& mVecinfo;

  /// Next instructions to handle
  std::queue<const llvm::Instruction*> mWorklist;

  const llvm::DataLayout& layout;
  const llvm::LoopInfo& mLoopInfo; // Preserves LoopInfo

  // Shape computation:
  const VectorFuncMap& mFuncinfo;

  // Divergence computation:
  BranchDependenceAnalysis BDA;

public:
  VectorizationAnalysis(Config config,
                        PlatformInfo & platInfo,
                        VectorizationInfo& VecInfo,
                        const llvm::CDG& cdg,
                        const llvm::DFG& dfg,
                        const llvm::LoopInfo& LoopInfo);

  VectorizationAnalysis(const VectorizationAnalysis&) = delete;
  VectorizationAnalysis& operator=(VectorizationAnalysis) = delete;

  void analyze(const llvm::Function& F);

  void addInitial(const llvm::Instruction* inst, VectorShape shape);

private:
  /// Get the shape for a value
  //  if loop carried, this is the shape observed within the loop that defines @V
  VectorShape getShape(const llvm::Value* const V);

  // Initialize all statically known shapes (constants, arguments via argument mapping,
  // shapes set by the user)
  void init(const llvm::Function& F);

  // adjust missing shapes to undef, optimize pointer shape alignments
  void adjustValueShapes(const llvm::Function& F);

  // Run Fix-Point-Iteration after initialization
  void compute(const llvm::Function& F);

  // specialized transfer functions
  VectorShape computePHIShape(const llvm::PHINode& phi);

  // only call these if all operands have defined shape
  VectorShape computeShapeForInst(const llvm::Instruction* I);
  VectorShape computeShapeForBinaryInst(const llvm::BinaryOperator* I);
  VectorShape computeShapeForCastInst(const llvm::CastInst* I);

  // generic (fallback) transfer function for instructions w/o side effects
  VectorShape computeGenericArithmeticTransfer(const llvm::Instruction& I);

  // Update a value with its computed shape, adding users to the WL if a change occured
  void update(const llvm::Value* const V, VectorShape AT);

  // Returns true iff the shape has been changed
  bool updateShape(const llvm::Value* const V, VectorShape AT);
  void analyzeDivergence(const llvm::BranchInst* const branch);

  // Adds all dependent values of V to the worklist:
  // - Any user of this value in the region (minus void-returning calls)
  // - Any alloca used by this value if it is not of uniform shape
  void addDependentValuesToWL(const llvm::Value* V);

  VectorShape joinIncomingValues(const llvm::PHINode& phi);

  // Returns true iff all operands currently have a computed shape
  // This is essentially a negated check for bottom
  bool pushMissingOperands(const llvm::Instruction* I);

  // Cast undefined instruction shapes to uniform shapes
  void fixUndefinedShapes(const llvm::Function& F);

  // Mark loops as divergent
  void computeLoopDivergence();
};

llvm::FunctionPass* createVectorizationAnalysisPass(Config config=Config());

}

#endif
