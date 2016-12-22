//===- ProgramDependenceAnalysis.h----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//

#ifndef RV_PROGRAMDEPENDENCEANALYSIS_H
#define RV_PROGRAMDEPENDENCEANALYSIS_H

#include <string>
#include <map>

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
#include "rv/rvInfo.h"
#include "rv/VectorizationInfoProxyPass.h"
#include "rv/rvInfoProxyPass.h"
#include "rv/Region/Region.h"

namespace rv {

class PDAWrapperPass : public llvm::FunctionPass {
  static char ID;
public:
  PDAWrapperPass() : FunctionPass(ID) { }
  PDAWrapperPass(const PDAWrapperPass&) = delete;
  PDAWrapperPass& operator=(PDAWrapperPass) = delete;

  void getAnalysisUsage(AnalysisUsage& Info) const override {
    Info.addRequired<DominatorTreeWrapperPass>();
    Info.addRequired<PostDominatorTree>();
    Info.addRequired<DFGBaseWrapper<true>>();
    Info.addRequired<DFGBaseWrapper<false>>();
    Info.addRequired<RVInfoProxyPass>();
    Info.addRequired<LoopInfoWrapperPass>();
    Info.addRequired<VectorizationInfoProxyPass>();

    Info.setPreservesAll();
  }

  bool runOnFunction(Function& F) override;

  // void print(llvm::raw_ostream& O, const Module* M) const override;
};

class PDA {
  std::set<const Value*> overrides;
  DataLayout layout;

public:
  using ValueMap          = std::map<const Value*, VectorShape>;
  using InstructionSet    = llvm::SmallPtrSet<const Instruction*, 32>;
  using FuncInfo          = native::VectorMappingMap;

  PDA(VectorizationInfo& VecInfo,
      const CDG& cdg,
      const DFG& dfg,
      const FuncInfo& Funcinfo,
      const LoopInfo& LoopInfo);

  PDA(const PDA&) = delete;
  PDA& operator=(PDA) = delete;

  void analyze(Function& F);

  //---------------------- Map access -------------------------//
  /// Get the shape for a value
  //  if loop carried, this is the shape observed within the loop that defines @V
  const VectorShape& getShape(const Value* const V);

  //---------------------- Iterators --------------------------//
  typename ValueMap::iterator begin();
  typename ValueMap::iterator end();
  typename ValueMap::const_iterator begin() const;
  typename ValueMap::const_iterator end() const;

private:
  VectorizationInfo& mVecinfo;  // This will be the output
  const CDG& mCDG;      // Preserves CDG
  const DFG& mDFG;      // Preserves DFG
  const LoopInfo& mLoopInfo; // Preserves LoopInfo
  const FuncInfo& mFuncinfo;

  Region* mRegion;

  ValueMap mValue2Shape;    // Computed shapes
  InstructionSet mWorklist;       // Next instructions to handle

  std::map<BasicBlock*, std::vector<BasicBlock*>> mDivergenceCauseMap;

  // VectorShape analysis logic

  // Initialize all statically known shapes (constants, arguments via argument mapping,
  // shapes set by the user)
  void init(Function& F);

  // Run Fix-Point-Iteration after initialization
  void compute(Function& F);

  // Returns true if this block is contained in the region we want to analyze
  bool isInRegion(const BasicBlock* BB);
  bool isInRegion(const Instruction& inst);

  VectorShape computeShapeForInst(const Instruction* I);
  VectorShape computeShapeForBinaryInst(const BinaryOperator* I);
  VectorShape computeShapeForCastInst(const CastInst* I);

  // Update a value with its new computed shape, recursing into users if it has changed
  void update(const Value* const V, VectorShape AT);

  // Calls update on every user of this PHI that is not in its loop
  // void updateOutsideLoopUsesVarying(const PHINode* PHI, const Loop* PHILoop);
  void updateOutsideLoopUsesVarying(const Loop* divLoop);

  // Adds all users of V to the worklist to continue iterating,
  // unless the concept of shape is not defined for the user (e.g. void return calls)
  void addRelevantUsersToWL(const Value* V);

  // Corrects the shapes for any alloca operand to continous/varying
  // and recomputes all shapes dependent on them from scratch
  // IMPORTANT: The result is as if the respective alloca had been
  // initialized continous/varying, the dependent values have
  // their shapes reset to bottom before recomputation
  void updateAllocaOperands(const Instruction* I);

  // Resets the shape of this value and every value in the user graph to bottom
  void eraseUserInfoRecursively(const Value* V);

  // Join shapes of all loop exits
  // FIXME: so maybe call it joinExitShapes?
  VectorShape joinExitShapes(const Loop* loop);

  VectorShape joinOperands(const Instruction* const I);

  // Returns true iff all operands currently have a computed shape
  // This is essentially a negated check for bottom
  bool allOperandsHaveShape(const Instruction* I);

  // Mandatory block analysis
  void markSuccessorsMandatory(const BasicBlock* endsVarying);
  void markDivergentLoopLatchesMandatory();
  void markLoopLatchesRecursively(const Loop* loop);
  void markDependentLoopExitsMandatory(const BasicBlock* endsVarying);

  // Returns true iff the constant is aligned respective to mVectorizationFactor
  unsigned getAlignment(const Constant* c) const;

  // Transfers the computed VectorShapes from mvalues to the VectorizationInfo object
  // TODO just write into mVecinfo immediately?
  void fillVectorizationInfo(Function& F);
};

static FunctionPass* createNewVectorizationAnalysisPass() {
  return new PDAWrapperPass();
}

}

#endif
