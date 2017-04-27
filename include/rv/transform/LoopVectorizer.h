//===- rv/LoopVectorizer.h - Vectorize Loops  ----------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//
//===----------------------------------------------------------------------===//

#include "llvm/Pass.h"

#include "rv/analysis/reductionAnalysis.h"

namespace llvm {
class Loop;
class LoopInfo;
class DominatorTree;
class ScalarEvolution;
struct PostDominatorTree;
}

namespace rv {

class VectorizerInterface;

class LoopVectorizer : public llvm::FunctionPass {
public:
  static char ID;
  LoopVectorizer() : llvm::FunctionPass(ID), reda() {}

  bool runOnFunction(llvm::Function &F) override;

  /// Register all analyses and transformation required.
  void getAnalysisUsage(llvm::AnalysisUsage &AU) const override;

private:
  std::unique_ptr<ReductionAnalysis> reda;

  bool canVectorizeLoop(llvm::Loop &L);

  bool canAdjustTripCount(llvm::Loop &L, llvm::ScalarEvolution &SE,
                          int VectorWidth, int TripCount);
  int getTripCount(llvm::Loop &L, llvm::ScalarEvolution &SE);
  int getVectorWidth(llvm::Loop &L, llvm::ScalarEvolution &SE);

  bool vectorizeLoop(llvm::Loop &L, llvm::ScalarEvolution &SE, VectorizerInterface &);
  bool vectorizeLoopOrSubLoops(llvm::Loop &L, llvm::ScalarEvolution &SE, VectorizerInterface &);
};

} // namespace rv
