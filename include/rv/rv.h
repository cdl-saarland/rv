//===- rv.h ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//

#ifndef RV_RV_H
#define RV_RV_H

#include "rv/PlatformInfo.h"
#include "rv/analysis/DFG.h"
#include "rv/transform/maskExpander.h"
#include "rv/optConfig.h"
#include "rv/analysis/VectorizationAnalysis.h"

#include "llvm/Transforms/Utils/ValueMapper.h"

#include <llvm/Support/Compiler.h>

namespace llvm {
  class LoopInfo;
  struct PostDominatorTree;
  class DominatorTree;
  class ScalarEvolution;
  class MemoryDependenceResults;
  class BranchProbabilityInfo;
}

namespace rv {

class VectorizationInfo;

/*
 * The new vectorizer interface.
 *
 * The functionality of this interface relies heavily on prior loop simplification
 * (see LoopExitCanonicalizer), aswell as the elimination of critical edges beforehand.
 *
 * No guarantees are made in case the Function that is handled has critical edges,
 * or loop exits with with more than one predecessor are present.
 *
 * Vectorization of the Function in question may violate the original semantics, if
 * a wrong analysis is used as the VectorizationInfo object. The user is responsible for
 * running the analysis phase prior, or specifying a valid analysis himself.
 */
[[deprecated]]
class VectorizerInterface {
public:
    LLVM_ATTRIBUTE_DEPRECATED(VectorizerInterface(PlatformInfo & _platform, OptConfig optConfig = OptConfig()), "use the new API in "rv/session.h" instead");

    //
    // Analyze properties of the scalar function that are needed later in transformations
    // towards its SIMD equivalent.
    //
    // This expects initial information about arguments to be set in the VectorizationInfo object
    // (see VectorizationInfo).
    //
    void analyze(VectorizationInfo& vecInfo,
                 const rv::CDG& cdg,
                 const rv::DFG& dfg,
                 const llvm::LoopInfo& loopInfo,
                 VAConfig vaConfig = VAConfig());

    //
    // Linearize divergent regions of the scalar function to preserve semantics for the
    // vectorized function
    //
    bool
    linearize(VectorizationInfo& vecInfo,
              rv::CDG& cdg,
              rv::DFG& dfg,
              llvm::LoopInfo& loopInfo,
              llvm::PostDominatorTree& postDomTree,
              llvm::DominatorTree& domTree,
              llvm::BranchProbabilityInfo * pbInfo = nullptr);

    //
    // Produce vectorized instructions
    //
    bool
    vectorize(VectorizationInfo &vecInfo,
              llvm::DominatorTree &domTree,
              llvm::LoopInfo & loopInfo,
              llvm::ScalarEvolution & SE,
              llvm::MemoryDependenceResults & MDR,
              llvm::ValueToValueMapTy * vecInstMap);

    //
    // Ends the vectorization process on this function, removes metadata and
    // writes the function to a file
    //
    void finalize();

    PlatformInfo & getPlatformInfo() const { return platInfo; }

private:
    OptConfig optConfig;
    PlatformInfo & platInfo;

    void addIntrinsics();
};


  // inline complex arithmetic function (compiler-rt runtime library)
  void lowerComplexArithmetic(VectorizationInfo & vecInfo, llvm::LoopInfo & loopInfo);

   // implement all rv_* intrinsics - such that they behave as if the vector width as one
   // this is necessary to make scalar functions with predicate intrinsics executable
   // the SIMS semantics of the function will change if @scalar func used any mask intrinsics
  void lowerIntrinsics(llvm::Function & scalarFunc);
  void lowerIntrinsics(llvm::Module & mod);
}

#endif // RV_RV_H
