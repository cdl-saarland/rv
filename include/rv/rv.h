//===- rv/rv.h - vectorizer pipeline, main header --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#ifndef RV_RV_H
#define RV_RV_H

#include "rv/PlatformInfo.h"
#include "rv/transform/maskExpander.h"
#include "rv/config.h"

#include "llvm/Transforms/Utils/ValueMapper.h"
#include "llvm/IR/PassManager.h"

namespace llvm {
  class LoopInfo;
  class PostDominatorTree;
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
class VectorizerInterface {
public:
    VectorizerInterface(PlatformInfo & _platform, Config config = Config());

    //
    // Analyze properties of the scalar function that are needed later in transformations
    // towards its SIMD equivalent.
    //
    // This expects initial information about arguments to be set in the VectorizationInfo object
    // (see VectorizationInfo).
    //
    void analyze(VectorizationInfo& vecInfo,
                 llvm::FunctionAnalysisManager &FAM);


    //
    // Linearize divergent regions of the scalar function to preserve semantics for the
    // vectorized function
    //
    bool
    linearize(VectorizationInfo& vecInfo,
              llvm::FunctionAnalysisManager& FAM);

    //
    // Produce vectorized instructions
    //
    bool
    vectorize(VectorizationInfo &vecInfo,
              llvm::FunctionAnalysisManager& FAM,
              llvm::ValueToValueMapTy * vecInstMap);

    PlatformInfo & getPlatformInfo() const { return platInfo; }

private:
    Config config;
    PlatformInfo & platInfo;
};


// implement all rv_* intrinsics
// this is necessary to make scalar functions with predicate intrinsics executable
// the SIMS semantics of the function will change if @scalar func used any mask intrinsics
bool lowerIntrinsics(llvm::Function & scalarFunc);
bool lowerIntrinsics(llvm::Module & mod);

bool cloneFunctionAndLowerIntrinsics(llvm::Function & kernel_func, llvm::Function & simd_kernel_func);

}

#endif // RV_RV_H
