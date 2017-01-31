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

using namespace llvm; // FIXME no no!

namespace rv {

class VectorizationInfo;
class MaskAnalysis;

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
    VectorizerInterface(PlatformInfo & _platform);
    //~VectorizerInterface();

    /*
     * Analyze properties of the scalar function that are needed later in transformations
     * towards its SIMD equivalent.
     *
     * This expects initial information about arguments to be set in the VectorizationInfo object
     * (see VectorizationInfo).
     */
    void analyze(VectorizationInfo& vectorizationInfo,
                 const CDG& cdg,
                 const DFG& dfg,
                 const LoopInfo& loopInfo,
                 const PostDominatorTree& postDomTree,
                 const DominatorTree& domTree);

    /*
     * Analyze mask values needed to mask certain values and preserve semantics of the function
     * after its control flow is linearized where needed.
     */
    MaskAnalysis* analyzeMasks(VectorizationInfo& vectorizationInfo, const LoopInfo& loopinfo);

    /*
     * Materialize the mask information.
     */
    bool generateMasks(VectorizationInfo& vectorizationInfo,
                       MaskAnalysis& maskAnalysis,
                       const LoopInfo& loopInfo);

    /*
     * Linearize divergent regions of the scalar function to preserve semantics for the
     * vectorized function
     */
    bool linearizeCFG(VectorizationInfo& vectorizationInfo,
                      MaskAnalysis& maskAnalysis,
                      LoopInfo& loopInfo,
                      DominatorTree& domTree);

    /*
     * Produce vectorized instructions
     */
    bool
    vectorize(VectorizationInfo &vecInfo, const DominatorTree &domTree);

    /*
     * Ends the vectorization process on this function, removes metadata and
     * writes the function to a file
     */
    void finalize(VectorizationInfo & vecInfo);

private:
    PlatformInfo platInfo;

    void addPredicateIntrinsics();
};


   // implement all rv_* predicate intrinsics in function
   // this is necessary to make scalar functions with predicate intrinsics executable
   // the SIMS semantics of the function will change if @scalar func used any mask intrinsics
  void lowerPredicateIntrinsics(Function & scalarFunc);
  void lowerPredicateIntrinsics(Module & mod);
}

#endif // RV_RV_H
