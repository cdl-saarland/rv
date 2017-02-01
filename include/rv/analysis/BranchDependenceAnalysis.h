#ifndef _RV_BRANCHDEPENDENCEANALYSIS_H_
#define _RV_BRANCHDEPENDENCEANALYSIS_H_

//===- BranchDependenceAnalysis.cpp ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//

#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/SmallPtrSet.h>
#include "rv/analysis/DFG.h"

namespace llvm {
  class Function;
  class BasicBlock;
  class TerminatorInst;
}

using llvm::DenseMap;
using ConstBlockSet = llvm::SmallPtrSet<const llvm::BasicBlock*, 4>;

namespace rv {

/// BranchDependenceAnalysis
///
/// This is an analysis to be used in the context of SIMD/GPU execution of a function.
/// It enables the VectorizationAnalysis to correctly propagate divergent control from branches to phi nodes.
///
/// In the SPMD setting a group of threads executes a function in bulk-synchornouous fashion.
/// For every instruction each thread may see the same result (uniform value) or a different result (varying/divergent value).
/// If a varying instruction computes a branch condition control among the threads may diverge (p in the example).
/// If phi nodes are dependent on such a divergent branch the phis may receive values from different incoming blocks at once (phi node x).
/// The phis become divergent even if the incoming values per predecessor are uniform values.
///
/// if (p) {
//    x0 = 1
//  } else {
//    x1 = 2
//  }
//  C: x = phi [x0, x1]
///
/// The analysis result maps every branch to a set of basic blocks whose phi nodes will become varying if the branch is varying.
/// This is directly used by the VectorizationAnalysis to propagate control-induced value divergence.
///
class BranchDependenceAnalysis {
  DenseMap<const llvm::TerminatorInst*, ConstBlockSet> effectedBlocks;
  const llvm::CDG & cdg;
  const llvm::DFG & dfg;

public:
  BranchDependenceAnalysis(llvm::Function & F, const llvm::CDG & cdg, const llvm::DFG & dfg);

  /// \brief returns the set of blocks whose PHI nodes become divergent if @branch is divergent
  const ConstBlockSet & getEffectedBlocks(const llvm::TerminatorInst & term) const {
    auto it = effectedBlocks.find(&term);
    assert(it != effectedBlocks.end());
    return it->second;
  }
};

} // namespace rv

#endif // _RV_BRANCHDEPENDENCEANALYSIS_H_
