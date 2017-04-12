#ifndef RV_COSTMODEL_H
#define RV_COSTMODEL_H

#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Analysis/ScalarEvolution.h>
#include <llvm/Analysis/TargetTransformInfo.h>
#include <llvm/Analysis/DemandedBits.h>
#include <llvm/ADT/MapVector.h>
#include <llvm/Analysis/LoopAccessAnalysis.h>
#include <rv/vectorizationInfo.h>

#undef ENABLE_VF_AUTO_SELECT

namespace llvm {

/// LoopVectorizationCostModel - estimates the expected speedups due to
/// vectorization.
/// In many cases vectorization is not profitable. This can happen because of
/// a number of reasons. In this class we mainly attempt to predict the
/// expected speedup/slowdowns due to the supported instruction set. We use the
/// TargetTransformInfo to query the different backends for the cost of
/// different operations.
class LoopVectorizationCostModel {
public:
  LoopVectorizationCostModel(const Loop* L,
                             ScalarEvolution* SE,
                             LoopInfo* LI,
                             const rv::VectorizationInfo& VecInfo,
                             const TargetTransformInfo& TTI,
                             const TargetLibraryInfo* TLI,
                             SmallPtrSetImpl<const Value*>& ValuesToIgnore)

          : TheLoop(L), SE(SE), LI(LI), VecInfo(VecInfo), TTI(TTI), TLI(TLI),
            ValuesToIgnore(ValuesToIgnore) { }

#if ENABLE_VF_AUTO_SELECT

  /// Information about vectorization costs
  struct VectorizationFactor {
    unsigned Width; // Vector width with best cost
    unsigned Cost; // Cost of the loop with that width
  };

  /// \return The most profitable vectorization factor and the cost of that VF.
  /// This method checks every power of two up to VF. If UserVF is not ZERO
  /// then this vectorization factor will be selected if vectorization is
  /// possible.
  VectorizationFactor selectVectorizationFactor(bool OptForSize);

#endif

  /// Returns the expected execution cost. The unit of the cost does
  /// not matter because we use the 'cost' units to compare different
  /// vector widths. The cost that is returned is *not* normalized by
  /// the factor width.
  int expectedCost(unsigned VF);

private:
  /// Returns the execution time cost of an instruction for a given vector
  /// width. Vector width of one means scalar.
  int getInstructionCost(Instruction* I, unsigned VF);

public:
  /// Map of scalar integer values to the smallest bitwidth they can be legally
  /// represented as. The vector equivalents of these values should be truncated
  /// to this type.
  MapVector<Instruction*, uint64_t> MinBWs; // FIXME right now this map is empty
                                            // FIXME (why) do we need this?

  /// The loop that we evaluate.
  const Loop* TheLoop;
  /// Scev analysis.
  ScalarEvolution* SE;
  /// Loop Info analysis.
  LoopInfo* LI;
  /// Vectorization info.
  const rv::VectorizationInfo& VecInfo;
  /// Vector target information.
  const TargetTransformInfo& TTI;
  /// Target Library Info.
  const TargetLibraryInfo* TLI;
  // Values to ignore in the cost model.
  const SmallPtrSetImpl<const Value*>& ValuesToIgnore;
};

}

#endif //RV_COSTMODEL_H
