#ifndef RV_ANALYSIS_UNDEADMASKANALYSIS_H
#define RV_ANALYSIS_UNDEADMASKANALYSIS_H

#include <llvm/IR/Value.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Dominators.h>

#include <map>

namespace rv {

class VectorizationInfo;

// this analysis is run by the backend to determine whether there is at least a single active lean when a piece of code is executed.
// if (rv_any(p && q)) {
//   if (p) *uniformPtr = <uniform value>
// }
//
// without undead mask analysis:
// if (rv_any(p && q)) {
//   if (rv_any(p)) {
//     *uniformPtr = <uniform value>
//   }
// }
//
// with undead mask analysis:
// if (rv_any(p && q)) {
//   *uniformPtr = <uniform value>
// }
class UndeadMaskAnalysis {
  const llvm::DominatorTree & domTree;
  VectorizationInfo & vecInfo;

  // whether @lhs ^ @lhsNegated implies @rhs ^ @rhsNegated
  // (returns false if answer unknown)
  bool implies(const llvm::Value & lhs, bool lhsNegated, const llvm::Value & rhs, bool rhsNegated);
  std::map<const llvm::Value*, const llvm::BasicBlock*> liveDominatorMap;

public:
  UndeadMaskAnalysis(const llvm::DominatorTree & domTree, VectorizationInfo & vecInfo);
  bool isUndead(const llvm::Value & mask, const llvm::BasicBlock & where);
};

} // namespace rv

#endif // RV_ANALYSIS_UNDEADMASKANALYSIS_H
