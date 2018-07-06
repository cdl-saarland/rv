#ifndef RV_INTRINSICS_H
#define RV_INTRINSICS_H

#include <llvm/ADT/StringRef.h>

namespace llvm {
  class Function;
  class Value;
}

namespace rv {
  enum RVIntrinsic : int {
    Unknown = -1, // no recognized intrinsic

  // predicate intrinsics
    All = 0, // rv_all(V) implies that all bits in the predicate V are set
    Any = 1, // rv_any(V) implies that at least one bit in the predicate V is set
    Ballot = 2, // rv_ballot(V) returns the boolean vector predicate V as bits in an integer value (movmsk)
    PopCount = 3, // rv_popcount(V) returns the number of set bits in the predicate V
    Index = 4, // prefix sum of mask vector (only defined where the mask is set)

  // data intrinsics
    Extract = 100, // rv_extract(V, L) returns the L-th lane of V as a uniform value (lane broadcast)
    Insert = 101 , // rv_insert(V, L, X) returns the result of inserting the uniform value X into the L-th lane of V
    VecLoad = 102, // rv_load(V)
    VecStore = 103, // rv_store(V)
    Shuffle = 104, // rv_shuffle(V, S) returns the varying value V shifted by constant S
    Align = 105 // rv_align(V, C) informs RV that V has the alignment constant C
  };

  llvm::StringRef GetIntrinsicName(RVIntrinsic id);
  bool IsIntrinsic(const llvm::Value& val, RVIntrinsic id);
  RVIntrinsic GetIntrinsicID(const llvm::Function&);
  RVIntrinsic GetIntrinsicID(const llvm::Value&);
}


#endif // RV_INTRINSICS_H
