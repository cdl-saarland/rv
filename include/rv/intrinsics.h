#ifndef RV_INTRINSICS_H
#define RV_INTRINSICS_H

#include "rv/vectorMapping.h"
#include <llvm/ADT/StringRef.h>

namespace llvm {
  class Function;
  class Module;
  class Value;
}

namespace rv {
  // TODO auto-generate this from intrinsics.def
  enum RVIntrinsic : int {
    Unknown = -1, // no recognized intrinsic

  // predicate intrinsics
    EntryMask = 0, // rv_entry_mask() returns the region entry mask (implicit function argument).
    All = 1, // rv_all(V) implies that all bits in the predicate V are set
    Any = 2, // rv_any(V) implies that at least one bit in the predicate V is set
    Ballot = 3, // rv_ballot(V) returns the boolean vector predicate V as bits in an integer value (movmsk)
    PopCount = 4, // rv_popcount(V) returns the number of set bits in the predicate V
    Index = 5, // prefix sum of mask vector (only defined where the mask is set)
    Mask = 6, // Gets the current execution mask
    Compact = 7, // rv_compact(V, M) returns the V compacted according to M

  // data intrinsics
    Extract = 100, // rv_extract(V, L) returns the L-th lane of V as a uniform value (lane broadcast)
    Insert = 101 , // rv_insert(V, L, X) returns the result of inserting the uniform value X into the L-th lane of V
    VecLoad = 102, // rv_load(V)
    VecStore = 103, // rv_store(V)
    Shuffle = 104, // rv_shuffle(V, S) returns the varying value V shifted by constant S
    Align = 105, // rv_align(V, C) informs RV that V has the alignment constant C
  };

  VectorMapping GetIntrinsicMapping(llvm::Function&, RVIntrinsic rvIntrin);

  llvm::StringRef GetIntrinsicName(RVIntrinsic id);
  bool IsIntrinsic(const llvm::Value& val, RVIntrinsic id);
  RVIntrinsic GetIntrinsicID(const llvm::Function&);
  RVIntrinsic GetIntrinsicID(const llvm::Value&);

  llvm::Function &DeclareIntrinsic(RVIntrinsic id, llvm::Module &);
}


#endif // RV_INTRINSICS_H
