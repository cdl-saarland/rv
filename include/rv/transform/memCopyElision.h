#ifndef MEMCOPYELISION_H
#define MEMCOPYELISION_H

#include <cstdlib>
#include "rv/vectorizationInfo.h"
#include "rv/PlatformInfo.h"
#include "llvm/IR/IRBuilder.h"

namespace llvm {
  class Instruction;
  class DataLayout;
  class Type;
}


namespace rv {

// eliminate memcpy/mommov instructions that operate on divergent pointers
class
MemCopyElision {
  VectorizationInfo & vecInfo;
  const llvm::DataLayout & layout;
  bool IsDivergent(llvm::Instruction & inst) const;

  // request a base GEP for accessing @numBytes with flat GEP (or return nullptr)
  llvm::Value * deriveBase(llvm::Value * ptr, size_t numBytes);
  // derive a common aggregate type that aTy and bTy could be bitcase to too access up to @numBytes
  // will fail if aTy and bTy are not field aligned in the byte range [0, numBytes - 1]
  llvm::Type * deriveCommonType(llvm::Type * aTy, llvm::Type * bTy, size_t numBytes);

  // materialize a ptr with type @baseTy deriving from @ptrVal
  llvm::Value * createBaseGEP(llvm::Value * ptrVal, llvm::Type * baseTy, llvm::IRBuilder<> & builder);

  // implement a memcpy with load/store
  void lowerMemCopy(llvm::Value * aBase, llvm::Value * bBase, llvm::Type * commonTy, llvm::IRBuilder<> & builder, size_t numBytes);

public:
  MemCopyElision(PlatformInfo & platInfo, VectorizationInfo & _vecInfo)
  : vecInfo(_vecInfo)
  , layout(platInfo.getDataLayout())
  {}

  bool run();
};


}

#endif // MEMCOPYELISION_H

