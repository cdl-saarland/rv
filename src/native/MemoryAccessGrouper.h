//===- MemoryAccessGrouper.h -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @author montada

#ifndef RV_MEMORYACCESSGROUPER_H
#define RV_MEMORYACCESSGROUPER_H

#include <llvm/Analysis/ScalarEvolution.h>

namespace native {
  class MemoryGroup {
    unsigned topIdx;
    std::vector<const llvm::SCEV *> elements;

  public:
    MemoryGroup(const llvm::SCEV *scev);
    MemoryGroup();

    void insert(const llvm::SCEV *scev, int offset);
    const llvm::SCEV *operator[](int i) const { return elements[i]; }
    unsigned size() const { return topIdx; }
  };

  class MemoryAccessGrouper {
    llvm::ScalarEvolution &SE;
    unsigned laneByteSize;
    bool getConstantOffset(const llvm::SCEV *a, const llvm::SCEV *b, int &offset);

    llvm::Type *laneFloatTy;
    llvm::Type *laneIntTy;

  public:
    std::vector<MemoryGroup> memoryGroups;

    MemoryAccessGrouper(llvm::ScalarEvolution &SE, unsigned laneByteSize);
    const llvm::SCEV *add(llvm::Value *addrVal);
  };
}



#endif //RV_MEMORYACCESSGROUPER_H
