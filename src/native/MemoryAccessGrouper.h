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
#include <llvm/Analysis/MemoryDependenceAnalysis.h>

namespace rv {
  class MemoryGroup {
    unsigned topIdx;
    std::vector<const llvm::SCEV *> elements;

  public:
    MemoryGroup(const llvm::SCEV *scev);
    MemoryGroup();

    void insert(const llvm::SCEV *scev, int offset);
    const llvm::SCEV *operator[](int i) const { return elements[i]; }
    unsigned size() const { return topIdx; }
    std::vector<const llvm::SCEV *>::iterator begin() { return elements.begin(); }
    std::vector<const llvm::SCEV *>::iterator end() { return elements.end(); }

    void print(llvm::raw_ostream &) const;
    void dump() const;
  };

  class MemoryAccessGrouper {
    llvm::ScalarEvolution &SE;
    unsigned laneByteSize;
    bool getConstantOffset(const llvm::SCEV *a, const llvm::SCEV *b, int &offset);

  public:
    std::vector<MemoryGroup> memoryGroups;

    // relaxed diffing
    bool getConstantDiff(const llvm::SCEV * A, const llvm::SCEV * B, int64_t & oDelta);

    // relaxed equality check
    bool equals(const llvm::SCEV * A, const llvm::SCEV * B);

    MemoryAccessGrouper(llvm::ScalarEvolution &SE, unsigned laneByteSize);
    const llvm::SCEV *add(llvm::Value *addrVal);
    const MemoryGroup & getMemoryGroup(const llvm::SCEV *scev);
  };

  class InstructionGroup {
    bool isStoreGroup;
    llvm::Type *groupType;
    std::vector<llvm::Instruction *> elements;
    std::vector<llvm::Instruction *> passedMemoryAndCallInstructions;

  public:
    InstructionGroup(llvm::Instruction *element);
    InstructionGroup();

    bool insert(llvm::Instruction *element, llvm::MemoryDependenceResults &MDR);
    unsigned long size() const { return elements.size(); }

    std::vector<llvm::Instruction *>::iterator begin();
    std::vector<llvm::Instruction *>::iterator end();
  };

  class InstructionGrouper {
  public:
    std::vector<InstructionGroup> instructionGroups;

    void add(llvm::Instruction *instr, llvm::MemoryDependenceResults &MDR);
    void clearAll();
    bool empty();
    const InstructionGroup & getInstructionGroup(llvm::Instruction *instr);
  };
}



#endif //RV_MEMORYACCESSGROUPER_H
