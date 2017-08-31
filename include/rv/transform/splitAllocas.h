#ifndef RV_TRANSFORM_SPLITALLOCAS_H
#define RV_TRANSFORM_SPLITALLOCAS_H

#include <llvm/IR/Value.h>
#include <llvm/IR/IRBuilder.h>

#include <unordered_map>
#include <memory>

#include "rv/vectorShape.h"

namespace llvm {
  class Instruction;
  class AllocaInst;
  class Type;
}

namespace rv {

class VectorizationInfo;

class SplitAllocas {
  VectorizationInfo & vecInfo;

  struct AllocaTree {
    typedef std::vector<std::unique_ptr<AllocaTree>> Children;

    llvm::AllocaInst * leafAlloca;
    Children children;
    llvm::Type * type;

    AllocaTree(llvm::Type * _type, llvm::AllocaInst * _leafAlloca)
      : leafAlloca(_leafAlloca), type(_type)
    {}

    AllocaTree(llvm::Type * _type, Children&& _children)
      : leafAlloca(nullptr), children(std::move(_children)), type(_type)
    {}

    void store(llvm::IRBuilder<> & builder, VectorizationInfo &, VectorShape, llvm::Value *);
    llvm::Value * load(llvm::IRBuilder<> & builder, VectorizationInfo &, VectorShape);
  };

  /// recursively analyses every use of an alloca and determine if it can be split
  /// allocas that are used by instructions other than loads/stores/geps cannot be split
  bool analyseUses(llvm::Instruction *, llvm::Type *);
  /// creates a sequence of allocas to replace the original alloca instruction
  std::unique_ptr<AllocaTree> createAllocaTree(llvm::AllocaInst * allocaInst, llvm::Type *, VectorShape);
  /// recursively splits the uses of an alloca
  void splitUses(llvm::Instruction *, AllocaTree *, VectorShape);

public:
  SplitAllocas(VectorizationInfo & _vecInfo);

  bool run();
};

} // namespace rv

#endif // RV_TRANSFORM_SPLITALLOCAS_H
