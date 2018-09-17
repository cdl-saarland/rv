#ifndef RV_RVLINKING_H
#define RV_RVLINKING_H


#include <llvm/IR/GlobalValue.h>
#include <llvm/ADT/StringRef.h>

namespace llvm {
  class Function;
}

/// /brief Support for cross-module Function/GlobalVariable cloning.
namespace rv {
  llvm::Constant & cloneConstant(llvm::Constant& constVal, llvm::Module & cloneInto);
  llvm::GlobalValue & cloneGlobalIntoModule(llvm::GlobalValue &gv, llvm::Module &cloneInto);
  llvm::Function &cloneFunctionIntoModule(llvm::Function &func, llvm::Module &cloneInto, llvm::StringRef name);
}

#endif // RV_RVLINKING_H
