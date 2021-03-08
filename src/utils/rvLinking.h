#ifndef RV_RVLINKING_H
#define RV_RVLINKING_H


#include <llvm/IR/GlobalValue.h>
#include <llvm/ADT/StringRef.h>

namespace llvm {
  class Function;
}

/// /brief Support for cross-module Function/GlobalVariable cloning.
namespace rv {
  // Copy \p GV into \p Mod and return the copy.
  using LinkerCallback = std::function<llvm::Value*(llvm::GlobalValue&, llvm::Module&)>;

  // Copy into module - return reference to existing GlobalValue if there is one.
  llvm::Value & cloneConstant(llvm::Constant& constVal, llvm::Module & cloneInto, LinkerCallback LCB = nullptr);
  llvm::GlobalValue & cloneGlobalIntoModule(llvm::GlobalValue &gv, llvm::Module &cloneInto, LinkerCallback LCB = nullptr);
  llvm::Function &cloneFunctionIntoModule(llvm::Function &func, llvm::Module &cloneInto, llvm::StringRef name, LinkerCallback LCB = nullptr);
}

#endif // RV_RVLINKING_H
