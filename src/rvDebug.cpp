#include "rv/rvDebug.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/Debug.h"

void rv::Dump(const llvm::Value & val) {
  val.print(llvm::dbgs(), true);
}

void rv::Dump(const llvm::Module & mod) {
  mod.print(llvm::dbgs(), nullptr, false, true);
}
