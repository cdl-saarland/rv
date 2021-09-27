#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Pass.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/IPO/PassManagerBuilder.h"
#include "rv/registerPasses.h"
#include "rv/passes.h"

using namespace llvm;

llvm::PassPluginLibraryInfo getRVPLUGPluginInfo() {
  return {LLVM_PLUGIN_API_VERSION, "RV", LLVM_VERSION_STRING,
          [](PassBuilder &PB) { rv::addConfiguredRVPasses(PB); }};
}

#ifndef LLVM_RVPLUG_LINK_INTO_TOOLS
extern "C" LLVM_ATTRIBUTE_WEAK ::llvm::PassPluginLibraryInfo
llvmGetPassPluginInfo() {
  return getRVPLUGPluginInfo();
}
#endif
