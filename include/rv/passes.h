#ifndef RV_PASSES_H
#define RV_PASSES_H

#include "llvm/Pass.h"
#include "llvm/IR/LegacyPassManager.h"
#include "rv/config.h"

namespace rv {
  // Rv-based loop vectorizer pass
  llvm::FunctionPass *createLoopVectorizerPass();

  // vector IR polisher
  llvm::FunctionPass *createIRPolisherWrapperPass(Config config = Config());

  // Controlled Node Splitting (Irreducible loop normalization)
  llvm::FunctionPass *createCNSPass();

  // add RV's outer loop vectorizer and required passes to @PM
  void addOuterLoopVectorizer(llvm::legacy::PassManagerBase & PM, Config config = Config());
} // namespace rv


#endif
