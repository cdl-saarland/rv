#ifndef RV_PASSES_H
#define RV_PASSES_H

#include "llvm/Pass.h"
#include "rv/config.h"

namespace rv {
  // Rv-based loop vectorizer pass
  llvm::FunctionPass *createLoopVectorizerPass();

  // vector IR polisher
  llvm::FunctionPass *createIRPolisherWrapperPass(Config config = Config());

  // Controlled Node Splitting (Irreducible loop normalization)
  llvm::FunctionPass *createCNSPass();
} // namespace rv


#endif
