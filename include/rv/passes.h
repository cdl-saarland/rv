#ifndef RV_PASSES_H
#define RV_PASSES_H

#include "llvm/Pass.h"
#include "llvm/IR/LegacyPassManager.h"
#include "rv/config.h"

namespace rv {


  // add all passes of RV to the pass pipeline PM.
  void addRVPasses(llvm::legacy::PassManagerBase & PM);


// fine-grained pass adding
  // RV-based loop vectorizer pass
  llvm::FunctionPass *createLoopVectorizerPass();

  // Whole-Function Vectorizer pass
  llvm::ModulePass *createWFVPass();

  // vector IR polisher
  llvm::FunctionPass *createIRPolisherWrapperPass(Config config = Config());

  // Controlled Node Splitting (Irreducible loop normalization)
  llvm::FunctionPass *createCNSPass();


  // add normalization passes required by RV (BEFORE)
  void addPreparatoryPasses(llvm::legacy::PassManagerBase & PM);

  // add RV's outer loop vectorizer and required passes to @PM
  void addOuterLoopVectorizer(llvm::legacy::PassManagerBase & PM);

  // add RV's whole function and required passes to @PM
  void addWholeFunctionVectorizer(llvm::legacy::PassManagerBase & PM);

  // add cleanup passes to run after RV (AFTER)
  void addCleanupPasses(llvm::legacy::PassManagerBase & PM);
} // namespace rv


#endif
