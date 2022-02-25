#include "rv-c/passes.h"

#include "rv/passes/LoopVectorizer.h"

void RVAddLoopVectorizePass(LLVMPassManagerRef PM) {
  llvm::unwrap(PM)->add(rv::createLoopVectorizerLegacyPass());
}
