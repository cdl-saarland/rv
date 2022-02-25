#ifndef RV_C_PASSES_H
#define RV_C_PASSES_H

#include "llvm-c/ExternC.h"
#include "llvm-c/Types.h"

LLVM_C_EXTERN_C_BEGIN

// Add RV's LoopVectorizer pass.
void RVAddLoopVectorizePass(LLVMPassManagerRef PM);

LLVM_C_EXTERN_C_END

#endif // RV_C_PASSES_H
