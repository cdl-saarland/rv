#ifndef RV_TRANSFORM_REDTOOLS_H
#define RV_TRANSFORM_REDTOOLS_H

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instruction.h>
#include "rv/analysis/reductionAnalysis.h"

namespace rv {

// materialize a single instance of firstArg [[RedKind~OpCode]] secondArg
llvm::Instruction& CreateReduce(llvm::IRBuilder<> & builder, llvm::Value & firstArg, llvm::Value & secondArg);

// reduce the vector @vectorVal to a scalar value (using redKind)
llvm::Value & CreateVectorReduce(llvm::IRBuilder<> & builder, RedKind redKind, llvm::Value & vectorVal);

}

#endif // RV_TRANSFORM_REDTOOLS_H
