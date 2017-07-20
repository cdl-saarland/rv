#ifndef RV_TRANSFORM_REDTOOLS_H
#define RV_TRANSFORM_REDTOOLS_H

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instruction.h>
#include "rv/analysis/reductionAnalysis.h"

namespace rv {

// materialize a single instance of firstArg [[RedKind~OpCode]] secondArg
llvm::Instruction& CreateReductInst(llvm::IRBuilder<> & builder, RedKind redKind, llvm::Value & firstArg, llvm::Value & secondArg);

// reduce the vector @vectorVal to a scalar value (using redKind)
llvm::Value & CreateVectorReduce(llvm::IRBuilder<> & builder, RedKind redKind, llvm::Value & vectorVal, llvm::Value * initVal=nullptr);

// if laneOffset is >= 0 create an extract from that offset, if laneOffset < 0 add the vector width first
// will return @vecVal if it is not a vector (uniform value)
llvm::Value & CreateExtract(llvm::IRBuilder<> & builder, llvm::Value & vecVal, int laneOffset);

}

#endif // RV_TRANSFORM_REDTOOLS_H
