//===- rvTools.h ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @author karrenberg
//

#ifndef _RVTOOLS_H
#define _RVTOOLS_H

#include <string>
#include <set>

#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/SmallPtrSet.h>

#include "rv/utils/functionInfoMap.h"
#include "rv/vectorizationInfo.h"

#include "analysis/analysisCfg.h"

// Forward declarations.

namespace llvm {
class Value;
class Module;
class Function;
class BasicBlock;
class Instruction;
class Type;
class Constant;
class LLVMContext;
class CallInst;
class Loop;
class LoopInfo;
}

namespace rv {
  class RVInfo;
  class ValueInfoMap;
  class VectorizationInfo;
}

using namespace llvm;

typedef std::set<BasicBlock*> BlockSet;
typedef std::set<BasicBlock*> ConstBlockSet;

namespace rv {

bool
HasVaryingBranch(const llvm::BasicBlock & block, const VectorizationInfo& vecInfo);

bool
IsPrimitiveType(const llvm::Type & type);

// returns true, if this is a pointer to a primitive type
bool
IsFlatPointer(const llvm::Type & type);

// Mutate type of original value and
// replace its uses by the new value.
void
uncheckedReplaceAllUsesWith(Value* value, Value* with);

// This function defines what we consider matching types
// in terms of uniform/varying.
bool
typesMatch(Type* t1, Type* t2);

unsigned
getVectorizationFactor(const Type& type);

bool
typesMatchPumped(const Type& t1, const Type& t2);

// Find out which parameters of scalarFun remain scalar in the simd function.
void
findUniformArguments(const Function&          scalarFn,
                     const Function&          vectorFn,
                     SmallVector<bool, 4>&    uniformArgs,
                     const rv::ValueInfoMap* valueInfoMap = nullptr,
                     const int                maskIndex = -1);

bool
hasUniformReturn(const Function& scalarFn,
                 const Function& vectorFn);

bool
returnsVoidPtr(const Instruction& inst);

bool
isVectorizableInst(const Instruction& inst);

bool
isVectorizableNonDerivedType(const Type& type);

bool
isVectorizableType(const Type& type);

bool
isVectorizedType(const Type& type);

bool
hasNestedPointer(const Type& type, const bool inNest=false);

// Create SIMD type from scalar type.
// -> only 32bit-float, integers <= 32bit, pointers, arrays and structs allowed
// -> no scalar datatypes allowed
// -> no pointers to pointers allowed
// TODO: i8* should not be transformed to <4 x i8>* !
Type*
vectorizeSIMDType(Type* oldType, const unsigned vectorizationFactor);

// broadcast &value to a vector type (if necessary)
// if this is a constant, vectorize that
Value*
InsertBroadcast(Value * value, unsigned simdFactor, Instruction & insertBefore);

Value *
createPointerCast(Value * pointer, unsigned factor, Instruction * insertBefore);

// Returns the element type for a splitting operation.
// The input type is required to be vectorized, meaning that e.g. on
// leaf level of a struct, the underlying type is a vector (vector)
// and never a scalar.
// NOTE: This basically applies the vectorization rules in reverse.
// EXAMPLE: { <4 x float>, [ <4 x float>, <4 x float> ], <4 x int> }
//          -> { float, [ float, float ], int }
Type*
getScalarFromVectorizedType(Type* type);

bool
mayHaveSideEffects(const Instruction&     inst,
                   const FunctionInfoMap* functionInfoMap);

// Create a new constant from the value of 'c' that
// requires at most 32 bits. If the value is too large
// to be stored in 32 bit or if the type of the constant
// is a non-numerical type, the function throws an exception.
Constant*
getMax32BitConstant(Constant* c, LLVMContext& context);

Instruction*
createDummy(Type* type, Instruction* insertBefore);

Instruction*
createNoOp(Type* type, Instruction* insertBefore);

// Returns the unique return block of function 'f'.
// We rely on the ReturnUnifier pass and thus terminate as soon as we
// have found a return.
void
findReturnBlocks(Function&                    f,
                 SmallPtrSet<BasicBlock*, 2>& returnBlocks);
void
findReturnBlocks(const Function&                    f,
                 SmallPtrSet<const BasicBlock*, 2>& returnBlocks);

unsigned
getNumIncomingEdges(const BasicBlock& block);

BasicBlock*
getExitBlock(const BasicBlock* exitingBlock, const LoopInfo& loopInfo);

void
getExitingBlocks(BasicBlock*                  exitBlock,
                 const LoopInfo&              loopInfo,
                 SmallVector<BasicBlock*, 2>& exitingBlocks);

Loop*
getInnermostExitedLoop(const BasicBlock& exitBlock,
                       const LoopInfo&   loopInfo);

Loop*
getOutermostExitedLoop(const BasicBlock& exitBlock,
                       const LoopInfo&   loopInfo);

Loop*
getCommonLoop(Loop* loopA, Loop* loopB);

const Loop*
getCommonLoop(const Loop* loopA, const Loop* loopB);

Loop*
getCommonLoop(const SmallPtrSet<BasicBlock*, 2>& blocks,
              const LoopInfo&                    loopInfo);

bool
isExitOfDivergentLoop(const BasicBlock&             exitBlock,
                      const LoopInfo&               loopInfo,
                      const rv::VectorizationInfo& vecInfo);

bool
isExitOfDivergentLoop(const BasicBlock&             exitingBlock,
                      const BasicBlock&             exitBlock,
                      const LoopInfo&               loopInfo,
                      const rv::VectorizationInfo& vecInfo);

bool
isReachable(const BasicBlock* target,
            const BasicBlock* source,
            const BasicBlock* doNotTraverse=nullptr,
            const Loop*       doNotLeaveLoop=nullptr,
            const Loop*       doNotUseBackEdge=nullptr,
            const bool        doNotUseAnyBackEdge=false,
            const LoopInfo*   loopInfo=nullptr);

typedef SmallVector<const BasicBlock*, 16> PathType;
typedef SmallVector<PathType*, 16> PathVecType;

void DumpPath(const PathType & path);

#ifdef RV_ENABLE_LEGACY_API
bool
collectPaths(const BasicBlock*                   target,
             const BasicBlock*                   source,
             PathType&                           currentPath,
             PathVecType&                        paths,
             const bool                          ignoreOuterLoops,
             const LoopInfo&                     loopInfo,
             SmallPtrSet<const BasicBlock*, 16>& visitedBlocks,
             SmallPtrSet<const BasicBlock*, 16>& unreachableBlocks);

void
collectLoopPaths(const BasicBlock*                   target,
                 const BasicBlock*                   source,
                 const Loop*                         loop,
                 PathType&                           currentPath,
                 PathVecType&                        paths,
                 SmallPtrSet<const BasicBlock*, 16>& visitedBlocks);
#endif

Loop*
findNestedLoopOfInst(Loop* parentLoop, Instruction* inst);

Loop*
findNextNestedLoopOfExit(Loop*       loop,
                         BasicBlock* exitingBlock);

Loop*
findTopLevelLoopOfExit(Loop*           loop,
                       BasicBlock*     exitingBlock,
                       BasicBlock*     exitBlock,
                       const LoopInfo& loopInfo);

Instruction*
generateAlignedAlloc(Type*          targetType,
                     const RVInfo& mInfo,
                     Instruction*   insertBefore);


Module*
createModuleFromFile(const std::string & fileName, LLVMContext & context);

void
writeModuleToFile(const Module& mod, const std::string& fileName);

void
writeFunctionToFile(const Function& f, const std::string& fileName);

// insert print statement that prints 'value' preceeded by 'DEBUG: `message`'
// example what can be generated:
// declare i32 @printf(i8* noalias nocapture, ...) nounwind
// @.str1 = private constant [19 x i8] c"DEBUG: indexA: %d\0A\00", align 1 ; <[19 x i8]*> [#uses=1]
// %printf1 = tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([19 x i8]* @.str1, i64 0, i64 0), i32 %call) ; <i32> [#uses=0]
// Usage Example:
// insertPrintf("else-block executed! - pointerIdx: ", pointerIdx, true, (*elseBB)->getTerminator());
CallInst*
insertPrintf(const std::string& message,
             Value*             value,
             const bool         endLine,
             Instruction*       insertBefore);

} // namespace rv


#endif /* _RVTOOLS_H */

