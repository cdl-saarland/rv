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
#include <llvm/IR/BasicBlock.h>

#include "rv/vectorizationInfo.h"

#include "rv/Region/Region.h"

// Forward declarations.

namespace llvm {
  class Value;
  class Module;
  class Function;
  class Instruction;
  class Type;
  class Constant;
  class LLVMContext;
  class CallInst;
  class Loop;
  class LoopInfo;
}

namespace rv {
  class ValueInfoMap;
  class VectorizationInfo;
}

using namespace llvm;

namespace rv {

void
getExitingBlocks(BasicBlock*                  exitBlock,
                      const LoopInfo&              loopInfo,
                      SmallVector<BasicBlock*, 2>& exitingBlocks);

bool
returnsVoidPtr(const Instruction& inst);

Loop*
findNestedLoopOfInst(Loop* parentLoop, Instruction* inst);

Loop*
findNextNestedLoopOfExit(Loop*       loop,
                         BasicBlock* exitingBlock);

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

Loop*
findTopLevelLoopOfExit(Loop*           loop,
                       BasicBlock*     exitingBlock,
                       BasicBlock*     exitBlock,
                       const LoopInfo& loopInfo);

bool typesMatch(Type* t1, Type* t2);

// returns the minimal guaranteed (ptr) alignment for @V
// Otw, returns 0 if the alignment is unknown
unsigned
getBaseAlignment(const Value & V, const DataLayout &DL);

} // namespace rv


#endif /* _RVTOOLS_H */

