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

#include "rv/region/Region.h"

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

Module*
createModuleFromFile(const std::string & fileName, LLVMContext & context);

Module*
createModuleFromBuffer(const char buffer[], size_t length, LLVMContext & context);

void
writeModuleToFile(const Module& mod, const std::string& fileName);

void
writeFunctionToFile(const Function& f, const std::string& fileName);

bool typesMatch(Type* t1, Type* t2);

// returns the minimal guaranteed (ptr) alignment for @V
// Otw, returns 0 if the alignment is unknown
unsigned
getBaseAlignment(const Value & V, const DataLayout &DL);

} // namespace rv


#endif /* _RVTOOLS_H */

