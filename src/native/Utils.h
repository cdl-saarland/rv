//===- Utils.h -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//

#ifndef NATIVE_UTILS_H
#define NATIVE_UTILS_H

#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Value.h>

#include <vector>

llvm::Type *getVectorType(llvm::Type *type, unsigned width);

llvm::Value *createContiguousVector(unsigned width, llvm::Type *type, int start = 0);

/***
 * Create blocks needed for an if-cascade. Condition blocks are inserted into condBlocks, the masked blocks into
 * maskedBlocks. Pointer to return block is returned.
 */
llvm::BasicBlock *createCascadeBlocks(llvm::Function *insertInto, unsigned vectorWidth,
                                      std::vector<llvm::BasicBlock *> &condBlocks,
                                      std::vector<llvm::BasicBlock *> &maskedBlocks);

#endif //NATIVE_UTILS_H
