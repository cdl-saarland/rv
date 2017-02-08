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
#include <rv/PlatformInfo.h>

llvm::Type *getVectorType(llvm::Type *type, unsigned width);

llvm::Value *createContiguousVector(unsigned width, Type *type, int start, int stride);

llvm::Value *getConstantVectorPadded(unsigned width, Type *type, std::vector<unsigned> &values);

llvm::Value *getPointerOperand(llvm::Instruction *instr);


llvm::BasicBlock *createCascadeBlocks(llvm::Function *insertInto, unsigned vectorWidth,
                                      std::vector<llvm::BasicBlock *> &condBlocks,
                                      std::vector<llvm::BasicBlock *> &maskedBlocks);

bool isSupportedOperation(llvm::Instruction *const inst);

bool isStructAccess(llvm::Value * const address);

#endif //NATIVE_UTILS_H
