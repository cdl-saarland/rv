//===- Utils.cpp -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @author montada

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Constants.h>
#include "Utils.h"

using namespace llvm;

Type *getVectorType(Type *type, unsigned width) {
    if (type->isVoidTy())
        return type;
    else
        return VectorType::get(type, width);
}

Value *createContiguousVector(unsigned width, Type *type, int start) {
    Constant *constants[width];
    for (unsigned i = 0; i < width; ++i) {
        constants[i] = ConstantInt::get(type, i + start);
    }
    return ConstantVector::get(ArrayRef<Constant *>(constants, width));
}

BasicBlock *createCascadeBlocks(Function *insertInto, unsigned vectorWidth,
                                std::vector<BasicBlock *> &condBlocks,
                                std::vector<BasicBlock *> &maskedBlocks) {
    BasicBlock *cond, *load;
    for (unsigned lane = 0; lane < vectorWidth; ++lane) {
        cond = BasicBlock::Create(insertInto->getContext(), "cascade_cond_" + std::to_string(lane),
                                  insertInto);
        load = BasicBlock::Create(insertInto->getContext(), "cascade_masked_" + std::to_string(lane),
                                  insertInto);
        condBlocks.push_back(cond);
        maskedBlocks.push_back(load);
    }
    return BasicBlock::Create(insertInto->getContext(), "cascade_end", insertInto);
}
