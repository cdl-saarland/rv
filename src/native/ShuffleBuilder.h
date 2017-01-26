//===- ShuffleBuilder.h -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @author montada

#ifndef RV_SHUFFLEBUILDER_H
#define RV_SHUFFLEBUILDER_H

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Value.h>

#include <vector>

namespace native {
  class ShuffleBuilder {
    unsigned vectorWidth;
    std::vector<llvm::Value *> inputVectors;

  public:
    ShuffleBuilder(unsigned vectorWidth) : vectorWidth(vectorWidth), inputVectors() {}
    ShuffleBuilder(std::vector<llvm::Value *> sources, unsigned vectorWidth) : vectorWidth(vectorWidth),
                                                                               inputVectors(sources) {}

    void add(llvm::Value *vector);
    llvm::Value *shuffleFromInterleaved(llvm::IRBuilder<> &builder, unsigned stride, unsigned start);
    llvm::Value *shuffleToInterleaved(llvm::IRBuilder<> &builder, unsigned stride, unsigned start);
  };
}

#endif //RV_SHUFFLEBUILDER_H
