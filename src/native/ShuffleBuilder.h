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
    bool cropped;
    
    void prepareCroppedVector(llvm::IRBuilder<> &builder);

  public:
    ShuffleBuilder(unsigned vectorWidth) : vectorWidth(vectorWidth), inputVectors(), cropped(false) {}
    ShuffleBuilder(std::vector<llvm::Value *> &sources, unsigned vectorWidth) : vectorWidth(vectorWidth),
                                                                               inputVectors(sources), cropped(false) {}

    void add(llvm::Value *vector);
    void add(std::vector<llvm::Value *> &sources);
    llvm::Value *shuffleFromInterleaved(llvm::IRBuilder<> &builder, unsigned stride, unsigned start);
    llvm::Value *shuffleToInterleaved(llvm::IRBuilder<> &builder, unsigned stride, unsigned start);
    llvm::Value *append(llvm::IRBuilder<> &builder);
    llvm::Value *extractVector(llvm::IRBuilder<> &builder, unsigned index, unsigned offset);
  };
}

#endif //RV_SHUFFLEBUILDER_H
