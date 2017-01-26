//===- ShuffleBuilder.cpp -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @author montada

#include "ShuffleBuilder.h"
#include "Utils.h"

using namespace native;
using namespace llvm;

void ShuffleBuilder::add(Value *vector) {
  inputVectors.push_back(vector);
}

llvm::Value *ShuffleBuilder::buildShuffle(llvm::IRBuilder<> &builder, unsigned stride, unsigned start) {
  // use a loop that builds shuffles until all input vectors have been used
  // shuffles are build like this: take the last shuffle and the next input vector and the shuffle mask
  // as shuffle mask, fixed positions from last shuffle, then all indices of form start + k * stride that are inside
  // the second argument vector. the shuffle that is build becomes the new last shuffle
  // initialize like this: last shuffle = inputVectors[0], next input vector = inputVectors[1],
  // shuffleMask = <[start + k * stride](=m1)> for all m1 < vectorWidth
  // lastly, return lastShuffle

  assert(inputVectors.size() >= 2 && "not enough input vectors to build shuffle");
  Value *lastShuffle = inputVectors[0];
  Value *nextInput;
  std::vector<unsigned> shuffleMask;
  unsigned inputVectorIndex = 1;
  unsigned indexLimit = vectorWidth * 2;
  unsigned index = start; // is valid since start >= 0

  // index init
  while (index < vectorWidth) {
    shuffleMask.push_back(index);
    index += stride;
  }

  do {
    nextInput = inputVectors[inputVectorIndex];

    assert(index >= vectorWidth && index < vectorWidth * 2 && "index not inside 2nd vector!");

    // index is now inside the 2nd vector. compute all indices that lie inside
    while (index < indexLimit) {
      shuffleMask.push_back(index);
      index += stride;
    }
    unsigned undefIndex = static_cast<unsigned>(shuffleMask.size());

    // build a shuffle and init next iteration (except nextInput to avoid out-of-bounds access
    Value *shuffleMaskVector = getConstantVectorPadded(vectorWidth, builder.getInt32Ty(), shuffleMask);
    lastShuffle = builder.CreateShuffleVector(lastShuffle, nextInput, shuffleMaskVector, "native_shuffle");

    // lastShuffle will have the first <undefIndex> elements set
    shuffleMask.erase(shuffleMask.begin(), shuffleMask.end());
    for (unsigned i = 0; i < undefIndex; ++i) {
      shuffleMask.push_back(i);
    }

    assert(index >= vectorWidth * 2 && index < vectorWidth * 3 && "index not inside 3rd vector!");

    // index now points to the 3rd vector. subtract vectorWidth as it will become new 2nd vector
    index -= vectorWidth;

    // go to next element
    ++inputVectorIndex;

  } while (inputVectorIndex < inputVectors.size());

  return lastShuffle;
}
