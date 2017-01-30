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

llvm::Value *ShuffleBuilder::shuffleFromInterleaved(llvm::IRBuilder<> &builder, unsigned stride, unsigned start) {
  // expects that the values of each input vector ARE interleaved. creates an non-interleaved value
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
  unsigned index = start;

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

llvm::Value *ShuffleBuilder::shuffleToInterleaved(llvm::IRBuilder<> &builder, unsigned stride, unsigned start) {
  // expects that the values of each input vector are NOT interleaved. creates an interleaved value
  // use a loop that builds shuffles until all <vectorWidth> positions are set
  // shuffles are build like this: take the last shuffle and the next input vector and the shuffle mask
  // as shuffle mask: all positions that have been set in lastShuffle, then all undef positions (will be k * stride)
  // set to <vecWidth> + k. the shuffle that is build becomes the new last shuffle
  // initialize like this: last shuffle = inputVectors[0], next input vector = inputVectors[1],
  // shuffleMask = <[k * stride](=m1)> for all m1 < vectorWidth
  // lastly, return lastShuffle

  Type *i32Ty = builder.getInt32Ty();

  assert(inputVectors.size() >= 2 && "not enough input vectors to build shuffle");
  unsigned inputVectorIndex = ((vectorWidth % stride) * start) % static_cast<unsigned>(inputVectors.size());
  Value *lastShuffle = inputVectors[inputVectorIndex];
  Value *nextInput;
  std::vector<Constant *> shuffleMask(vectorWidth, UndefValue::get(i32Ty));
  inputVectorIndex = (inputVectorIndex + 1) % static_cast<unsigned>(inputVectors.size());
  unsigned counter = 1;

  // index init
  unsigned i = (vectorWidth / stride) * start;
  unsigned index = 0;
  while (index < vectorWidth) {
    Constant *constIdx = ConstantInt::get(i32Ty, i);
    shuffleMask[index] = constIdx;
    ++i;
    index += stride;
  }

  do {
    nextInput = inputVectors[inputVectorIndex];

    // set shuffle mask for next input
    i = ((vectorWidth / stride) * start) + vectorWidth;
    index = counter;
    while (index < vectorWidth) {
      Constant *constIdx = ConstantInt::get(i32Ty, i);
      shuffleMask[index] = constIdx;
      ++i;
      index += stride;
    }

    // create shuffle
    Value *idxVector = ConstantVector::get(shuffleMask);
    lastShuffle = builder.CreateShuffleVector(lastShuffle, nextInput, idxVector, "native_shuffle");

    // prepare next iteration
    inputVectorIndex = (inputVectorIndex + 1) % static_cast<unsigned>(inputVectors.size());
    ++counter;

    // loop over the shuffle mask and replace the already set positions with identity constant
    for (i = 0; i < vectorWidth; ++i) {
      Constant *constIdx = shuffleMask[i];
      if (isa<UndefValue>(constIdx))
        continue;
      constIdx = ConstantInt::get(i32Ty, i);
      shuffleMask[i] = constIdx;
    }

  } while (counter < inputVectors.size());

  return lastShuffle;
}
