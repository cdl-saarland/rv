//===- rv/analysis/loopAnnotations.h - loop md reader/writer --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef RV_ANALYSIS_LOOPANNOTATIONS_H
#define RV_ANALYSIS_LOOPANNOTATIONS_H

#include <limits>
#include <cstdint>

#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Support/raw_ostream.h>

namespace rv {
  using iter_t = int64_t;

  static const iter_t ParallelDistance = std::numeric_limits<iter_t>::max();
  std::string DepDistToString(iter_t depDist);

  // TODO use std++17 std::optional<bool>
  template<typename T>
  class Optional {
    bool hasValue;
    T value;

  public:
    Optional() : hasValue(false) {}
    Optional(T val) : hasValue(true), value(val) {}

    bool isSet() const { return hasValue; }
    T get() const { return value; }
    T safeGet(T defaultVal) {
      if (isSet()) return get(); else return defaultVal;
    }

    Optional<T>& operator=(T v) {
      value = v;
      hasValue = true;
      return *this;
    }
  };

  struct
  LoopMD {
    // whether this loop was already vectorized
    Optional<bool> alreadyVectorized;

    // whether the loop carries and explicit hint to enable/disable loop vectorization
    Optional<bool> vectorizeEnable;

    // mandatory vector width
    Optional<iter_t> explicitVectorWidth;

    // minimum dependence distance between two loop iterations
    Optional<iter_t> minDepDist;

    llvm::raw_ostream& print(llvm::raw_ostream & out) const;
    void dump() const;
  };

  // Clear all loop vectorize annotations from the loop \p L.
  void ClearLoopVectorizeAnnotations(llvm::Loop & L);

  // Encode \p loopMD as LLVM LoopVectorizer Metadata hints for the loop \p L.
  void SetLLVMLoopAnnotations(llvm::Loop & L, LoopMD && loopMD);

  // Encode \p llvmLoopMD as MDNodes to attach to an LLVM Loop ID node.
  void AppendMDEntries(llvm::LLVMContext & ctx, std::vector<llvm::Metadata*> & mdArgs, const LoopMD & llvmLoopMD);

  // Optimistic reading of two conflicting loop annotations
  LoopMD OptimisticJoin(LoopMD && A, LoopMD && B);

  // Parse loop annotations for loop \p L
  LoopMD GetLoopAnnotation(llvm::Loop & L);
}

#endif // RV_ANALYSIS_LOOPANNOTATIONS_H
