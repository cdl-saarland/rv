//===- rv/config.h - vectorizer options --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef RV_CONFIG_H
#define RV_CONFIG_H

#include <llvm/Support/raw_ostream.h>

namespace llvm {
  class Function;
}

namespace rv {


struct Config {
  Config();

// native configuration (backend)
  bool scalarizeIndexComputation;
  bool useScatterGatherIntrinsics;
  bool enableMaskedMove;
  bool useSafeDivisors; // blend-in safe divisors to eliminate spurious arithmetic exceptions

// optimization flags
  bool enableIRPolish;
  bool enableOptimizedBlends;

  // maximum ULP error bound for math functions
  // unit for maxULPErrorBound is tenth of ULP (a value of 10 implies that an ULP error of <= 1.0 is acceptable)
  int maxULPErrorBound;

// target features
  bool useSSE;
  bool useAVX;
  bool useAVX2;
  bool useAVX512;
  bool useADVSIMD;

// code gen options
  void print(llvm::raw_ostream&) const;

  // create default configuration (RV_ARCH env var)
  static Config createDefaultConfig();

  // auto-detect target machine features (SIMD ISAs) for function \p F.
  static Config createForFunction(llvm::Function & F);
};

}


#endif // RV_CONFIG_H
