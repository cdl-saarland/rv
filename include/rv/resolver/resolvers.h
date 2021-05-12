//===- rv/resolver/resolvers.h - builtin function-call resolvers --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef RV_RESOLVERS_H
#define RV_RESOLVERS_H

#include "rv/config.h"
#include "rv/PlatformInfo.h"

namespace rv {
  // Use the SLEEF library to implement math functions.
  void addSleefResolver(const Config & config, PlatformInfo & platInfo);

  // Vectorize functions that are declares with "pragma omp declare simd".
  void addOpenMPResolver(const Config & config, PlatformInfo & platInfo);

  // use recursive vectorization.
  void addRecursiveResolver(const Config & config, PlatformInfo & platInfo);
}

#endif
