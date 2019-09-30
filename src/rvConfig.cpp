//===- src/rvConfig.cpp - some more helper functions --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
// TODO refactor & deprecate

#include <sstream>
#include <cstdlib>
#include "rvConfig.h"

bool rvVerbose = false;

namespace rv {

template<typename N>
N
GetValue(const char * name, N defVal) {
  auto * text = getenv(name);
  if (!text) return defVal;
  else {
    std::stringstream ss(text);
    N res;
    ss >> res;
    return res;
  }
}

template double GetValue(const char * name, double defVal);
template size_t GetValue(const char * name, size_t defVal);

} // namespace rv
