//===- rvConfig.h -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @authors simon
//

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
