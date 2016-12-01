//===- sleefLibrary.h -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @author montada

#ifndef RV_SLEEFLIBRARY_H
#define RV_SLEEFLIBRARY_H

#include <PlatformInfo.h>
#include "llvm/Analysis/TargetLibraryInfo.h"

namespace rv {
  bool addSleefMappings(const bool useSSE, const bool useAVX, const bool useAVX2, PlatformInfo &platformInfo);
}

#endif //RV_SLEEFLIBRARY_H
