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

#include "llvm/Analysis/TargetLibraryInfo.h"

namespace rv {
  llvm::TargetLibraryInfo addSleefMappings(const bool useSSE, const bool useAVX, const bool useAVX2);
}

#endif //RV_SLEEFLIBRARY_H
