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

#include "rv/config.h"
#include "rv/PlatformInfo.h"
#include "llvm/Analysis/TargetLibraryInfo.h"

namespace rv {
  // function resolver for SLEEF
  // unit for ulpErrorBound is tenth of ULP (a value of 10 implies that an ULP error of <= 1.0 is acceptable)
  void addSleefResolver(const Config & config, PlatformInfo & platInfo, unsigned ulpErrorBound);

  // link the compiler-rt code for the specified complex arithmetic function @funcName with @funcTy into @insertInto
  llvm::Function *
  requestScalarImplementation(const llvm::StringRef & funcName, llvm::FunctionType & funcTy, llvm::Module &insertInto);
}

#endif //RV_SLEEFLIBRARY_H
