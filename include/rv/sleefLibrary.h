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
  // add sleef vector math library
  bool addSleefMappings(const Config & config, PlatformInfo &platformInfo, bool useImpreciseFunctions);

  llvm::Function *
  requestSleefFunction(const llvm::StringRef &funcName, llvm::StringRef &vecFuncName, llvm::Module *insertInto, bool doublePrecision);

  llvm::Function *
  requestScalarImplementation(const llvm::StringRef & funcName, llvm::FunctionType & funcTy, llvm::Module &insertInto);
}

#endif //RV_SLEEFLIBRARY_H
