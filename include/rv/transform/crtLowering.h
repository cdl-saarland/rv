//===- rv/transform/crtLowering.h - pre-vect, compiler-rt BC inliner --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#ifndef RV_CRTLOWERING_H
#define RV_CRTLOWERING_H

#include "rv/config.h"
#include "rv/PlatformInfo.h"
#include "llvm/Analysis/TargetLibraryInfo.h"

namespace rv {
  // link the compiler-rt code for the specified complex arithmetic function @funcName with @funcTy into @insertInto
  llvm::Function *
  requestScalarImplementation(const llvm::StringRef & funcName, llvm::FunctionType & funcTy, llvm::Module &insertInto);
}


#endif // RV_CRTLOWERING_H
