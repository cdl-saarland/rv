//===- src/transform/crtLowering.cpp - pre-vect, compiler-rt BC inliner --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#include "rv/transform/crtLowering.h"

#include "utils/rvLinking.h"
#include "utils/rvTools.h"
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>

using namespace llvm;

#ifdef RV_ENABLE_CRT
extern const unsigned char * crt_Buffer;
extern const size_t crt_BufferLen;

#endif

namespace rv {

// compiler-rt early inlining
Function *
requestScalarImplementation(const StringRef & funcName, FunctionType & funcTy, Module &insertInto) {
#ifdef RV_ENABLE_CRT
  static Module * scalarModule = nullptr;
  if (!scalarModule) {
    scalarModule = createModuleFromBuffer(reinterpret_cast<const char*>(&crt_Buffer), crt_BufferLen, insertInto.getContext());
  }

  if (!scalarModule) return nullptr; // could not load module

  auto * scalarFn = scalarModule->getFunction(funcName);
  if (!scalarFn) return nullptr;
  return &cloneFunctionIntoModule(*scalarFn, insertInto, funcName);
#else
  return nullptr; // compiler-rt not available as bc module
#endif
}

} // namespace rv
