//===- src/rvDebug.h - persistent Dump() functions --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "rv/rvDebug.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/Debug.h"

void rv::Dump(const llvm::Value & val) {
  val.print(llvm::dbgs(), true);
}

void rv::Dump(const llvm::Module & mod) {
  mod.print(llvm::dbgs(), nullptr, false, true);
}
