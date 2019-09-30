//===- rv/rvDebug.h - persistent Dump() functions --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef RV_RVDEBUG_H
#define RV_RVDEBUG_H

namespace llvm {
  class Value;
  class Module;
}

namespace rv {
  void Dump(const llvm::Value & val);
  void Dump(const llvm::Module & mod);
}
#endif
