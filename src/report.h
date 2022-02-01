//===- src/report.h - stdout diagnostics, envvars --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef RV_REPORT_H_
#define RV_REPORT_H_

#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/Compiler.h>

namespace rv {

// check if an environment flag is set
bool CheckFlag(const char * flagName);

// output stream for diagnostic outputs (prefixes "rv: ")
llvm::raw_ostream & Report();

// continue a "rv: " line started with "Report()"
llvm::raw_ostream & ReportContinue();

// output stream for error
llvm::raw_ostream & Error();

[[noreturn]] void fail(const std::string &text);
}

#endif // RV_REPORT_H_
