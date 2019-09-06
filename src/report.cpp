//===- src/report.cpp stdout diagnostics, envvars --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "report.h"

#include <llvm/Support/raw_os_ostream.h>
#include <llvm/Support/raw_ostream.h>



// RV_REPORT_FILE stream handle
static std::unique_ptr<llvm::raw_fd_ostream> outFileStream;

static llvm::raw_ostream &
reps() {
  if (outFileStream) return *outFileStream;

  // no reporting
  const bool hasReport = rv::CheckFlag("RV_REPORT");
  if (!hasReport) {
    return llvm::nulls();
  }

  // std out
  const char * repFilePath = getenv("RV_REPORT_FILE");
  if (!repFilePath) {
    return llvm::outs();
  }

  // report file stream
  std::error_code EC;
  outFileStream = std::make_unique<llvm::raw_fd_ostream>(llvm::StringRef(repFilePath), EC);
  return *outFileStream;
}

namespace rv {

bool
CheckFlag(const char * flagName) {
  char * envVal = getenv(flagName);
  if (!envVal) return false;
  else return *envVal != '0';
}

// report stream (TODO use llvm optimization log stream)
llvm::raw_ostream &
Report() {
  return reps() << "rv: ";
}

llvm::raw_ostream &
ReportContinue() {
  return reps();
}

llvm::raw_ostream &
Error() {
  return (llvm::errs() << "rv ERROR: ");
}

void
fail(const std::string & text) {
  Error() << text << "\n";
  abort();
}


}
