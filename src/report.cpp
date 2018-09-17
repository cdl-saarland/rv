#include "report.h"

#include <llvm/Support/raw_os_ostream.h>

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
  if (CheckFlag("RV_REPORT")) {
    return (llvm::outs() << "rv: ");
  }

  // default to null stream
  return llvm::nulls();
}

llvm::raw_ostream &
ReportContinue() {
  if (CheckFlag("RV_REPORT")) {
    return llvm::outs();
  }

  // default to null stream
  return llvm::nulls();
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
