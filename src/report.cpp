#include "report.h"

#include <llvm/Support/raw_os_ostream.h>

namespace rv {

// report stream (TODO use llvm optimization log stream)
llvm::raw_ostream &
Report() {
  return (llvm::outs() << "rv: ");
}

llvm::raw_ostream &
Error() {
  return (llvm::errs() << "rv ERROR: ");
}


}
