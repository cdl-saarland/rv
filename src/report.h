#ifndef RV_REPORT_H_
#define RV_REPORT_H_

#include <llvm/Support/raw_ostream.h>

namespace rv {

// check if an environment flag is set
bool CheckFlag(const char * flagName);

// output stream for diagnostic outputs
llvm::raw_ostream & Report();

// output stream for error
llvm::raw_ostream & Error();

}

#endif // RV_REPORT_H_
