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

void fail(const std::string & text) LLVM_ATTRIBUTE_NORETURN;

}

#endif // RV_REPORT_H_
