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
