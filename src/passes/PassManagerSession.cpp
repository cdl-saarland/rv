#include "rv/passes/PassManagerSession.h"

#include "llvm/Passes/PassBuilder.h"

using namespace rv;
using namespace llvm;

PassManagerSession::PassManagerSession() {
  // setup LLVM analysis infrastructure
  PassBuilder PB;
  PB.registerModuleAnalyses(MAM);
  PB.registerFunctionAnalyses(FAM);
  PB.registerLoopAnalyses(LAM);
  PB.registerCGSCCAnalyses(CGAM);
  PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);
}
