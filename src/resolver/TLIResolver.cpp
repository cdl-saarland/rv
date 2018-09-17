#include "rv/resolver/resolver.h"

#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/Analysis/TargetTransformInfo.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>

using namespace llvm;

namespace rv {

class TLIFuncResolver : public FunctionResolver {
  TargetLibraryInfo & TLI;
  llvm::StringRef funcName;
  llvm::FunctionType & scaFuncTy;
  int vectorWidth;

public:
  TLIFuncResolver(Module & _destModule, TargetLibraryInfo & _TLI, llvm::StringRef _funcName, llvm::FunctionType & _scaFuncTy, int _vectorWidth)
  : FunctionResolver(_destModule)
  , TLI(_TLI)
  , funcName(_funcName)
  , scaFuncTy(_scaFuncTy)
  , vectorWidth(_vectorWidth)
  {}

  VectorShape
  requestResultShape() { return VectorShape::varying(); }

  Function& requestVectorized() {
    // TODO actually emit a SIMD declaration for this function
    StringRef tliFnName = TLI.getVectorizedFunction(funcName, vectorWidth);
    return *targetModule.getFunction(tliFnName);
  }
};

class TLIResolverService : public ResolverService {
  TargetLibraryInfo & TLI;

public:
  TLIResolverService(TargetLibraryInfo & _TLI)
  : TLI(_TLI)
  {}

  std::unique_ptr<FunctionResolver>
  resolve(llvm::StringRef funcName, llvm::FunctionType & scaFuncTy, const VectorShapeVec & argShapes, int vectorWidth, llvm::Module & destModule) {
    StringRef tliFnName = TLI.getVectorizedFunction(funcName, vectorWidth);
    if (!tliFnName.empty()) {
      return std::make_unique<TLIFuncResolver>(destModule, TLI, funcName, scaFuncTy, vectorWidth);
    }
    return nullptr;
  }
};


} // namespace rv
