#include "rv/intrinsics.h"

#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>

using namespace llvm;

namespace rv {
RVIntrinsic GetIntrinsicID(const llvm::Function& func) {
  auto funcName = func.getName();
#define RV_MAP_INTRINSIC(FUNC, VALUE) \
  if (funcName == #FUNC ) return RVIntrinsic:: VALUE;
#include "rv/intrinsics.def"
#undef RV_MAP_INTRINSIC

  return RVIntrinsic::Unknown;
}

StringRef GetIntrinsicName(RVIntrinsic id) {
  switch (id) {
#define RV_MAP_INTRINSIC(FUNC, VALUE) \
  case RVIntrinsic:: VALUE : return #FUNC;
#include "rv/intrinsics.def"
#undef RV_MAP_INTRINSIC
  case RVIntrinsic::Unknown: return "";
  default: abort();
  }
}

static
const Function*
GetCallee(const Value & val) {
  const auto * call = dyn_cast<const CallInst>(&val);
  if (!call) return nullptr;
  const auto * func = dyn_cast<const Function>(call->getCalledValue());
  if (!func) return nullptr;
  return func;
}

RVIntrinsic
GetIntrinsicID(const llvm::Value& val) {
  const auto * func = GetCallee(val);
  if (!func) return RVIntrinsic::Unknown;
  return GetIntrinsicID(*func);
}


bool
IsIntrinsic(const llvm::Value& val, RVIntrinsic id) {
  const auto * func = GetCallee(val);
  if (!func) return false;
  return GetIntrinsicName(id) == func->getName();
}

}

