#include "rv/intrinsics.h"

#include <llvm/IR/Instructions.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>

using namespace llvm;

namespace rv {
RVIntrinsic GetIntrinsicID(const llvm::Function& func) {
  auto funcName = func.getName();
#define RV_MAP_INTRINSIC(FUNC, VALUE) \
  if ( StringRef(#FUNC).startswith(funcName) ) return RVIntrinsic:: VALUE;
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


VectorMapping
GetIntrinsicMapping(Function & func, RVIntrinsic rvIntrin) {
  switch (rvIntrin) {
    default:
      llvm_unreachable("unrecognized RVIntrinsic!");
    case RVIntrinsic::EntryMask: {
      return (VectorMapping(
        &func,
        &func,
        0, // no specific vector width
        -1, //
        VectorShape::varying(), // FIXME actually could be uniform.. depending on whether the WFV mapping provides a maskPos>=0 or not.
        {},
        CallPredicateMode::SafeWithoutPredicate
      ));
    } break;

    case RVIntrinsic::Mask: {
      return (VectorMapping(
        &func,
        &func,
        0, // no specific vector width
        -1, //
        VectorShape::varying(),
        {},
        CallPredicateMode::SafeWithoutPredicate
      ));
    } break;

    case RVIntrinsic::Any:
    case RVIntrinsic::All: {
      return (VectorMapping(
        &func,
        &func,
        0, // no specific vector width
        -1, //
        VectorShape::uni(),
        {VectorShape::varying()},
        CallPredicateMode::SafeWithoutPredicate
      ));
    } break;

    case RVIntrinsic::Extract: {
      return (VectorMapping(
        &func,
        &func,
        0, // no specific vector width
        -1, //
        VectorShape::uni(),
        {VectorShape::varying(), VectorShape::uni()},
        CallPredicateMode::SafeWithoutPredicate
      ));
    } break;

    case RVIntrinsic::Insert: {
      return (VectorMapping(
        &func,
        &func,
        0, // no specific vector width
        -1, //
        VectorShape::varying(),
        {VectorShape::varying(), VectorShape::uni(), VectorShape::uni()},
        CallPredicateMode::SafeWithoutPredicate
      ));
    } break;

    case RVIntrinsic::VecLoad: {
      return (VectorMapping(
        &func,
        &func,
        0, // no specific vector width
        -1, //
        VectorShape::uni(),
        {VectorShape::varying(), VectorShape::uni()},
        CallPredicateMode::SafeWithoutPredicate
      ));
    } break;

    case RVIntrinsic::VecStore: {
      return (VectorMapping(
        &func,
        &func,
        0, // no specific vector width
        -1, //
        VectorShape::uni(),
        {VectorShape::varying(), VectorShape::uni(), VectorShape::uni()},
        CallPredicateMode::SafeWithoutPredicate
      ));
    } break;

    case RVIntrinsic::Shuffle: {
      return (VectorMapping(
        &func,
        &func,
        0, // no specific vector width
        -1, //
        VectorShape::uni(),
        {VectorShape::uni(), VectorShape::uni()},
        CallPredicateMode::SafeWithoutPredicate
      ));
    } break;

    case RVIntrinsic::Ballot: {
      return (VectorMapping(
        &func,
        &func,
        0, // no specific vector width
        -1, //
        VectorShape::uni(),
        {VectorShape::varying()},
        CallPredicateMode::SafeWithoutPredicate
        ));
    } break;

    case RVIntrinsic::PopCount: {
      return (VectorMapping(
        &func,
        &func,
        0, // no specific vector width
        -1, //
        VectorShape::uni(),
        {VectorShape::varying()},
        CallPredicateMode::SafeWithoutPredicate
        ));
    } break;

    case RVIntrinsic::Index: {
      return (VectorMapping(
        &func,
        &func,
        0, // no specific vector width
        -1, //
        VectorShape::varying(),
        {VectorShape::varying()},
        CallPredicateMode::SafeWithoutPredicate
        ));
    } break;

    case RVIntrinsic::Align: {
      return (VectorMapping(
        &func,
        &func,
        0, // no specific vector width
        -1, //
        VectorShape::undef(),
        {VectorShape::undef(), VectorShape::uni()},
        CallPredicateMode::SafeWithoutPredicate
        ));
    } break;

    case RVIntrinsic::Compact: {
      return (VectorMapping(
        &func,
        &func,
        0, // no specific vector width
        -1, //
        VectorShape::varying(),
        {VectorShape::varying(), VectorShape::varying()},
        CallPredicateMode::SafeWithoutPredicate
        ));
    } break;
  }
}

llvm::Function &
DeclareIntrinsic(RVIntrinsic id, llvm::Module & mod) {
  auto &context = mod.getContext();
  auto *boolTy = Type::getInt1Ty(context);
  auto *intTy = Type::getIntNTy(context, 32);

  Function * rvFunc = nullptr;
  std::string mangledName = GetIntrinsicName(id);
  switch (id) {
  default:
    // TODO some decls are missing
    llvm_unreachable("unrecognized rv intrinsic");

  case RVIntrinsic::EntryMask: {
    auto *funcTy = FunctionType::get(boolTy, {}, false);
    rvFunc = Function::Create(funcTy, GlobalValue::ExternalLinkage, mangledName, &mod);
  } break;

  case RVIntrinsic::Any:
  case RVIntrinsic::All: {
    auto *funcTy = FunctionType::get(boolTy, boolTy, false);
    rvFunc = Function::Create(funcTy, GlobalValue::ExternalLinkage, mangledName, &mod);
    rvFunc->setDoesNotAccessMemory();
    rvFunc->setDoesNotThrow();
    rvFunc->setConvergent();
    rvFunc->setDoesNotRecurse();
  } break;

  case RVIntrinsic::Ballot:
  case RVIntrinsic::PopCount: {
    auto *funcTy = FunctionType::get(intTy, boolTy, false);
    rvFunc = Function::Create(funcTy, GlobalValue::ExternalLinkage, mangledName, &mod);
    rvFunc->setDoesNotAccessMemory();
    rvFunc->setDoesNotThrow();
    rvFunc->setConvergent();
    rvFunc->setDoesNotRecurse();
  } break;

  }

  // set default attributes
  rvFunc->setDoesNotAccessMemory();
  rvFunc->setDoesNotThrow();
  rvFunc->setConvergent();
  rvFunc->setDoesNotRecurse();
  return *rvFunc;
}

}

