//===- src/intrinsics.cpp - RV intrinsics --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "rv/intrinsics.h"

#include <llvm/IR/Instructions.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/DerivedTypes.h>

using namespace llvm;

std::string
MangleType(const Type & Ty) {
  if (Ty.isIntegerTy()) {
    return "i" + std::to_string(Ty.getScalarSizeInBits());
  } else if (Ty.isDoubleTy()) {
    return "d";
  } else if (Ty.isFloatTy()) {
    return "f";
  } else if (auto ScalableVT = dyn_cast<ScalableVectorType>(&Ty)) {
    return "nxv"+ std::to_string(ScalableVT->getElementCount().getKnownMinValue()) + MangleType(*ScalableVT->getElementType());
  } else if (auto FixedVT = dyn_cast<FixedVectorType>(&Ty)) {
    return "v" + std::to_string(FixedVT->getNumElements()) + MangleType(*FixedVT->getElementType());
  }
  abort(); // TODO we really should use LLVM's facilities here...
}

namespace rv {
RVIntrinsic GetIntrinsicID(const llvm::Function& func) {
  auto funcName = func.getName();
#define RV_MAP_INTRINSIC(FUNC, VALUE) \
  if ( funcName.startswith(StringRef(#FUNC)) ) return RVIntrinsic:: VALUE;
#include "rv/intrinsics.def"
#undef RV_MAP_INTRINSIC

  return RVIntrinsic::Unknown;
}

std::string GetIntrinsicName(RVIntrinsic id, Type * DataTy) {
  StringRef BaseName = "";
  switch (id) {
#define RV_MAP_INTRINSIC(FUNC, VALUE) \
  case RVIntrinsic:: VALUE : BaseName= #FUNC; break;
#include "rv/intrinsics.def"
#undef RV_MAP_INTRINSIC
  case RVIntrinsic::Unknown: break;
  default: abort();
  }
  if (DataTy) {
    std::string typeSuffix = MangleType(*DataTy);
    return (BaseName + "_" + typeSuffix).str();
  }
  return BaseName.str();
}

static
const Function*
GetCallee(const Value & val) {
  const auto * call = dyn_cast<const CallInst>(&val);
  if (!call) return nullptr;
  const auto * func = call->getCalledFunction();
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
  return func->getName().startswith(GetIntrinsicName(id));
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
    case RVIntrinsic::LaneID: {
      return (VectorMapping(
        &func,
        &func,
        0, // no specific vector width
        -1, //
        VectorShape::cont(0), // contiguous thread id
        {},
        CallPredicateMode::SafeWithoutPredicate
        ));
    } break;
    case RVIntrinsic::NumLanes: {
      return (VectorMapping(
        &func,
        &func,
        0, // no specific vector width
        -1, //
        VectorShape::uni(), // broadcasted (static) number of lanes
        {},
        CallPredicateMode::SafeWithoutPredicate
        ));
    } break;
  }
}

llvm::Function &
DeclareIntrinsic(RVIntrinsic id, llvm::Module & mod, Type *DataTy) {
  auto &context = mod.getContext();
  auto *boolTy = Type::getInt1Ty(context);
  auto *intTy = Type::getIntNTy(context, 32);

  std::string mangledName = GetIntrinsicName(id, DataTy);

  Function * rvFunc = nullptr;
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

  case RVIntrinsic::NumLanes:
  case RVIntrinsic::LaneID: {
    auto *funcTy = FunctionType::get(intTy, {}, false);
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

