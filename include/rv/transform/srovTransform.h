//===- rv/transform/srovTransform.h - scalar replication of varying aggregates  --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#ifndef RV_TRANSFORM_SROVTRANSFORM_H
#define RV_TRANSFORM_SROVTRANSFORM_H

#include <llvm/IR/Value.h>
#include <llvm/Transforms/Utils/ValueMapper.h>

#include "rv/PlatformInfo.h"
#include "rv/shape/vectorShape.h"

namespace llvm {
  class AllocaInst;
  class DataLayout;
}

namespace rv {

class VectorizationInfo;

class SROVTransform {
  VectorizationInfo & vecInfo;
  const PlatformInfo & platInfo;

public:
  SROVTransform(VectorizationInfo & _vecInfo, const PlatformInfo & _platInfo);

  bool run();
};


} // namespace rv

#endif// RV_TRANSFORM_SROVTRANSFORM_H
