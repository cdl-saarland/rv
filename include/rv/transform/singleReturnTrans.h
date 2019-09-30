//===- rv/transform/singleReturnTrans.h - merge all return blocks --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#ifndef RV_TRANSFORM_SINGLERETURNTRANS_H
#define RV_TRANSFORM_SINGLERETURNTRANS_H

#include <rv/region/Region.h>

namespace rv {

// join all return terminators in the region (needed for linearization in case there is control divergence around returns)
struct SingleReturnTrans {
  static bool run(Region & region);
};

}

#endif // RV_TRANSFORM_SINGLERETURNTRANS_H
