//===- rv/VectorizationInfoProxyPass.cpp - vecInfo proxy pass (FIXME deprecated) --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#include "rv/VectorizationInfoProxyPass.h"

using namespace llvm;

INITIALIZE_PASS(VectorizationInfoProxyPass,
                "vecInfoProxy",
                "Vectorization Info ProxyPass",
                false,
                true)

// VectorizationInfoProxyPass
char VectorizationInfoProxyPass::ID = 0;

VectorizationInfoProxyPass::VectorizationInfoProxyPass()
: ImmutablePass(ID),
  vecInfo(nullptr),
  platInfo(nullptr)
{
    initializeVectorizationInfoProxyPassPass(*PassRegistry::getPassRegistry());
}

VectorizationInfoProxyPass::VectorizationInfoProxyPass(rv::PlatformInfo & _platInfo, rv::VectorizationInfo & _vecInfo) :
        ImmutablePass(ID),
        vecInfo(&_vecInfo),
        platInfo(&_platInfo)
{
    initializeVectorizationInfoProxyPassPass(*PassRegistry::getPassRegistry());
}


rv::VectorizationInfo&
VectorizationInfoProxyPass::getInfo() const
{
    return *vecInfo;
}


rv::PlatformInfo &
VectorizationInfoProxyPass::getPlatformInfo() const {
  return *platInfo;
}
