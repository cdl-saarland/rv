//===- VectorizationInfoProxyPass.cpp -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @author kloessner
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
