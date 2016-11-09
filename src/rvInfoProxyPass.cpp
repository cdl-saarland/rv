//===- rvInfoProxyPass.h ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @authors kloessner
//

#include <rv/rvInfoProxyPass.h>
#include <rv/rvInfo.h>

using namespace llvm;

INITIALIZE_PASS(RVInfoProxyPass,
                "RVInfoProxy",
                "RVInfo Proxy Pass",
                false,
                true)

char RVInfoProxyPass::ID = 0;


ImmutablePass*
llvm::createRVInfoProxyPass(rv::RVInfo* rvInfo)
{
    return new RVInfoProxyPass(rvInfo);
}


RVInfoProxyPass::RVInfoProxyPass() : ImmutablePass(ID), mRVInfo(nullptr)
{
    initializeRVInfoProxyPassPass(*PassRegistry::getPassRegistry());
}

RVInfoProxyPass::RVInfoProxyPass(rv::RVInfo* rvInfo) : ImmutablePass(ID), mRVInfo(rvInfo)
{
    initializeRVInfoProxyPassPass(*PassRegistry::getPassRegistry());
}

rv::RVInfo& RVInfoProxyPass::getInfo() const
{
    return *mRVInfo;
}

void RVInfoProxyPass::getAnalysisUsage(llvm::AnalysisUsage &AU) const
{
    AU.setPreservesAll();
}

bool RVInfoProxyPass::runOnModule(llvm::Module& M)
{
    assert (&M == mRVInfo->mModule);
    return false;
}

void RVInfoProxyPass::releaseMemory()
{
    delete mRVInfo;
}

