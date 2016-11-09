//
// Created by thorsten on 06.10.16.
//

#include "PlatformInfo.h"

namespace rv {

PlatformInfo::PlatformInfo(TargetTransformInfo* TTI, TargetLibraryInfo* TLI) : mTTI(TTI), mTLI(TLI)
{ }

void PlatformInfo::addMapping(const Function* function, const rv::VectorMapping* mapping)
{
    funcMappings[function] = mapping;
}

void PlatformInfo::removeMappingIfPresent(const Function* function)
{
    auto found = funcMappings.find(function);

    if (found != funcMappings.end())
        funcMappings.erase(found);
}

const rv::VectorMapping* PlatformInfo::getMappingByFunction(const Function* function) const
{
    auto found = funcMappings.find(function);

    if (found != funcMappings.end())
        return found->second;

    return nullptr;
}

void PlatformInfo::setTTI(TargetTransformInfo* TTI)
{
    mTTI = TTI;
}

void PlatformInfo::setTLI(TargetLibraryInfo* TLI)
{
    mTLI = TLI;
}

TargetTransformInfo* PlatformInfo::getTTI()
{
    return mTTI;
}

TargetLibraryInfo* PlatformInfo::getTLI()
{
    return mTLI;
}

}
