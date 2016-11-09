//
// Created by thorsten on 06.10.16.
//

#ifndef RV_PLATFORMINFO_H
#define RV_PLATFORMINFO_H

#include <rv/vectorMapping.h>
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/Analysis/TargetLibraryInfo.h"

using namespace llvm;

namespace rv {

class PlatformInfo
{
    typedef std::map<const Function*, const VectorMapping*> FuncToVecMapping;
public:
    PlatformInfo(TargetTransformInfo* TTI, TargetLibraryInfo* TLI);

    void addMapping(const Function* function, const VectorMapping* mapping);

    void removeMappingIfPresent(const Function* function);
    const VectorMapping* getMappingByFunction(const Function* function) const;

    void setTTI(TargetTransformInfo* TTI);
    void setTLI(TargetLibraryInfo* TLI);

    TargetTransformInfo* getTTI();
    TargetLibraryInfo* getTLI();

private:
    TargetTransformInfo* mTTI;
    TargetLibraryInfo* mTLI;
    FuncToVecMapping funcMappings;
};

}

#endif // RV_PLATFORMINFO_H
