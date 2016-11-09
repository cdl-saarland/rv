//===- functionInfoMap.cpp ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @author karrenberg

#include <cassert>

#include <llvm/IR/Function.h>
#include <llvm/IR/DerivedTypes.h> // FunctionType
#include <llvm/Support/raw_ostream.h>

#include "rv/utils/functionInfoMap.h"

using namespace llvm;

namespace rv {

NativeFunctionInfo::NativeFunctionInfo(const NativeFunctionInfo& other)
: mSimdFunction(other.mSimdFunction),
    mMaskIndex(other.mMaskIndex),
    mMayHaveSideEffects(other.mMayHaveSideEffects),
    mUniformArguments(other.mUniformArguments)
{
}

NativeFunctionInfo::NativeFunctionInfo(const Function*             simdFunction,
                                       const int                   maskIndex,
                                       const bool                  mayHaveSideEffects,
                                       const SmallVector<bool, 4>& uniformArguments)
: mSimdFunction(simdFunction),
    mMaskIndex(maskIndex),
    mMayHaveSideEffects(mayHaveSideEffects),
    mUniformArguments(uniformArguments)
{
}

NativeFunctionInfo::~NativeFunctionInfo()
{
}

bool
NativeFunctionInfo::isUniformArgument(const unsigned index) const
{
    assert (index < mUniformArguments.size());
    return mUniformArguments[index];
}




FunctionInfoMap::FunctionInfoMap()
{
}

FunctionInfoMap::FunctionInfoMap(const FunctionInfoMap& other)
{
    for (const auto &FI : other)
    {
        NativeFunctionInfo* info = new NativeFunctionInfo(*FI.second);
        mNativeFunctionMap.insert(std::make_pair(FI.first, info));
    }
}

FunctionInfoMap::~FunctionInfoMap()
{
    for (const auto &it : mNativeFunctionMap)
    {
        delete it.second;
    }
}

bool
FunctionInfoMap::empty() const
{
    return mNativeFunctionMap.empty();
}

FunctionInfoMap::iterator
FunctionInfoMap::begin()
{
    return mNativeFunctionMap.begin();
}

FunctionInfoMap::const_iterator
FunctionInfoMap::begin() const
{
    return mNativeFunctionMap.begin();
}

FunctionInfoMap::iterator
FunctionInfoMap::end()
{
    return mNativeFunctionMap.end();
}

FunctionInfoMap::const_iterator
FunctionInfoMap::end() const
{
    return mNativeFunctionMap.end();
}

bool
FunctionInfoMap::add(const Function&             scalarFunction,
                     const Function&             simdFunction,
                     const int                   maskIndex,
                     const bool                  mayHaveSideEffects,
                     const SmallVector<bool, 4>& uniformArguments)
{
    assert (uniformArguments.size() == scalarFunction.getArgumentList().size());

    if (maskIndex < -1 ||
        maskIndex > (int)scalarFunction.getFunctionType()->getNumParams())
    {
        errs() << "ERROR: bad mask index found (should be between -1 "
            << "(no mask) and " << scalarFunction.getFunctionType()->getNumParams()
            << ")!\n";
        return false;
    }
#if 0
    if (hasMapping(scalarFunction))
    {
        errs() << "ERROR: adding more than one mapping for scalar function '"
            << scalarFunction.getName() << "' is not allowed!\n";
        return false;
    }
#else
    auto * oldMapping = mNativeFunctionMap[&scalarFunction];
    delete oldMapping;
    mNativeFunctionMap.erase(&scalarFunction);
#endif
    NativeFunctionInfo* info = new NativeFunctionInfo(&simdFunction,
                                                      maskIndex,
                                                      mayHaveSideEffects,
                                                      uniformArguments);

    const std::pair<NativeFunctionMapType::iterator, bool>& result =
        mNativeFunctionMap.insert(std::make_pair(&scalarFunction, info));

    return result.second;
}

bool
FunctionInfoMap::hasMapping(const Function& scalarFunction) const
{
    return mNativeFunctionMap.count(&scalarFunction);
}

const Function&
FunctionInfoMap::getSimdFunction(const Function& scalarFunction) const
{
    assert (hasMapping(scalarFunction));
    return *mNativeFunctionMap.find(&scalarFunction)->second->mSimdFunction;
}

int
FunctionInfoMap::getMaskIndex(const Function& scalarFunction) const
{
    assert (hasMapping(scalarFunction));
    return mNativeFunctionMap.find(&scalarFunction)->second->mMaskIndex;
}

bool
FunctionInfoMap::mayHaveSideEffects(const Function& scalarFunction) const
{
    assert (hasMapping(scalarFunction));
    return mNativeFunctionMap.find(&scalarFunction)->second->mMayHaveSideEffects;
}

bool
FunctionInfoMap::isUniformArgument(const Function& scalarFunction,
                                   const unsigned index) const
{
    assert (hasMapping(scalarFunction));
    return mNativeFunctionMap.find(&scalarFunction)->second->isUniformArgument(index);

}

}
