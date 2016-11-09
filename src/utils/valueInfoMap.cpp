//===- valueInfoMap.cpp ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @author karrenberg
//

#include "rv/utils/valueInfoMap.h"

#include <llvm/IR/Value.h>
#include <llvm/Support/raw_ostream.h>

#include <cassert>

using namespace llvm;

namespace rv {

ValueInfo::ValueInfo(const ValueInfo& other)
: mIsOpUniform(other.mIsOpUniform),
    mIsOpVarying(other.mIsOpVarying),
    mIsOpSequential(other.mIsOpSequential),
    mIsOpSequentialGuarded(other.mIsOpSequentialGuarded),
    mIsResultUniform(other.mIsResultUniform),
    mIsResultVector(other.mIsResultVector),
    mIsResultScalars(other.mIsResultScalars),
    mIsAligned(other.mIsAligned),
    mIsIndexSame(other.mIsIndexSame),
    mIsIndexConsecutive(other.mIsIndexConsecutive)
{
}

ValueInfo::ValueInfo(const bool isOpUniform,
                     const bool isOpVarying,
                     const bool isOpSequential,
                     const bool isOpSequentialGuarded,
                     const bool isResultUniform,
                     const bool isResultVector,
                     const bool isResultScalars,
                     const bool isAligned,
                     const bool isIndexSame,
                     const bool isIndexConsecutive)
: mIsOpUniform(isOpUniform),
    mIsOpVarying(isOpVarying),
    mIsOpSequential(isOpSequential),
    mIsOpSequentialGuarded(isOpSequentialGuarded),
    mIsResultUniform(isResultUniform),
    mIsResultVector(isResultVector),
    mIsResultScalars(isResultScalars),
    mIsAligned(isAligned),
    mIsIndexSame(isIndexSame),
    mIsIndexConsecutive(isIndexConsecutive)
{
}

ValueInfo::~ValueInfo()
{
}




ValueInfoMap::ValueInfoMap()
{
}

ValueInfoMap::ValueInfoMap(const ValueInfoMap& other)
{
    for (const auto &VI : other)
    {
        ValueInfo* info = new ValueInfo(*VI.second);
        mValueMap.insert(std::make_pair(VI.first, info));
    }
}

ValueInfoMap::~ValueInfoMap()
{
    for (const auto &it : mValueMap)
    {
        delete it.second;
    }
}

bool
ValueInfoMap::empty() const
{
    return mValueMap.empty();
}

ValueInfoMap::iterator
ValueInfoMap::begin()
{
    return mValueMap.begin();
}

ValueInfoMap::const_iterator
ValueInfoMap::begin() const
{
    return mValueMap.begin();
}

ValueInfoMap::iterator
ValueInfoMap::end()
{
    return mValueMap.end();
}

ValueInfoMap::const_iterator
ValueInfoMap::end() const
{
    return mValueMap.end();
}

bool
ValueInfoMap::add(const Value& value,
                  const bool   isOpUniform,
                  const bool   isOpVarying,
                  const bool   isOpSequential,
                  const bool   isOpSequentialGuarded,
                  const bool   isResultUniform,
                  const bool   isResultVector,
                  const bool   isResultScalars,
                  const bool   isAligned,
                  const bool   isIndexSame,
                  const bool   isIndexConsecutive)
{
    if (hasMapping(value))
    {
        errs() << "ERROR: adding more than one mapping for scalar value '"
            << value.getName() << "' is not allowed!\n";
        return false;
    }

    ValueInfo* info = new ValueInfo(isOpUniform,
                                    isOpVarying,
                                    isOpSequential,
                                    isOpSequentialGuarded,
                                    isResultUniform,
                                    isResultVector,
                                    isResultScalars,
                                    isAligned,
                                    isIndexSame,
                                    isIndexConsecutive);

    const std::pair<ValueMapType::iterator, bool>& result =
        mValueMap.insert(std::make_pair(&value, info));

    return result.second;
}

bool
ValueInfoMap::hasMapping(const Value& value) const
{
    return mValueMap.count(&value);
}

const ValueInfo&
ValueInfoMap::get(const Value& value) const
{
    assert (hasMapping(value));
    return *mValueMap.find(&value)->second;
}

void
ValueInfoMap::mapValueInformation(ValueToValueMapTy& valueMap)
{
    // We iterate over a map while deleting and inserting elements...
    // Better just create a new map and replace the old one.
    ValueMapType newValueMap;

    for (auto it : mValueMap)
    {
        // Map ValueInfo contents.
        const Value*     value = it.first;
        const ValueInfo* info  = it.second;

        assert (valueMap.count(value));
        const Value* newValue = valueMap[value];

        // Map entry of mValueMap itself.
        newValueMap[newValue] = info;
    }

    mValueMap.clear();
    mValueMap.insert(newValueMap.begin(), newValueMap.end());
}

void
ValueInfoMap::print(raw_ostream& o) const
{
    o << "ValueInfoMap:\n";
    for (auto it : mValueMap)
    {
        const ValueInfo& info = *it.second;
        o << "  * " << *it.first << " ( "
            << (info.mIsOpUniform ? "OP_UNIFORM " : "")
            << (info.mIsOpVarying ? "OP_VARYING " : "")
            << (info.mIsOpSequential ? "OP_SEQUENTIAL " : "")
            << (info.mIsOpSequentialGuarded ? "OP_SEQUENTIAL_GUARDED " : "")
            << (info.mIsResultUniform ? "RES_UNIFORM " : "")
            << (info.mIsResultVector ? "RES_VECTOR " : "")
            << (info.mIsResultScalars ? "RES_SCALARS " : "")
            << (info.mIsAligned ? "ALIGN_TRUE " : "ALIGN_FALSE ")
            << (info.mIsIndexSame ? "INDEX_SAME " :
                (info.mIsIndexConsecutive ? "INDEX_CONSECUTIVE " : "INDEX_RANDOM "))
            << ")\n";
    }
}

}
