//===- functionInfoMap.h ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//

#ifndef _FUNCTIONINFOMAP_H
#define	_FUNCTIONINFOMAP_H

#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/ValueMap.h>


using namespace llvm;

namespace rv {

class NativeFunctionInfo {
public:
    const Function*            mSimdFunction;
    const int                  mMaskIndex;
    const bool                 mMayHaveSideEffects;
    const SmallVector<bool, 4> mUniformArguments;

    explicit NativeFunctionInfo(const NativeFunctionInfo& other);
    NativeFunctionInfo(const Function*             simdFunction,
                       const int                   maskIndex,
                       const bool                  mayHaveSideEffects,
                       const SmallVector<bool, 4>& uniformArguments);

    ~NativeFunctionInfo();

    bool isUniformArgument(const unsigned index) const;
};

class FunctionInfoMap {
private:
    typedef ValueMap<const Function*, const NativeFunctionInfo*> NativeFunctionMapType;
    NativeFunctionMapType mNativeFunctionMap;

public:
    FunctionInfoMap();
    explicit FunctionInfoMap(const FunctionInfoMap& other);
    ~FunctionInfoMap();

    typedef NativeFunctionMapType::iterator       iterator;
    typedef NativeFunctionMapType::const_iterator const_iterator;

    iterator       begin();
    const_iterator begin() const;
    iterator       end();
    const_iterator end() const;

    bool empty() const;

    bool add(const Function&             scalarFunction,
             const Function&             simdFunction,
             const int                   maskIndex,
             const bool                  mayHaveSideEffects,
             const SmallVector<bool, 4>& uniformArguments);

    bool            hasMapping        (const Function& scalarFunction) const;
    const Function& getSimdFunction   (const Function& scalarFunction) const;
    int             getMaskIndex      (const Function& scalarFunction) const;
    bool            mayHaveSideEffects(const Function& scalarFunction) const;
    bool            isUniformArgument (const Function& scalarFunction,
                                       const unsigned index) const;
};

}

#endif	/* _FUNCTIONINFOMAP_H */

