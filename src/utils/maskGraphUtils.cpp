//===- maskGraphUtils.cpp ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @author karrenberg

#include "rv/utils/maskGraphUtils.h"

#include <llvm/IR/Value.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/Analysis/LoopInfo.h>

#include "rvConfig.h"


using namespace llvm;


namespace rv {
namespace MaskGraphUtils {

static unsigned staticMaskID = 0;

Mask::Mask(const NodeType type,
           Instruction*   insertPoint)
: mID(staticMaskID++),
        mType(type),
        mValue(nullptr),
        mInsertPoint(insertPoint)
{
}

Mask::~Mask()
{
    // Nothing to do due to weak pointers.
}

bool
Mask::operator==(const Mask& other) const
{
    if (this == &other) return true;

    if (mType != other.mType) return false;
    if (mOperands.size() != other.mOperands.size()) return false;
    if (mIncomingDirs.size() != other.mIncomingDirs.size()) return false;
    if (mValue != other.mValue) return false;
    if (mInsertPoint != other.mInsertPoint) return false;

    for (unsigned i=0, e=mOperands.size(); i<e; ++i)
    {
        if (mOperands[i].lock() != other.mOperands[i].lock()) return false;
    }
    for (unsigned i=0, e=mIncomingDirs.size(); i<e; ++i)
    {
        if (mIncomingDirs[i] != other.mIncomingDirs[i]) return false;
    }

    return true;
}

void
Mask::print(raw_ostream& o) const
{
    o << mID << ": ";
    if (mValue)
    {
        o << *mValue << "\n";
        return;
    }

    switch (mType)
    {
        case CONSTANT: o << "CONST( "; break;
        case VALUE: o << "CMP( "; break;
        case NEGATE: o << "NEG( "; break;
        case CONJUNCTION: o << "AND( "; break;
        case DISJUNCTION: o << "OR( "; break;
        case SELECT: o << "SELECT( "; break;
        case PHI: o << "PHI( "; break;
        case LOOPMASKPHI: o << "LOOPMASKPHI( "; break;
        case LOOPEXITPHI: o << "LOOPEXITPHI( "; break;
        case LOOPEXITUPDATE: o << "LOOPEXITUPDATE( "; break;
        case REFERENCE: o << "REF( "; break;
        default:
        {
            assert (false && "unknown mask type found!");
        }
    }

    for (unsigned i=0, e=mOperands.size(); i<e; ++i)
    {
        o << mOperands[i].lock()->mID;
        if (i+1 != e) o << ", ";
    }

    if (mIncomingDirs.size() > 0)
    {
        o << " [ ";
        for (unsigned i=0, e=mIncomingDirs.size(); i<e; ++i)
        {
            o << mIncomingDirs[i]->getName();
            if (i+1 != e) o << ", ";
        }
        o << " ] ";
    }

    o << " )";
}

BlockMaskInfo::~BlockMaskInfo()
{
    mEntryMask.reset();

    for (auto &it : mExitMasks)
    {
        MaskPtr mp = it;
        mp.reset();
    }
}

void
BlockMaskInfo::print(raw_ostream& o) const
{
    o << "BlockMaskInfo for block '" << mBlock->getName() << "':\n";
    o << "  entry mask : "; mEntryMask->print(o); o << "\n";
    unsigned i=0;
    for (auto &it : mExitMasks)
    {
        MaskPtr mp = it;
        o << "  exit mask " << i++ << ": ";
        mp->print(o);
        o << "\n";
    }
}

LoopMaskInfo::~LoopMaskInfo()
{
    mMaskPhi.reset();
    mCombinedLoopExitMask.reset();
}

void
LoopMaskInfo::print(raw_ostream& o) const
{
    o << "LoopMaskInfo for loop with header '" << mLoop->getHeader()->getName() << "':\n";
    o << "  mask phi          : "; if (mMaskPhi) mMaskPhi->print(o); else o << "null"; o << "\n";
    o << "  combined exit mask: "; if (mCombinedLoopExitMask) mCombinedLoopExitMask->print(o); else o << "null"; o << "\n";
}

LoopExitMaskInfo::~LoopExitMaskInfo()
{
    for (auto &it : mMaskPhiMap)
    {
        it.second.reset();
    }
    for (auto &it : mMaskUpdateOpMap)
    {
        it.second.reset();
    }
}

void
LoopExitMaskInfo::print(raw_ostream& o) const
{
    o << "Loop exit: " << mBlock->getName();
    o << " -> " << mTarget->getName() << "\n";
    for (auto &it : mMaskPhiMap)
    {
        o << "  mask phi (depth " << it.first->getLoopDepth() << "):";
        it.second->print(o);
        o << "\n";
    }
    for (auto &it : mMaskUpdateOpMap)
    {
        o << "  mask update (depth " << it.first->getLoopDepth() << "):";
        it.second->print(o);
        o << "\n";
    }
}

} // namespace rv

} // namespace MaskGraphUtils
