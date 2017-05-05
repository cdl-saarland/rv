//===- maskGraphUtils.h ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//

#ifndef _MASKGRAPHUTILS_H
#define _MASKGRAPHUTILS_H

#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/Twine.h>
#include <llvm/Support/raw_ostream.h>

#include <memory> // shared_ptr, weak_ptr

namespace llvm {
class Value;
class BasicBlock;
class Instruction;
class Loop;
class Twine;
}

using namespace llvm;


namespace rv {

namespace MaskGraphUtils {

enum NodeType
{
    CONSTANT,
    VALUE,
    NEGATE,
    CONJUNCTION,
    DISJUNCTION,
    SELECT,
    PHI,
    LOOPMASKPHI,
    LOOPEXITPHI,
    LOOPEXITUPDATE,
    REFERENCE
};

// Use shared_ptr for Mask objects that can
// directly be deleted via MaskAnalysis::mMaskMap.
// Use weak_ptr for operands to cope with cycles.
// This means that when deleting the object,
// the operands may already have been deleted,
// but in that case the operand is set to
// nullptr.
struct Mask;
typedef std::shared_ptr<Mask> MaskPtr;
typedef std::weak_ptr<Mask>   MaskWeakPtr;

struct Mask
{
    const unsigned              mID;
    const NodeType              mType;
    SmallVector<MaskWeakPtr, 2> mOperands;
    SmallVector<BasicBlock*, 2> mIncomingDirs;
    Value*                      mValue;
    Instruction*                mInsertPoint;
    std::string                 mName = "";

    Mask(const NodeType type, Instruction* insertPoint);
    ~Mask();
    bool operator==(const Mask& other) const;
    void print(raw_ostream& o) const;
};

// Objects of this type are only referenced in the blockMap.
struct BlockMaskInfo
{
    BasicBlock*             mBlock;
    MaskPtr                 mEntryMask;
    SmallVector<MaskPtr, 2> mExitMasks;

    ~BlockMaskInfo();
    void print(raw_ostream& o) const;
};

struct LoopMaskInfo
{
    const Loop* mLoop;
    MaskPtr     mMaskPhi;
    MaskPtr     mCombinedLoopExitMask;

    ~LoopMaskInfo();
    void print(raw_ostream& o) const;
};

// Objects of this type are only referenced in the loopExitMap.
struct LoopExitMaskInfo
{
    BasicBlock* mBlock;  // Exiting block.
	BasicBlock* mTarget; // Exit block.

    // The innermost loop that is left (not necessarily the one this
    // node is connected to!).
	const Loop* mInnermostLoop;

    // The top level loop of this exit.
	const Loop* mTopLevelLoop;

    // The 'or' operations that update the current loop's exit mask.
    // This is the accumulation of all instances that left over this edge
    // in the current iteration of the corresponding loop (if any).
    // NOTE: The mask that stores which instances left over this edge in
    //       *any* iteration of *any* of the enclosing loops is simply the
    //       update operation of the top level loop.
	typedef DenseMap<const Loop*, MaskPtr> LoopMaskMapType;
	LoopMaskMapType mMaskUpdateOpMap;

	// Map of phi operations of each loop & related mask update operations.
	// NOTE: This also includes the innermost loop.
	LoopMaskMapType mMaskPhiMap;

    ~LoopExitMaskInfo();
    void print(raw_ostream& o) const;
};

} // namespace MaskGraphUtils
} // namespace rv


#endif /* _MASKGRAPHUTILS_H */
