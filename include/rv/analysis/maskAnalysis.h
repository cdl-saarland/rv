//===- maskAnalysis.h ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//


#ifndef _MASKANALYSIS_H
#define _MASKANALYSIS_H

#include "rv/VectorizationInfoProxyPass.h"
#include "rv/vectorizationInfo.h"
#include "rv/region/Region.h"
#include "rv/utils/maskGraphUtils.h"

#include <llvm/Pass.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/DenseSet.h>
#include <llvm/Transforms/Utils/ValueMapper.h>

#include <memory> // shared_ptr, weak_ptr

namespace llvm {
class Value;
class BasicBlock;
class Loop;
class LoopInfo;
}

using namespace llvm;
using namespace rv::MaskGraphUtils;
using rv::VectorizationInfo;
using rv::PlatformInfo;


namespace rv {
  class PlatformInfo;

class MaskAnalysis
{
public:
    MaskAnalysis(VectorizationInfo& vecInfo,
		 const LoopInfo&    Loopinfo);
    ~MaskAnalysis();

    bool analyze(Function& F);

    void print(raw_ostream& O, const Module* M) const;

    void invalidateInsertPoints();

    typedef DenseMap<const Value*, Value*> MaskValueMapType;
    // This function maps mask values according to the given map.
    // It is used after reg2mem/mem2reg after CFG linearization.
    void mapMaskValues(MaskValueMapType& valueMap);
    // This function maps the entire mask info structure including blocks.
    // It is used during function vectorization after cloning the scalar code into
    // the SIMD prototype.
    void mapMaskInformation(ValueToValueMapTy& valueMap);

    Value* getEntryMask           (const BasicBlock& block) const;
    Value* getExitMask            (const BasicBlock& block,
                                   const unsigned    index) const;
    Value* getExitMask            (const BasicBlock& block,
                                   const BasicBlock& direction) const;
    Value* getLoopMaskPhi         (const Loop& loop) const;
    Value* getCombinedLoopExitMask(const Loop& loop) const;
    // returns the exit mask at @exiting
    // this will return nullptr, if the exit edge kills the loop
    // a loop is killed at an exit if all remaining instances leave at the same iteration
    // In that cast the state of the last iteration can be used by outside users without any masking
    Value* getActualLoopExitMask(BasicBlock & exiting);
    Value* getLoopExitMaskPhi     (const Loop&       loop,
                                   const BasicBlock& exitingBlock) const;
    Value* getLoopExitMaskUpdate  (const Loop&       loop,
                                   const BasicBlock& exitingBlock) const;

    void setEntryMask         (const BasicBlock& block,
                               Value*            value);
    void setExitMask          (const BasicBlock& block,
                               const unsigned    index,
                               Value*            value);
    void setExitMask          (const BasicBlock& block,
                               const BasicBlock& direction,
                               Value*           value);
    void setLoopExitMaskPhi   (const Loop&       loop,
                               const BasicBlock& exitingBlock,
                               Value*            value);
    void setLoopExitMaskUpdate(const Loop&       loop,
                               const BasicBlock& exitingBlock,
                               Value*            value);

    void copyMaskInfoToNewBlock(BasicBlock*       newBlock,
                                const BasicBlock& oldBlock);
    void clearEntryMask(const BasicBlock& block);
    void clearExitMasks(const BasicBlock& block);
    void updateEntryMask(const BasicBlock& block,
                         Value*            newMask,
                         Instruction*      insertPoint);
    void updateExitMasks(const BasicBlock& block,
                         Value*            value0,
                         Value*            value1,
                         Instruction*      insertPoint);
#if 0
    void updateExitMask(const BasicBlock& block,
                        const unsigned    index,
                        Value*            value);
    void resizeExitMasks(const BasicBlock& block,
                         const unsigned    newSize);
#endif
    void removeExitMask(const BasicBlock& block,
                        const unsigned    index);
    void removeExitMask(const BasicBlock& block,
                        const BasicBlock& direction);

    MaskPtr getEntryMaskPtr           (const BasicBlock& block) const;
    MaskPtr getExitMaskPtr            (const BasicBlock& block,
                                       const unsigned    index) const;
    MaskPtr getExitMaskPtr            (const BasicBlock& block,
                                       const BasicBlock& direction) const;
    MaskPtr getLoopMaskPtrPhi         (const Loop& loop) const;
    MaskPtr getLoopExitMaskPtrPhi     (const Loop&       loop,
                                       const BasicBlock& exitingBlock) const;
    MaskPtr getLoopExitMaskPtrUpdate  (const Loop&       loop,
                                       const BasicBlock& exitingBlock) const;
    MaskPtr getCombinedLoopExitMaskPtr(const Loop& loop) const;

private:
    VectorizationInfo& vecInfo;
    const LoopInfo&    mLoopInfo;

    DenseMap<const BasicBlock*, BlockMaskInfo*>    mBlockMap;
    DenseMap<const Loop*,       LoopMaskInfo*>     mLoopMaskMap;
    DenseMap<const BasicBlock*, LoopExitMaskInfo*> mLoopExitMap;
    std::vector<MaskPtr>                           mMasks;

    Value * mConstBoolFalse;
    Value * mConstBoolTrue;

    inline MaskPtr createMask(const NodeType type,
                              Instruction*   insertPoint);

	BlockMaskInfo* getOrCreateBMIFor(BasicBlock* block);

    void    createMaskGraph   (Function&                f);
    void    recCreateMaskGraph(BasicBlock*              block,
                               DenseSet<BasicBlock*>&   markedBlocks);
    MaskPtr createEntryMask   (BasicBlock*              block);
    void    createExitMasks   (BasicBlock*              block,
                               MaskPtr                  entryMask,
                               SmallVector<MaskPtr, 2>& exitMasks);

    void createLoopExitMasks(Loop* loop);

    void setEntryMaskPtr         (const BasicBlock& block,
                                  MaskPtr           mask);
    void setExitMaskPtr          (const BasicBlock& block,
                                  const unsigned    index,
                                  MaskPtr           mask);
    void setExitMaskPtr          (const BasicBlock& block,
                                  const BasicBlock& direction,
                                  MaskPtr           mask);
    void setLoopExitMaskPtrPhi   (const Loop&       loop,
                                  const BasicBlock& exitingBlock,
                                  MaskPtr           mask);
    void setLoopExitMaskPtrUpdate(const Loop&       loop,
                                  const BasicBlock& exitingBlock,
                                  MaskPtr           mask);
};

}

class MaskAnalysisWrapper : public FunctionPass {
  rv::MaskAnalysis* mMaskAnalysis;
public:
  static char ID __attribute__((unused)); // Pass identification, replacement for typeid.

  MaskAnalysisWrapper();

  rv::MaskAnalysis* getMaskAnalysis() const;

  virtual void releaseMemory   ();
  virtual void getAnalysisUsage(AnalysisUsage &AU) const;
  virtual bool doInitialization(Module& M);
  virtual bool doFinalization  (Module& M);
  virtual bool runOnFunction   (Function& F);
  virtual void print           (raw_ostream& O, const Module* M) const;
};



// Forward declaration of initializer and public interface.
namespace llvm {
void initializeMaskAnalysisWrapperPass(PassRegistry&);
FunctionPass* createMaskAnalysisPass();
}

#endif /* _MASKANALYSIS_H */
