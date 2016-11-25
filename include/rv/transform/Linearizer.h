#ifndef RV_TRANSFORM_LINEARIZER_H_
#define RV_TRANSFORM_LINEARIZER_H_

//===- Linearizer.h ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @authors simon
//
// ----------------------------------------------------------------------------
// Partial control linearizer
// Convertes divergent branches to data flow
// ----------------------------------------------------------------------------


#include "rv/vectorizationInfo.h"
#include <llvm/IR/Dominators.h>

#include <vector>
#include <map>

namespace llvm {
  class LoopInfo;
  class BasicBlock;
  class TerminatorInst;
}


namespace rv {

  class VectorizationInfo;
  class Region;

  typedef std::vector<llvm::BasicBlock*> BlockVector;
  typedef std::map<const llvm::BasicBlock*, unsigned> BlockIndex;


  class Linearizer {
    VectorizationInfo & vecInfo;
    Region * region;
    llvm::DominatorTree & dt;
    llvm::LoopInfo & li;

  // topological sorted blocks in the region
    BlockIndex blockIndex;
    BlockVector blocks;
    void addToBlockIndex(llvm::BasicBlock & block);
    void buildBlockIndex(llvm::DomTreeNode & domNode);
    void buildBlockIndex();

    bool needsFolding(llvm::TerminatorInst & branch);

  public:
    Linearizer(VectorizationInfo & _vecInfo, llvm::DominatorTree & _dt, llvm::LoopInfo & _li)
    : vecInfo(_vecInfo)
    , region(vecInfo.getRegion())
    , dt(_dt)
    , li(_li)
    {}

    void run();
  };


}

#endif // RV_TRANSFORM_LINEARIZER_H_
