//===- DFG.h - Implements the Dominance Frontier Graph ----------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
/// \file
///
/// Implements a Dominance Frontier Graph (DFG) parametric in the direction the
/// CFG is looked at.  If we compute the DFG with backward = false, the result is
/// a Control Dependence Graph (CDG).
///
/// In the DFG, the predecessors of a node form the dominance frontier set.
/// A node in the CDG is control dependent on its predecessors.
///
//===----------------------------------------------------------------------===//

#ifndef LLVM_ANALYSIS_DFG_H
#define LLVM_ANALYSIS_DFG_H


#include <vector>
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Analysis/PostDominators.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/ValueMap.h"
#include "llvm/Pass.h"


namespace rv {

template<bool backward> class DFGBase;

template<bool backward>
class DFGBaseWrapper : public llvm::FunctionPass {
    DFGBase<backward>* mDFGBase;
public:
    static char ID;

    DFGBaseWrapper() : FunctionPass(ID), mDFGBase(nullptr) {}

    void getAnalysisUsage(llvm::AnalysisUsage& Info) const override
    {
        if (backward)
            Info.addRequired<llvm::DominatorTreeWrapperPass>();
        else
            Info.addRequired<llvm::PostDominatorTreeWrapperPass>();

        Info.setPreservesAll();
    }

    bool runOnFunction(llvm::Function& F) override;

    DFGBase<backward>* getDFG()
    {
        return mDFGBase;
    }
};

template<bool backward>
class DFGBase {
public:
    class Node;

    using nodes_t = llvm::ArrayRef<const Node*>;

    class Node {
        friend class DFGBase<backward>;

        explicit Node(llvm::BasicBlock* const BB) : BB(BB) { }

    public:
        llvm::BasicBlock* getBB() const { return BB; }

        nodes_t preds() const { return preds_; }
        nodes_t succs() const { return succs_; }

    private:
        mutable llvm::BasicBlock* BB;
        mutable std::vector<Node*> preds_;
        mutable std::vector<Node*> succs_;
    };

    //----------------------------------------------------------------------------

    DFGBase(const llvm::DominatorTreeBase<llvm::BasicBlock, backward>& DT) : DT(DT)
    {
        assert (backward == DT.isPostDominator() && "Wrong dominance tree specified!\n");
    }

    DFGBase(const DFGBase&) = delete;
    DFGBase& operator=(DFGBase) = delete;

    void create(llvm::Function& F);

    ~DFGBase();

    Node* operator[](const llvm::BasicBlock* const BB) const { return get(*BB); }

private:
    const llvm::DominatorTreeBase<llvm::BasicBlock, backward>& DT;

    Node* get(const llvm::BasicBlock& BB) const
    {
        auto const it = nodes_.find(&BB);
        if (it == nodes_.end())
            return nullptr;
        return it->second;
    }

    llvm::DenseMap<const llvm::BasicBlock*, Node*> nodes_;
};

//------------------------------------------------------------------------------

template<> char DFGBaseWrapper<true>::ID;
template<> char DFGBaseWrapper<false>::ID;

using DFGWrapper = DFGBaseWrapper<false>;
using CDGWrapper = DFGBaseWrapper<true>;
using DFG = DFGBase<false>;  /* Dominance Frontier Graph */
using CDG = DFGBase<true>; /* Control Dependence Graph */
using DFNode = DFG::Node;
using CDNode = CDG::Node;

//------------------------------------------------------------------------------

llvm::FunctionPass* createDFGPass();
llvm::FunctionPass* createCDGPass();

}


#endif
