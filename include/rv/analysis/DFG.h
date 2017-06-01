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
/// CFG is looked at.  If we compute the DFG with forward = false, the result is
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


namespace llvm {
template<bool forward> class DFGBase;

template<bool forward>
class DFGBaseWrapper : public FunctionPass {
    DFGBase<forward>* mDFGBase;
public:
    static char ID;

    DFGBaseWrapper() : FunctionPass(ID), mDFGBase(nullptr) {}

    void getAnalysisUsage(AnalysisUsage& Info) const override
    {
        if (forward)
            Info.addRequired<DominatorTreeWrapperPass>();
        else
            Info.addRequired<PostDominatorTreeWrapperPass>();

        Info.setPreservesAll();
    }

    bool runOnFunction(Function& F) override;

    DFGBase<forward>* getDFG()
    {
        return mDFGBase;
    }
};

template<bool forward>
class DFGBase {
public:
    class Node;

    using nodes_t = ArrayRef<const Node*>;

    class Node {
        friend class DFGBase<forward>;

        explicit Node(BasicBlock* const BB) : BB(BB) { }

    public:
       BasicBlock* getBB() const { return BB; }

        nodes_t preds() const { return preds_; }
        nodes_t succs() const { return succs_; }

    private:
        mutable BasicBlock* BB;
        mutable std::vector<Node*> preds_;
        mutable std::vector<Node*> succs_;
    };

    //----------------------------------------------------------------------------

    DFGBase(const DominatorTreeBase<BasicBlock>& DT) : DT(DT)
    {
        assert (forward == !DT.isPostDominator() && "Wrong dominance tree specified!\n");
    }

    DFGBase(const DFGBase&) = delete;
    DFGBase& operator=(DFGBase) = delete;

    void create(Function& F);

    ~DFGBase();

    Node* operator[](const BasicBlock* const BB) const { return get(BB); }

private:
    const DominatorTreeBase<BasicBlock>& DT;

    Node* get(const BasicBlock* const BB) const
    {
        auto const it = nodes_.find(BB);
        if (it == nodes_.end())
            return nullptr;
        return it->second;
    }

    DenseMap<const BasicBlock*, Node*> nodes_;
};

//------------------------------------------------------------------------------

template<> char DFGBaseWrapper<true>::ID;
template<> char DFGBaseWrapper<false>::ID;

using DFGWrapper = DFGBaseWrapper<true>;
using CDGWrapper = DFGBaseWrapper<false>;
using DFG = DFGBase<true>;  /* Dominance Frontier Graph */
using CDG = DFGBase<false>; /* Control Dependence Graph */
using DFNode = DFG::Node;
using CDNode = CDG::Node;

//------------------------------------------------------------------------------

FunctionPass* createDFGPass();
FunctionPass* createCDGPass();
}


#endif
