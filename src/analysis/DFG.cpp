#include "rv/analysis/DFG.h"
#include "llvm/IR/CFG.h"


using namespace llvm;

template<> char DFGBaseWrapper<true>::ID = 0;
template<> char DFGBaseWrapper<false>::ID = 0;

template<>
bool DFGBaseWrapper<true>::runOnFunction(Function& F)
{
    DominatorTreeBase<BasicBlock, true>& tree = static_cast<DominatorTreeBase<BasicBlock, true>&>(getAnalysis<PostDominatorTreeWrapperPass>().getPostDomTree());

    mDFGBase = new DFGBase<true>(tree);
    mDFGBase->create(F);
    return false;
}

template<>
bool DFGBaseWrapper<false>::runOnFunction(Function& F)
{
    DominatorTreeBase<BasicBlock, false>& tree = static_cast<DominatorTreeBase<BasicBlock, false>&>(getAnalysis<DominatorTreeWrapperPass>().getDomTree());

    mDFGBase = new DFGBase<false>(tree);
    mDFGBase->create(F);
    return false;
}




template<bool backward>
DFGBase<backward>::~DFGBase() {
  for (auto it : nodes_)
    delete it.second;
}

template<bool backward>
void DFGBase<backward>::create(Function& F) {
  auto const getIdom = [&](const BasicBlock* const BB) {
    auto idomNode = DT.getNode(const_cast<BasicBlock*>(BB));
    auto idom = idomNode ? idomNode->getIDom() : nullptr;
    return idom ? idom->getBlock() : nullptr;
  };

  for (Function::iterator it = F.begin(), E = F.end(); it != E; ++it) {
    BasicBlock* const BB = &*it;
    nodes_[BB] = new Node(BB);
  }

  for (Function::iterator it = F.begin(), E = F.end(); it != E; ++it) {
    BasicBlock* const BB = &*it;
    Node* const df_node = get(BB);
    BasicBlock* const idom = getIdom(BB);
    if (idom == nullptr) continue;

    if (!backward) { /* Dominance Frontier Graph */
      pred_iterator pi       = pred_begin(BB);
      pred_iterator const E  = pred_end(BB);
      for (; pi != E; ++pi) {
        BasicBlock* runner = *pi;
        while (runner != idom) {
          Node* const df_runner = get(runner);
          df_node->succs_.push_back(df_runner);
          df_runner->preds_.push_back(df_node);
          runner = getIdom(df_runner->getBB());
        }
      }
    } else { /* Control Dependence Graph */
      succ_iterator si       = succ_begin(BB);
      succ_iterator const E  = succ_end(BB);
      for (; si != E; ++si) {
        BasicBlock* runner = *si;
        while (runner != idom) {
          // if (runner == BB)
          //   break;
          Node* const df_runner = get(runner);
          df_node->succs_.push_back(df_runner);
          df_runner->preds_.push_back(df_node);
          runner = getIdom(df_runner->getBB()); // postIDom
        }
      }
    }
  }
}

//------------------------------------------------------------------------------

namespace llvm {
template class DFGBase<true>;
template class DFGBase<false>;

FunctionPass* createDFGPass()
{
    return new DFGBaseWrapper<false>();
}

FunctionPass* createCDGPass()
{
    return new DFGBaseWrapper<true>();
}
}
