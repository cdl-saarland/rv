#include "rv/analysis/DFG.h"
#include "llvm/IR/CFG.h"


using namespace rv;

template<> char DFGBaseWrapper<true>::ID = 0;
template<> char DFGBaseWrapper<false>::ID = 0;

template<>
bool DFGBaseWrapper<true>::runOnFunction(llvm::Function& F)
{
  llvm::DominatorTreeBase<llvm::BasicBlock, true>& tree = static_cast<llvm::DominatorTreeBase<llvm::BasicBlock, true>&>(getAnalysis<llvm::PostDominatorTreeWrapperPass>().getPostDomTree());

    mDFGBase = new DFGBase<true>(tree);
    mDFGBase->create(F);
    return false;
}

template<>
bool DFGBaseWrapper<false>::runOnFunction(llvm::Function& F)
{
  llvm::DominatorTreeBase<llvm::BasicBlock, false>& tree = static_cast<llvm::DominatorTreeBase<llvm::BasicBlock, false>&>(getAnalysis<llvm::DominatorTreeWrapperPass>().getDomTree());

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
void DFGBase<backward>::create(llvm::Function& F) {
  auto const getIdom = [&](const llvm::BasicBlock& BB) {
    auto idomNode = DT.getNode(const_cast<llvm::BasicBlock*>(&BB));
    auto idom = idomNode ? idomNode->getIDom() : nullptr;
    return idom ? idom->getBlock() : nullptr;
  };

  for (auto & BB : F) {
    nodes_[&BB] = new Node(&BB);
  }

  for (const auto & BB : F) {
    Node* df_node = get(BB);
    const llvm::BasicBlock* idom = getIdom(BB);
    if (idom == nullptr) continue;

    if (!backward) { // Dominance Frontier Graph
      for (auto pi : llvm::predecessors(&BB)) {
        const llvm::BasicBlock* runner = pi;
        while (runner != idom) {
          Node* df_runner = get(*runner);
          df_node->succs_.push_back(df_runner);
          df_runner->preds_.push_back(df_node);
          runner = getIdom(*df_runner->getBB());
        }
      }
    } else { // Control Dependence Graph
      for (auto si : llvm::successors(&BB)) {
        const llvm::BasicBlock* runner = si;
        while (runner != idom) {
          Node* const df_runner = get(*runner);
          df_node->succs_.push_back(df_runner);
          df_runner->preds_.push_back(df_node);
          runner = getIdom(*df_runner->getBB()); // postIDom
        }
      }
    }
  }
}

//------------------------------------------------------------------------------

namespace rv {
template class DFGBase<true>;
template class DFGBase<false>;

llvm::FunctionPass* createDFGPass()
{
    return new DFGBaseWrapper<false>();
}

llvm::FunctionPass* createCDGPass()
{
    return new DFGBaseWrapper<true>();
}
}
