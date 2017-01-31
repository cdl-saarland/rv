
#include "rv/analysis/DFG.h"
#include "llvm/IR/CFG.h"


using namespace llvm;

template<bool forward>
char DFGBaseWrapper<forward>::ID = 0;

template<bool forward>
bool DFGBaseWrapper<forward>::runOnFunction(Function& F)
{
    DominatorTreeBase<BasicBlock>& tree = forward ?
                                          getAnalysis<DominatorTreeWrapperPass>().getDomTree() :
                                          *getAnalysis<PostDominatorTree>().DT;

    mDFGBase = new DFGBase<forward>(tree);
    mDFGBase->create(F);
    return false;
}

template<bool forward>
DFGBase<forward>::~DFGBase() {
  for (auto it : nodes_)
    delete it.second;
}

template<bool forward>
void DFGBase<forward>::create(Function& F) {
  auto const getIdom = [&](const BasicBlock* const BB) {
    auto idom = DT.getNode(const_cast<BasicBlock*>(BB))->getIDom();
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

    if (forward) { /* Dominance Frontier Graph */
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
    return new DFGBaseWrapper<true>();
}

FunctionPass* createCDGPass()
{
    return new DFGBaseWrapper<false>();
}
}
