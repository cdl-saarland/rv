/*
 * SplitTree.h
 *
 *  Created on: 29.04.2010
 */

#ifndef SPLITTREE_HPP_
#define SPLITTREE_HPP_

#include <set>

#include "BlockGraph.h"

namespace rv {
namespace cns {
class SplitTree {
  SplitTree *parent;
  BlockGraph::SubgraphMask mask;
  BlockGraph graph;
  unsigned splitNode;
  std::set<SplitTree *> children;
  int depth;

  void addChild(SplitTree *);
  void removeChild(SplitTree *);

  SplitTree(SplitTree *_parent, BlockGraph::SubgraphMask _mask,
            BlockGraph _graph, unsigned _splitNode, int _depth);

public:
  int getDepth() const;
  /*
   * creates a root node
   */
  SplitTree(BlockGraph::SubgraphMask _mask, BlockGraph _graph);
  ~SplitTree();

  std::set<SplitTree *> &getChildren();
  unsigned getNumChildren();
  unsigned getSplitNode() const;
  bool isRoot() const;
  SplitTree *getParent() const;

  /*
   * add a split to the tree
   */
  SplitTree *pushSplit(BlockGraph::SubgraphMask mask, BlockGraph graph,
                       unsigned splitNode);

  void dump();
};
}
}

#endif /* SPLITTREE_HPP_ */
