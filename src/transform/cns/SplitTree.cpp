/*
 * SplitTree.cpp
 *
 *  Created on: 29.04.2010
 */

#include "SplitTree.h"

namespace rv {
namespace cns {

SplitTree::SplitTree(BlockGraph::SubgraphMask _mask, BlockGraph _graph)
    : parent(NULL), mask(_mask), graph(_graph), depth(0) {}

void SplitTree::addChild(SplitTree *child) { children.insert(child); }

void SplitTree::removeChild(SplitTree *child) { children.erase(child); }

SplitTree::SplitTree(SplitTree *_parent, BlockGraph::SubgraphMask _mask,
                     BlockGraph _graph, unsigned _splitNode, int _depth)
    : parent(_parent), mask(_mask), graph(_graph), splitNode(_splitNode),
      depth(_depth) {}

SplitTree::~SplitTree() {
  if (parent)
    parent->removeChild(this);
}

SplitTree *SplitTree::pushSplit(BlockGraph::SubgraphMask mask, BlockGraph graph,
                                unsigned splitNode) {
  SplitTree *tree = new SplitTree(this, mask, graph, splitNode, getDepth() + 1);

  if (parent) {
    parent->addChild(tree);
  }

  return tree;
}

std::set<SplitTree *> &SplitTree::getChildren() { return children; }

unsigned SplitTree::getNumChildren() { return children.size(); }

unsigned SplitTree::getSplitNode() const { return splitNode; }

bool SplitTree::isRoot() const { return parent == NULL; }

SplitTree *SplitTree::getParent() const { return parent; }

int SplitTree::getDepth() const { return depth; }

void SplitTree::dump() {
  if (parent) {
    parent->dump();
    std::cerr << "(" << getDepth() << ") split node index=" << splitNode
              << ";\n mask ";
  } else {
    std::cerr << "root split node; mask=";
  }

  graph.dump(mask);
  std::cerr << "\n";
}
}

}
