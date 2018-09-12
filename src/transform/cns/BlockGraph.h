/*
 * BlockGraph.h
 *
 *  Created on: 26.04.2010
 */

#ifndef BLOCKGRAPH_HPP_
#define BLOCKGRAPH_HPP_

#include <iostream>
#include <map>
#include <vector>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Function.h>

#include "Bitmask.h"

namespace rv {

class BlockGraph {
public:
  typedef BoolVector SubgraphMask;

private:
  typedef std::vector<BoolVector> DirectedGraph;
  typedef std::vector<llvm::BasicBlock *> BlockVector;

  DirectedGraph graph;
  BlockVector labels;

  BlockGraph(DirectedGraph _graph, BlockVector _labels);

  void resetGraph(unsigned size);

  BlockGraph(unsigned size) { resetGraph(size); }

  DirectedGraph createExtendedGraph(SubgraphMask &mask, unsigned size) const;
  static void copyLeavingEdges(DirectedGraph &destGraph, unsigned destNode,
                               DirectedGraph sourceGraph, unsigned sourceNode);
  static void copyIncomingEdges(DirectedGraph &destGraph, unsigned destNode,
                                DirectedGraph sourceGraph, unsigned sourceNode);

public:
  inline void setLabel(unsigned index, llvm::BasicBlock *_label) {
    assert(index < labels.size());
    labels[index] = _label;
  }

  inline llvm::BasicBlock *getLabel(unsigned index) const {
    assert(index < labels.size());
    return labels[index];
  }

  inline BlockGraph::SubgraphMask mergedPredecessors(SubgraphMask mask) const {
    SubgraphMask result(getSize(), false);

    for (unsigned i = 0; i < getSize(); ++i) {
      if (mask[i]) {
        result = OR(result, graph[i]);
      }
    }

    return result;
  }

  inline BlockGraph::SubgraphMask mergedSuccessors(SubgraphMask mask) const {
    SubgraphMask result(getSize(), false);

    for (unsigned i = 0; i < getSize(); ++i) {
      if (!mask[i] && EXISTS(AND(graph[i], mask))) {
        result[i] = true;
      }
    }

    return result;
  }
  inline bool getEdge(const SubgraphMask &mask, unsigned from, unsigned to) const {
    return mask[to] && mask[from] && graph[to][from];
  }

  inline void setEdge(unsigned from, unsigned to, bool value) {
    graph[to][from] = value;
  }

  inline void setAllEdges(const SubgraphMask &sources,
                          const SubgraphMask &targets, bool value) {
    for (unsigned in = 0; in < sources.size(); ++in)
      if (sources[in])
        for (unsigned out = 0; out < targets.size(); ++out)
          if (targets[out])
            setEdge(in, out, value);
  }

  inline void setAllEdges(unsigned source, const SubgraphMask &targets,
                          bool value) {
    for (unsigned out = 0; out < targets.size(); ++out)
      if (targets[out])
        setEdge(source, out, value);
  }

  inline void setAllEdges(const SubgraphMask &sources, unsigned target,
                          bool value) {
    for (unsigned in = 0; in < sources.size(); ++in)
      if (sources[in])
        setEdge(in, target, value);
  }

  llvm::BasicBlock *getBasicBlock(unsigned index);
  std::string getNodeName(unsigned index) const;
  SubgraphMask createMask() const;

  // BlockGraph(llvm::Function & func);

  static BlockGraph CreateFromFunction(llvm::Function &func,
                                       SubgraphMask &oMask);

  ~BlockGraph();

  /*
   * splits the node at @index in the graph. @mask will be adapted to the
   * returned graph
   */
  BlockGraph createSplitGraph(SubgraphMask &mask, unsigned index);

  unsigned getSize(const SubgraphMask &mask) const;
  unsigned getSize() const;

  inline SubgraphMask getPredecessors(SubgraphMask mask, unsigned node) const {
    return AND(mask, graph[node]);
  }

  unsigned getNumPredecessors(const SubgraphMask &mask, unsigned index) const;
  unsigned getNumPredecessorsNonReflexive(const SubgraphMask &mask,
                                      unsigned index) const;

  unsigned getNumSuccessors(const SubgraphMask &mask, unsigned index) const;
  // uint getNumSuccessorsNonReflexive(const SubgraphMask & mask, uint index)
  // const;
  /*
   * returns -1 if there is not a unique incoming edge
   */
  int getSinglePredecessorNonReflexive(const SubgraphMask &mask,
                                       unsigned index) const;
  int getSingleSuccessorNonReflexive(const SubgraphMask &mask,
                                     unsigned index) const;

  // transformations
  void removeNode(SubgraphMask &mask, unsigned index) const;
  void contractNode(SubgraphMask &mask, unsigned mergedNode, unsigned destNode);

  /*
   * removes all edges from the E that contain vertices not occurring in the
   * subgraph mask
   */
  void enforceSubgraph(SubgraphMask &mask);

  void dumpGraphviz(const SubgraphMask &mask, std::ostream &out) const;
  void dump(const SubgraphMask &mask) const;
  static void dumpInternal(const DirectedGraph &graph);

  SubgraphMask getExitingNodes(const SubgraphMask &scope,
                               const SubgraphMask &mask) const;
  SubgraphMask getEntryNodes(const SubgraphMask &scope,
                             const SubgraphMask &mask) const;

  BlockGraph transposed() const;
};
}

#endif /* BLOCKGRAPH_HPP_ */
