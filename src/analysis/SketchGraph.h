//===- SketchGraph.h -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//


#ifndef SRC_ANALYSIS_SKETCHGRAPH_H_
#define SRC_ANALYSIS_SKETCHGRAPH_H_

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Dominators.h>

#include <set>
#include <map>
#include "analysisCfg.h"

// TODO move to separate header
template<class T>
static void setcpy(std::set<T> & destSet, const std::set<T> & srcSet) {
	for (auto it : srcSet) {
		destSet.insert(it);
	}
}

class SketchNode; // forward
typedef std::set<SketchNode*> NodeSet;

struct SketchNode : public NodeSet {
	SketchNode() : NodeSet() {}
};

typedef std::map<SketchNode*, const llvm::BasicBlock*> NodeToBlockMap;
typedef std::map<const llvm::BasicBlock*,SketchNode*> BlockToNodeMap;

class ContractionMap; // forward

class SketchGraph {
	typedef SketchNode Node;

	NodeToBlockMap nodeToBlockMap;
	BlockToNodeMap blockToNodeMap;

protected:
	// Will copy over all edges from the underlying CFG where source and destination are registered nodes
	// This is only possible on un-contracted graphs (thus the protected scope)
	void initializeWithCoveredEdges();

public:
// Generic helper functions
	void unlinkNode(SketchNode * node); // unlink the node from its predecessors
	void eraseNode(SketchNode * node); // unlink the node from its predecessors, remove it from all indices and delete it
	void eraseNodesExt(NodeSet & nodeSet); // unlink the node from its predecessors, remove it from all indices and delete it

	NodeSet getPredecessors(SketchNode * node);
	uint getNumNodes() const { return nodeToBlockMap.size(); }

// debug output
	void dump() const;
	void dumpSet(const NodeSet & nodes) const;

// Basic methods
	SketchGraph();
	~SketchGraph();

	// adds the node without linking it
	SketchNode * push(const llvm::BasicBlock * block);

	const llvm::BasicBlock * getBlock(SketchNode & node) const {
		NodeToBlockMap::const_iterator it = nodeToBlockMap.find(&node);
		return (it != nodeToBlockMap.end()) ? it->second : nullptr;
	}
	SketchNode * getNode(const llvm::BasicBlock & block) const {
		BlockToNodeMap::const_iterator it = blockToNodeMap.find(&block);
		return (it != blockToNodeMap.end()) ? it->second : nullptr;
	}


	// Encode an imediate dominator node as SketchGraph
	// Outputs all nodes with exiting edges in oExiting
	static SketchGraph * createFromDominatorNode(const llvm::DominatorTree & domTree, const llvm::BasicBlock & domRoot, NodeSet & oExiting);

	NodeToBlockMap::iterator begin() { return nodeToBlockMap.begin(); }
	NodeToBlockMap::iterator end() {  return nodeToBlockMap.end(); }
	NodeToBlockMap::const_iterator begin() const { return nodeToBlockMap.begin(); }
	NodeToBlockMap::const_iterator end() const { return nodeToBlockMap.end(); }

	typedef NodeToBlockMap::iterator iterator;
	typedef NodeToBlockMap::const_iterator const_iterator;

	// eliminate all paths leading to sinks that are not in @keepSet
	// this invalidates SketchNodes that only reach dead sinks
	void
	eliminateDeadEnds(NodeSet & keepSet);


	// eliminate all nodes that are unreachable from @sourceNode
	void eliminateUnreachable(SketchNode * sourceNode);

	// Collect all nodes that reach the sink
	// this will assume nodes in @oReaching have already been visited
	void collectReachingNodes(SketchNode & sinkNode, NodeSet & oReaching);
	// the same but accepts a set of nodes to each
	void collectReachingNodesExt(const NodeSet & sinkNodeSet, NodeSet & oReaching);


	// Contracts all confluent nodes in the graph and returns a relation between the contracted graph and sets of nodes of the original graph in oConfluentNOdes
	// n nodes are confluent, if they share an unique successor node
	// However, _core_ nodes will not be contracted
	// the resulting graph groups all nodes that can not reach a coreNode without crossing each other in the same confluenceNode
	// this is used in vectorization analysis to efficiently compute Ralf's disjoint path criterion
	void
	contractLocalConfluence(const NodeSet & coreNodes, ContractionMap * oConfluentNodes);

	// Derives a confluence-contracted graph for this graph
	SketchGraph *
	generateConfluenceGraph(const NodeSet & coreNodes, ContractionMap * oConfluentNodes);
};


class ContractionMap {
	std::map<SketchNode*, SketchNode*> rootMap; // original -> contracted
	std::map<SketchNode*, NodeSet*> confluenceMap; // contracted -> original

public:
	ContractionMap();
	~ContractionMap();

	// does a look-up in the original graph and returns the surviving root node of the contactedGraph
	// that is a remaining root basic block after contraction
	SketchNode * getRootForNode(SketchNode * node) const ;
	NodeSet * getNodeSetForRoot(SketchNode * root) const;

	// find the whole confluent set given an representative
	NodeSet * getNodeSetForElement(SketchNode * node) const {
		SketchNode * root = getRootForNode(node);
		if (root) return getNodeSetForRoot(root);
		else return nullptr;
	}

	// register an empty node set for this node
	void addNode(SketchNode * contractedNode, SketchNode * originalNode);

	// Merges @contractedNodes into @root and updates the look-up tables
	void mergeInto(SketchNode * root, SketchNode * contractedNode);

	// remove this root
	void dropRoot(SketchNode * root);

	void dump(SketchGraph & contractedGraph, SketchGraph & srcGraph) const;
};

#endif /* SRC_ANALYSIS_SKETCHGRAPH_H_ */
