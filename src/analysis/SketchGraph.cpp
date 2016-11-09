//===- SketchGraph.cpp -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @authors simon
//


#include "SketchGraph.h"
#include <llvm/IR/Dominators.h>
#include <llvm/IR/CFG.h> // GraphTraits
#include "rvConfig.h"

#include "utils/rvTools.h"

typedef llvm::GraphTraits<const llvm::BasicBlock*> ConstCFG;

// ContractionMap
ContractionMap::ContractionMap() {}

ContractionMap::~ContractionMap() {
	for (auto it : confluenceMap) {
		delete it.second;
	}
}

SketchNode *
ContractionMap::getRootForNode(SketchNode * node) const {
	auto it = rootMap.find(node);
	if (it == rootMap.end()) return nullptr;
	else return it->second;
}

NodeSet *
ContractionMap::getNodeSetForRoot(SketchNode * root) const {
	auto it = confluenceMap.find(root);
	if (it == confluenceMap.end()) return nullptr;
	else return it->second;
}


void
ContractionMap::addNode(SketchNode * contractedNode, SketchNode * originalNode) {
	NodeSet * initSet = new NodeSet;
	initSet->insert(originalNode);
	confluenceMap[contractedNode] = initSet;
	rootMap[originalNode] = contractedNode;
}



void
ContractionMap::mergeInto(SketchNode * root, SketchNode * contractedNode) {
	NodeSet * additionalConfluentNodes = getNodeSetForRoot(contractedNode);
	NodeSet * confluentNodes = getNodeSetForRoot(root);

// merge in confluence set (contractedNode -> root)
	setcpy(*confluentNodes, *additionalConfluentNodes);

// update inverse look-up table
	for (auto it : *additionalConfluentNodes) {
		rootMap[it] = root;
	}

	confluenceMap.erase(contractedNode);
}



void
ContractionMap::dump(SketchGraph & contractedGraph, SketchGraph & srcGraph) const {
	errs() << "ContractionMap {\n";
	for (auto it : confluenceMap) {
		SketchNode * contractedNode = it.first;
		errs() << contractedGraph.getBlock(*contractedNode)->getName() << " -> "; srcGraph.dumpSet(*it.second);
	}
	errs() << "}\n";
}


// SketchGraph
SketchGraph::SketchGraph() {}

SketchGraph::~SketchGraph() {
	for (auto pair : nodeToBlockMap)
		delete pair.first;
}

SketchNode *
SketchGraph::push(const llvm::BasicBlock * block) {
	SketchNode * node = new SketchNode();
	blockToNodeMap[block] = node;
	nodeToBlockMap[node] = block;
	return node;
}


typedef llvm::DomTreeNodeBase<llvm::BasicBlock*> DomNode;

static
void
rec_addNodes_idom(SketchGraph & graph, const llvm::DominatorTree & domTree, const llvm::BasicBlock & root, const llvm::BasicBlock & block, NodeSet & oExiting) {
	ConstCFG::ChildIteratorType itBegin = ConstCFG::child_begin(&block);
	ConstCFG::ChildIteratorType itEnd = ConstCFG::child_end(&block);

	auto node = graph.push(&block);
	IF_DEBUG llvm::errs() << " visiting " << block.getName() << "\n";

	bool hasExitNodes = false;
	for (auto it = itBegin; it != itEnd; ++it) {
		const llvm::BasicBlock & succ = **it;
		if (! graph.getNode(succ)) {
			if (domTree.dominates(&root, &succ)) {
				rec_addNodes_idom(graph, domTree, root, succ, oExiting);
			} else {
				hasExitNodes = true;
			}
		}
	}

	if (hasExitNodes) {
		oExiting.insert(node);
	}
}

void
SketchGraph::initializeWithCoveredEdges() {
	for (auto it : *this) {
		SketchNode * node = it.first;
		const llvm::BasicBlock * block = it.second;

		auto itBegin = ConstCFG::child_begin(block);
		auto itEnd = ConstCFG::child_end(block);

		for (auto itSucc = itBegin; itSucc != itEnd; ++itSucc) {
			const llvm::BasicBlock * childBlock = *itSucc;
			SketchNode * childNode = getNode(*childBlock);
			if (childNode) node->insert(childNode);
		}
	}
}

SketchGraph *
SketchGraph::createFromDominatorNode(const llvm::DominatorTree & domTree, const llvm::BasicBlock & domRoot, NodeSet & oExiting) {
	SketchGraph * graph = new SketchGraph();

// Transfer all nodes below domRoot
	rec_addNodes_idom(*graph, domTree, domRoot, domRoot, oExiting);

// Transfer all edges with source and dest in the region
	graph->initializeWithCoveredEdges();

	return graph;
}

// FIXME
void
SketchGraph::dump() const {
	llvm::errs() << "SketchGraph { \n";
	for (auto it : * this) {
		const llvm::BasicBlock & block = *it.second;
		std::string typeStr = /* RV::HasVaryingBranch(block, mv) ? " [V] " : */ " ";
		llvm::errs() << block.getName() << typeStr;
		dumpSet(*it.first);
	}
	llvm::errs() << "}\n";
}

void
SketchGraph::dumpSet(const NodeSet & nodeSet) const {
	llvm::errs() << "{";
	bool later = false;
	for (auto itNext : nodeSet) {
		if (later) { llvm::errs() << ','; }
		llvm::errs() << getBlock(*itNext)->getName();
		later = true;
	}
	llvm::errs() << "}\n";
}

void
SketchGraph::unlinkNode(SketchNode * node) {
	for (auto itOtherNode : *this) {
		SketchNode * otherNode = itOtherNode.first;
		otherNode->erase(node);
	}

}

void
SketchGraph::eraseNode(SketchNode * node) {
	unlinkNode(node);
	const llvm::BasicBlock * block = getBlock(*node);
	blockToNodeMap.erase(block);
	nodeToBlockMap.erase(node);
}

void
SketchGraph::eraseNodesExt(NodeSet & nodeSet) {
	for (auto node : nodeSet) {
		unlinkNode(node);
	}

	for (auto node : nodeSet) {
		const llvm::BasicBlock * block = nodeToBlockMap[node];
		blockToNodeMap.erase(block);
		nodeToBlockMap.erase(node);
	}
}


NodeSet
SketchGraph::getPredecessors(SketchNode * node) {
	NodeSet predSet;
	for (auto it : *this) {
		SketchNode * someNode = it.first;
		if (someNode->count(node)) {
			predSet.insert(someNode);
		}
	}
	return predSet;
}

// FIXME inefficient
void
SketchGraph::eliminateDeadEnds(NodeSet & keepSet) {
	bool changed = true;
	while (changed) {
		changed = false;
		for (auto itNode : *this) {
			SketchNode * node = itNode.first;
			if (node->empty() && ! keepSet.count(node)) {
				eraseNode(node);
                                delete node;
				changed = true;
				break; // FIXME necessary(?)
			}
		}
	}
}

void
SketchGraph::eliminateUnreachable(SketchNode * sourceNode) {
	NodeSet visited;
	std::vector<SketchNode*> stack;
	stack.push_back(sourceNode);

	while (! stack.empty()) {
		SketchNode * currNode = stack.back();
		stack.pop_back();

		if (visited.insert(currNode).second) {
			for (auto next : *currNode) {
				stack.push_back(next);
			}
		}
	}

	for (auto it : *this) {
		if (! visited.count(it.first)) {
			eraseNode(it.first);
		}
	}
}

SketchGraph *
SketchGraph::generateConfluenceGraph(const NodeSet & coreNodes, ContractionMap * oConfluentNodes) {
// This operates in two linked worlds: the original graph and the contracted graph, the ContractionMap establishes the mapping
	SketchGraph * contractedGraph = new SketchGraph();
	NodeSet contractedCoreNodes;

// Initialize confluenceMap
	for (auto it : *this) {
		SketchNode * node = it.first;
		const llvm::BasicBlock * block = getBlock(*node);
		SketchNode * contractedNode = contractedGraph->push(block);
		if (oConfluentNodes) oConfluentNodes->addNode(contractedNode, node); // we may only copy the original graph
		if (coreNodes.count(node)) {
			contractedCoreNodes.insert(contractedNode);
		}
	}

// The initial confluenceGraph is uncontracted. Just copy over all CFG edges
	contractedGraph->initializeWithCoveredEdges();

// contract all edges
	contractedGraph->contractLocalConfluence(contractedCoreNodes, oConfluentNodes);

	return contractedGraph;
}

void
SketchGraph::contractLocalConfluence(const NodeSet & coreNodes, ContractionMap * oConfluentNodes) {
// Repeatedly contract single successor nodes
// Each node in the contractedGraph, represents a set of nodes in the original graph that were contracted into it
// Predecessors edges are updated accordingly
	bool changed = true;
	while (changed) {
		changed = false;
		for (auto it : *this) {
			SketchNode * node = it.first;

		// Eliminate self-loops
			node->erase(node);

		// single successor node that is not a core node
			if (!coreNodes.count(node) && node->size() == 1) {
				SketchNode * uniqueSucc = *node->begin();
				NodeSet predSet = getPredecessors(node);


			// Link all predecessors to the unique successor
				for (SketchNode * pred : predSet) {
					pred->erase(node);
					pred->insert(uniqueSucc);
				}

			// Merge in the contracted node with the target confluent node
				if (oConfluentNodes) oConfluentNodes->mergeInto(uniqueSucc, node);

			// Drop the contractedNode
				eraseNode(node);
				delete node;

				changed = true;
				break;
			}
		}

	}
}

void
SketchGraph::collectReachingNodes(SketchNode & sinkNode, NodeSet & oReaching) {
	NodeSet singleSet;
	singleSet.insert(&sinkNode);
	collectReachingNodesExt(singleSet, oReaching);
}

void
SketchGraph::collectReachingNodesExt(const NodeSet & sinkNodeSet, NodeSet & oReaching) {
	std::vector<SketchNode* > stack;
	for (SketchNode * sinkNode : sinkNodeSet) {
		stack.push_back(sinkNode);
	}

	while (!stack.empty()) {
		SketchNode * node = stack.back();
		stack.pop_back();

		// visiting for the first time
		if (oReaching.insert(node).second) {
			NodeSet predSet = getPredecessors(node);

			for (SketchNode * pred : predSet) {
				stack.push_back(pred);
			}
		}
	}
}
