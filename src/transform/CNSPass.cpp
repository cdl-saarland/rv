/*
 * Regularizer.cpp
 *
 *  Created on: 29.04.2010
 *      Author: gnarf
 */
#include <fstream>

#include <llvm/Pass.h>

#include "cns/BlockGraph.h"
#include "cns/CNS.h"
#include "cns/CNSScoring.h"
#include "cns/SplitTree.h"
#include "cns/llvmDuplication.h"
#include "cns/CommonTypes.h"

#include "rv/LinkAllPasses.h"
#include "rv/passes.h"

using namespace rv;
using namespace llvm;

/*
 * The Regularizer makes irreducible control flow reducible by applying
 * controlled node splitting
 */
class CNS : public llvm::FunctionPass {
  cns::SplitTree *generateSplitSequence(cns::SplitTree *root,
                                        BlockGraph::SubgraphMask &mask,
                                        BlockGraph &graph);

  void applySplitSequence(BlockGraph &graph, std::vector<uint> nodes) const;

public:
  static char ID;

  CNS() : llvm::FunctionPass(ID) {}

  void getAnalysisUsage(llvm::AnalysisUsage &usage) const {}

  bool runOnFunction(llvm::Function &F);

  virtual llvm::StringRef getPassName() const;
};




/*
 * Controlled Node Splitting - main method
 */
cns::SplitTree *CNS::generateSplitSequence(cns::SplitTree *root,
                                               BlockGraph::SubgraphMask &mask,
                                               BlockGraph &graph) {
#ifdef DEBUG
  llvm::errs() << "### regularize : ";
  dumpVector(mask);
#endif

  /*
   * initial transformation (T1/T2)
   */
  cns::minimizeGraph(mask, graph);

  cns::SplitTree *tree = root;

#ifdef DEBUG
  llvm::errs() << "mask after contraction = ";
  dumpVector(mask);
  llvm::errs() << "graph Size after initial contraction=" << graph.getSize(mask)
               << "\n";
#endif

  while (graph.getSize(mask) > 1) {

/*
 * identify RC-nodes
 */
#ifdef DEBUG
    llvm::errs() << "identifying candidate nodes (SED, non-RC). . \n";
#endif
    BlockGraph::SubgraphMask candidates =
        cns::detectCandidateNodes(mask, graph);

#ifdef DEBUG
    llvm::errs() << "candidate nodes: ";
    dumpVector(candidates);
#endif

    /*
     * select splitting node (from the headers of the SCC)
     */
    uint splitNode =
        cns::getLowestScoringNode(candidates, graph, &cns::scoreBranches);

#ifdef DEBUG
    llvm::errs() << "heuristic picked node: " << splitNode << "\n";
#endif
    /*
     * split (complete graph mask gets modified to match)
     */
    BlockGraph splitGraph = graph.createSplitGraph(mask, splitNode);

    tree = tree->pushSplit(mask, splitGraph, splitNode);

#ifdef DEBUG
    llvm::errs() << "graph after split";
    splitGraph.dump(mask);
    llvm::errs() << "tree:\n";
    tree->dump();
#endif

    // for now just iteratively operate on a single graph
    graph = splitGraph;

    /*
     * compute limit graph
     */
    cns::minimizeGraph(mask, graph);
  }

  return tree;
}

bool CNS::runOnFunction(llvm::Function &func) {
  BlockGraph::SubgraphMask mask;
  BlockGraph graph = BlockGraph::CreateFromFunction(func, mask);
  /*
          {
                  std::ofstream of( (func.getNameStr() + "_graph.gv").c_str(),
     std::ios::out);
                  graph.dumpGraphviz(mask, of);
          }
  */
  cns::SplitTree *root = new cns::SplitTree(mask, graph);
  cns::SplitTree *tree = generateSplitSequence(root, mask, graph);

#ifdef DEBUG
  tree->dump();
#endif

  uint length = tree->getDepth();
  std::vector<uint> nodes(length);

  // recover split sequence
  for (uint i = length; i > 0; --i) {
    nodes[i - 1] = tree->getSplitNode();
    tree = tree->getParent();
  }

  delete root;

  applySplitSequence(graph, nodes);

#ifdef DEBUG
  llvm::errs() << "regularized function : \n";
  func.dump();
#endif

  return true;
}

llvm::StringRef CNS::getPassName() const {
  return "Controlled Node Splitting pass";
}

void CNS::applySplitSequence(BlockGraph &graph,
                                 std::vector<uint> nodes) const {
  for (uint i = 0; i < nodes.size(); ++i) {
    uint node = nodes[i];
    llvm::BasicBlock *splitBlock = graph.getLabel(node);

    splitNode(splitBlock);
  }
}




char CNS::ID = 0;

FunctionPass *rv::createCNSPass() { return new CNS(); }

INITIALIZE_PASS_BEGIN(CNS, "rv-cns",
                      "RV - Irreducible Loop Normalization (CNS)", false, false)
INITIALIZE_PASS_END(CNS, "rv-cns", "RV - Irreducible Loop Normalization (CNS)",
                    false, false)

