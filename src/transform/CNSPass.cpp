/*
 * Regularizer.cpp
 *
 *  Created on: 29.04.2010
 *      Author: gnarf
 */
#include <fstream>

#include <llvm/Pass.h>
#include <llvm/IR/Verifier.h>

#include "cns/BlockGraph.h"
#include "cns/CNS.h"
#include "cns/CNSScoring.h"
#include "cns/SplitTree.h"
#include "cns/llvmDuplication.h"
#include "cns/CommonTypes.h"

#include "rv/LinkAllPasses.h"
#include "rv/passes.h"

#include "report.h"

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
  IF_DEBUG_CNS {
    llvm::errs() << "### regularize : ";
    dumpVector(mask);
  }

  /*
   * initial transformation (T1/T2)
   */
  cns::minimizeGraph(mask, graph);

  cns::SplitTree *tree = root;

  IF_DEBUG_CNS {
    llvm::errs() << "mask after contraction = ";
    dumpVector(mask);
    llvm::errs() << "graph Size after initial contraction=" << graph.getSize(mask)
                 << "\n";
  }

  while (graph.getSize(mask) > 1) {

/*
 * identify RC-nodes
 */
    IF_DEBUG_CNS llvm::errs() << "identifying candidate nodes (SED, non-RC). . \n";
    BlockGraph::SubgraphMask candidates =
        cns::detectCandidateNodes(mask, graph);

    IF_DEBUG_CNS {
      llvm::errs() << "candidate nodes: ";
      dumpVector(candidates);
    }

    /*
     * select splitting node (from the headers of the SCC)
     */
    uint splitNode =
        cns::getLowestScoringNode(candidates, graph, &cns::scoreBranches);

    IF_DEBUG_CNS llvm::errs() << "heuristic picked node: " << splitNode << "\n";
    /*
     * split (complete graph mask gets modified to match)
     */
    BlockGraph splitGraph = graph.createSplitGraph(mask, splitNode);

    tree = tree->pushSplit(mask, splitGraph, splitNode);

    IF_DEBUG_CNS {
      llvm::errs() << "graph after split";
      splitGraph.dump(mask);
      llvm::errs() << "tree:\n";
      tree->dump();
    }

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
  size_t totalNumSplits = 0, numSplits;
  do {
    numSplits = 0;

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

    // FIXME split sequence implementation is broken..
    cns::SplitTree *tree = generateSplitSequence(root, mask, graph);

    IF_DEBUG_CNS tree->dump();

    numSplits = tree->getDepth();
    totalNumSplits += numSplits;
    std::vector<uint> nodes(numSplits);


    // recover split sequence
    for (size_t i = numSplits; i > 0; --i) {
      nodes[i - 1] = tree->getSplitNode();
      tree = tree->getParent();
    }

    delete root;

    applySplitSequence(graph, nodes);

    IF_DEBUG_CNS {
      llvm::errs() << "regularized function : \n";
      func.dump();
    }

    bool broken = verifyFunction(func, &errs());
    if (broken) fail("CNS broke the module");
  } while (numSplits > 0);

  if (totalNumSplits > 0) Report() << "cns: splitted " << totalNumSplits << " nodes.\n";

  return totalNumSplits > 0;
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

