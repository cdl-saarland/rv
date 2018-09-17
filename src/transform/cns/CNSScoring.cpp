/*
 * CNSScoring.cpp
 *
 *  Created on: 29.04.2010
 */

#include "CNSScoring.h"

namespace rv {

namespace cns {

typedef unsigned (*ScoringFunction)(BlockGraph::SubgraphMask &mask,
                                BlockGraph &graph, unsigned candidate);

unsigned scoreNumInstructions(BlockGraph::SubgraphMask &mask, BlockGraph &graph,
                          unsigned candidate) {
  llvm::BasicBlock *block = graph.getBasicBlock(candidate);
  return block->getInstList().size();
}

unsigned scoreBranches(BlockGraph::SubgraphMask &mask, BlockGraph &graph,
                   unsigned candidate) {
  return graph.getNumSuccessors(mask, candidate);
}

unsigned getLowestScoringNode(BlockGraph::SubgraphMask &mask, BlockGraph &graph,
                          ScoringFunction heuristicFunc) {
  unsigned lowest = 0xFFFFFFFF;
  unsigned lowestNode = 0;

  for (unsigned i = 0; i < mask.size(); ++i) {
    if (mask[i]) {
      unsigned tmpScore = (*heuristicFunc)(mask, graph, i);
      if (tmpScore < lowest) {
        lowest = tmpScore;
        lowestNode = i;
      }
    }
  }

  return lowestNode;
}
}
}
