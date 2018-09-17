/*
 * CNSScoring.h
 *
 *  Created on: 29.04.2010
 */

#ifndef CNSSCORING_HPP_
#define CNSSCORING_HPP_

#include "BlockGraph.h"

namespace rv {
namespace cns {
typedef unsigned (*ScoringFunction)(BlockGraph::SubgraphMask &mask,
                                BlockGraph &graph, unsigned candidate);

/*
 * returns the amount of instructions in this basic block
 */
unsigned scoreNumInstructions(BlockGraph::SubgraphMask &mask, BlockGraph &graph,
                          unsigned candidate);

/*
 * returns the number of predecessors of this block (in the BlockGraph)
 */
unsigned scoreBranches(BlockGraph::SubgraphMask &mask, BlockGraph &graph,
                   unsigned candidate);

/*
 * takes a per-node based scoring function and returns the node that scores
 * lowest
 *
 * returns 0 if the graph does not contain any nodes
 */
unsigned getLowestScoringNode(BlockGraph::SubgraphMask &mask, BlockGraph &graph,
                          ScoringFunction heuristicFunc);
}
}

#endif /* CNSSCORING_HPP_ */
