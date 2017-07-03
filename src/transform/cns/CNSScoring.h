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
typedef uint (*ScoringFunction)(BlockGraph::SubgraphMask &mask,
                                BlockGraph &graph, uint candidate);

/*
 * returns the amount of instructions in this basic block
 */
uint scoreNumInstructions(BlockGraph::SubgraphMask &mask, BlockGraph &graph,
                          uint candidate);

/*
 * returns the number of predecessors of this block (in the BlockGraph)
 */
uint scoreBranches(BlockGraph::SubgraphMask &mask, BlockGraph &graph,
                   uint candidate);

/*
 * takes a per-node based scoring function and returns the node that scores
 * lowest
 *
 * returns 0 if the graph does not contain any nodes
 */
uint getLowestScoringNode(BlockGraph::SubgraphMask &mask, BlockGraph &graph,
                          ScoringFunction heuristicFunc);
}
}

#endif /* CNSSCORING_HPP_ */
