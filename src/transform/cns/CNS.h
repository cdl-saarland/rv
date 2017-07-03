/*
 * CNSOperations.h
 *
 *  Created on: 29.04.2010
 */

#ifndef CNSOPERATIONS_HPP_
#define CNSOPERATIONS_HPP_

#include <stack>

#include "Bitmask.h"
#include "BlockGraph.h"

namespace rv {

namespace cns {
struct SED {
  uint iDom;
  BlockGraph::SubgraphMask mask;

  SED(uint _iDom, BlockGraph::SubgraphMask _mask) : iDom(_iDom), mask(_mask) {}
};

typedef std::vector<BlockGraph::SubgraphMask> MaskVector;
typedef std::vector<SED> SEDVector;

/*
 * compute RC-nodes (this will destroy some edges in the graph)
 */
BlockGraph::SubgraphMask
detectCandidateNodes(const BlockGraph::SubgraphMask &mask,
                     const BlockGraph &graph);

/*
 * computes the SCCs of the graph (Strongly Connected Component)
 */
MaskVector computeSCCs(const BlockGraph::SubgraphMask &mask,
                       const BlockGraph &graph);

/*
 * applies T1 and T2 to the maximum extend (CNS p. 47)
 */
void minimizeGraph(BlockGraph::SubgraphMask &mask, BlockGraph &graph);

/*
 * computes the dominance frontier
 */
BlockGraph::SubgraphMask
computeDominanceRegion(const BlockGraph::SubgraphMask &mask,
                       const BlockGraph &graph, uint node);
}
}

#endif /* CNSOPERATIONS_HPP_ */
