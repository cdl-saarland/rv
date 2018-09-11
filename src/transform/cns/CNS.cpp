/*
 * CNSOperations.cpp
 *
 *  Created on: 29.04.2010
 */

#include "CNS.h"
#include "CommonTypes.h"

namespace rv {

namespace cns {

BlockGraph::SubgraphMask
detectCandidateNodes(const BlockGraph::SubgraphMask &mask,
                     const BlockGraph &graph) {
  BlockGraph::SubgraphMask candidates(mask.size(), false);

  MaskVector SCCs = cns::computeSCCs(mask, graph);

  for (cns::MaskVector::iterator itMask = SCCs.begin(); itMask != SCCs.end();
       ++itMask) {
    BlockGraph::SubgraphMask &sccMask = *itMask;

    BlockGraph::SubgraphMask headers = graph.getEntryNodes(mask, sccMask);

    BlockGraph::SubgraphMask sccRest = sccMask;

    for (unsigned h = 0; h < headers.size(); ++h) {
      if (headers[h]) {
        BlockGraph::SubgraphMask domRegion =
            cns::computeDominanceRegion(mask, graph, h);
        BlockGraph::SubgraphMask sccDomRegion = AND(sccMask, domRegion);
        sccRest = AND(sccRest, NOT(sccDomRegion));

        sccDomRegion[h] = false; // disable the header itself

        if (EXISTS(sccDomRegion)) {

          // recurse
          BlockGraph::SubgraphMask innerCandidates =
              detectCandidateNodes(sccDomRegion, graph);
          candidates = OR(candidates, innerCandidates);
        } else {
          candidates[h] = true;
        }
      }
    }

    if (EXISTS(sccRest)) {
      // non-dominated graph region
      BlockGraph::SubgraphMask innerCand = detectCandidateNodes(sccRest, graph);
      candidates = OR(candidates, innerCand);
    }
  }

  return candidates;
}

void minimizeGraph(BlockGraph::SubgraphMask &mask, BlockGraph &graph) {
  unsigned size = graph.getSize();

  // T2 - Transform (ignore self-loops)
  bool contracted;
  do {
    contracted = false;
    for (unsigned index = 0; index < size; ++index) {
      if (mask[index]) {
        int pred = graph.getSinglePredecessorNonReflexive(mask, index);
        if (pred > -1) {
          IF_DEBUG_CNS std::cerr << "contracting " << pred << " to " << index << "\n";
          graph.contractNode(mask, pred, index);
          IF_DEBUG_CNS graph.dumpGraphviz(mask, std::cerr);
          contracted = true;
        }
      }
    }
  } while (contracted);

  // T1 - Transform
  for (unsigned index = 0; index < size; ++index) {
    if (graph.getEdge(mask, index, index)) {
      graph.setEdge(index, index, false);
    }
  }
}

/*
 * computes the SCCs of the graph (Strongly Connected Component)
 */

struct SCCFrame {
  unsigned node;
  int streakColor;

  SCCFrame(unsigned _node, int _streakColor)
      : node(_node), streakColor(_streakColor) {}
};

/*
 * computes all SCCs in the sub graph
 *
 * Implementation detail: implicitely operates on the inverted graph
 */
std::vector<BlockGraph::SubgraphMask>
computeSCCs(const BlockGraph::SubgraphMask &mask, const BlockGraph &graph) {
  std::vector<BlockGraph::SubgraphMask> result;

  BlockGraph::SubgraphMask visited(mask.size(), false);

  // additional graph labels
  typedef std::vector<BlockGraph::SubgraphMask> SubgraphMaskVector;
  typedef std::vector<int> IntVector;

  SubgraphMaskVector prefix(graph.getSize()); // used for writing down the trace
                                              // that encountered this node
                                              // first
  IntVector color(graph.getSize(), -1);

  std::stack<SCCFrame> stack;

  // exploration status
  BlockGraph::SubgraphMask trace(graph.getSize(), false);

  int nextFreeColor = 0;

  unsigned firstNode = 0;
  for (; firstNode < graph.getSize() && !mask[firstNode];)
    continue;
  if (firstNode >= graph.getSize())
    return result;

  stack.push(SCCFrame(firstNode, -1));

  while (true) // iterate over all valid start points
  {
    IF_DEBUG_CNS std::cerr << "# starting at node " << firstNode << "\n";

    while (!stack.empty()) {
      SCCFrame frame = stack.top();
      stack.pop();

      unsigned node = frame.node;
      int streakColor = frame.streakColor;
      IF_DEBUG_CNS {
        std::cerr << "stack top " << node << " with color " << streakColor
                << "\n";
      }

      {
        /*
         * process the current node
         */
        // found a loop with the trace
        if (trace[node]) {
          IF_DEBUG_CNS std::cerr << "trace loop at " << node << "\n";
          BlockGraph::SubgraphMask loopMask = AND(trace, NOT(prefix[node]));
          loopMask[node] = true;

          bool introducesColor = (streakColor == -1);

          if (introducesColor) {
            streakColor = nextFreeColor++;
          }

          for (unsigned i = 0; i < loopMask.size(); ++i)
            if (loopMask[i])
              color[i] = streakColor;

          if (introducesColor) {
            result.push_back(loopMask);
          } else {
            BlockGraph::SubgraphMask oldMask = result[streakColor];
            result[streakColor] = OR(oldMask, loopMask);
          }

          // found an additional path at a loop
        } else if ((streakColor > -1) && visited[node] &&
                   (color[node] == streakColor)) {
          IF_DEBUG_CNS std::cerr << "side streak at " << node << " with color "
                    << streakColor << "\n";
          assert(streakColor != -1 && "somewhere something strange happened");

          BlockGraph::SubgraphMask loopMask = AND(trace, NOT(prefix[node]));
          loopMask[node] = true;
          BlockGraph::SubgraphMask oldMask = result[streakColor];
          result[streakColor] = OR(oldMask, loopMask);

          for (unsigned i = 0; i < loopMask.size(); ++i)
            if (loopMask[i])
              color[i] = streakColor;

          // push first child
        } else if (!visited[node]) {
          trace[node] = true;
          prefix[node] = trace;

          unsigned firstSucc = 0;
          for (; (firstSucc < graph.getSize()) &&
                 !graph.getEdge(mask, firstSucc, node);
               ++firstSucc)
            continue;
          if (firstSucc < graph.getSize()) {
            stack.push(frame);
            stack.push(SCCFrame(firstSucc, streakColor));
            IF_DEBUG_CNS std::cerr << "extending trace from " << node << " to " << firstSucc
                      << "\n";
            visited[node] = true;
            continue;
          } else {
            IF_DEBUG_CNS std::cerr << "collapsing leaf " << node << "\n";
            trace[node] = false;
          }
        }

        visited[node] = true;

      /*
       * modify the trace to point to the next node according to DFS
       */
      NextEdge:
        trace[frame.node] = false;

        // edge segment (jump to next successor)
        if (!stack.empty()) {

          SCCFrame parentFrame = stack.top();
          unsigned base = parentFrame.node;
          unsigned nextNode = frame.node + 1;

          for (; nextNode < graph.getSize() &&
                 !graph.getEdge(mask, nextNode, base);
               ++nextNode)
            continue;

          if (nextNode >= graph.getSize()) { // collapse this edge
            IF_DEBUG_CNS std::cerr << "collapsing node " << frame.node << "\n";
            frame = stack.top();
            stack.pop();
            goto NextEdge;

          } else { // reinsert the frame
            IF_DEBUG_CNS std::cerr << "flipping edge from " << node << " to " << nextNode
                      << "\n";
            frame.streakColor = parentFrame.streakColor;
            frame.node = nextNode;
          }
          stack.push(frame);
        }
      }
    }

    for (firstNode++;
         firstNode < graph.getSize() && !mask[firstNode] && visited[firstNode];
         ++firstNode)
      continue;
    if (firstNode >= graph.getSize()) {
      return result;
    } else {
      stack.push(SCCFrame(firstNode, -1));
    }
    trace = BlockGraph::SubgraphMask(graph.getSize(), false);
  }
}

/*
 * computes the dominance frontier
 */
BlockGraph::SubgraphMask
computeDominanceRegion(const BlockGraph::SubgraphMask &mask,
                       const BlockGraph &graph, unsigned node) {
  BlockGraph invGraph = graph.transposed();

  BlockGraph::SubgraphMask dom(graph.getSize(), false);
  BlockGraph::SubgraphMask nextDom = dom;
  nextDom[node] = true;

  unsigned it = 0;
  do {
    dom = nextDom;
    nextDom = AND(mask, OR(dom, invGraph.mergedPredecessors(dom))); // successors

    IF_DEBUG_CNS {
      std::cerr << "# iteration " << it << "\n";
      std::cerr << "dom     : ";
      dumpVector(dom);
      std::cerr << "nextDom : ";
      dumpVector(nextDom);
    }

    for (unsigned i = 0; i < graph.getSize(); ++i) {
      if (i == node) {
        nextDom[i] = true;
      } else if (nextDom[i]) {
        BlockGraph::SubgraphMask incoming =
            graph.getPredecessors(mask, i); // predecessors
        BlockGraph::SubgraphMask nonDomPred = AND(incoming, NOT(dom));
        nonDomPred[i] = false; // allow self-loops

        IF_DEBUG_CNS {
          std::cerr << "- nonDomPred for " << i << "\n";
          std::cerr << ": ";
          dumpVector(nonDomPred);
        }

        nextDom[i] = NONE(nonDomPred);
      }
    }

    ++it;
  } while (nextDom != dom);

  return nextDom;
}
}
}
