//
// Created by tkloessner on 08.05.17.
//

#include <llvm/IR/CFG.h>
#include "rv/analysis/DivPathDecider.h"
#include "../rvConfig.h"
#include <llvm/Support/raw_ostream.h>
#include <llvm/Analysis/LoopInfo.h>
#include <stack>
#include <unordered_map>
#include <numeric>

#define IF_DEBUG_DPD if (false)

using llvm::BasicBlock;
using llvm::Function;
using llvm::Loop;

struct DivPathDecider::Node {
  enum SplitType { IN = 0, OUT = 1} type;
  const llvm::BasicBlock* BB;

  Node(SplitType type, const llvm::BasicBlock* BB) : type(type), BB(BB) {}
};

DivPathDecider::~DivPathDecider() {
  for (auto& p : innodes) {
    delete p.second;
  }

  for (auto& p : outnodes) {
    delete p.second;
  }
}

const DivPathDecider::Node* DivPathDecider::findPath(const Node* source,
                                                     const NodeList& sinks,
                                                     const EdgeSet& flow,
                                                     PredecessorMap& parent,
                                                     const Loop* TheLoop)
{
  std::set<const Node*> visited;
  std::stack<const Node*> stack;
  stack.push(source);

  while (!stack.empty()) {
    const Node* node = stack.top();
    stack.pop();
    visited.insert(node);

    const BasicBlock* runner = node->BB;

    auto found = std::find(sinks.begin(), sinks.end(), node);
    if (found != sinks.end()) {
      return *found;
    }

    if (node->type == Node::OUT) {
      // Successors
      for (auto* succ : llvm::successors(runner)) {
        const Node* next = getInNode(succ);
        if (!TheLoop || TheLoop->contains(runner)) {
          if (visited.count(next) == 0 && !flow.count({node, next})) {
            stack.push(next);
            parent[next] = node;
          }
        }
      }

      // Backwards split edge
      const Node* splitIN = getInNode(runner);
      if (visited.count(splitIN) == 0 && flow.count({splitIN, node})) {
        stack.push(splitIN);
        parent[splitIN] = node;
      }
    } else {
      // Traverse split edge
      const Node* splitOUT = getOutNode(runner);
      if (visited.count(splitOUT) == 0 && !flow.count({node, splitOUT})) {
        stack.push(splitOUT);
        parent[splitOUT] = node;
      }

      // Predecessors
      for (auto* pred : llvm::predecessors(runner)) {
        const Node* next = getOutNode(pred);
        if (!TheLoop || TheLoop->contains(runner)) {
          if (visited.count(next) == 0 && flow.count({next, node})) {
            stack.push(next);
            parent[next] = node;
          }
        }
      }
    }
  }

  return nullptr;
}

bool DivPathDecider::inducesDivergentExit(const BasicBlock* From,
                                          const BasicBlock* Exit,
                                          const Loop* loop)
{
  if (From == loop->getLoopLatch()) {
    return Exit->getUniquePredecessor() == From;
  }

  const Node* source = getOutNode(From);
  NodeList sinks = { getOutNode(Exit), getOutNode(loop->getLoopLatch()) };
  return divergentPaths(source, sinks, 2, loop);
}

// Find n vertex-disjoint paths from A to B, this algorithm is a specialization of Ford-Fulkerson,
// that terminates after a flow of n is found. Running time is thus O(Edges) * n
bool DivPathDecider::divergentPaths(const BasicBlock* From, const BasicBlock* To, unsigned int n) {
  const Node* source = getOutNode(From);
  NodeList sinks = { getInNode(To) };
  return divergentPaths(source, sinks, n, nullptr);
}

bool DivPathDecider::divergentPaths(const Node* source,
                                    const NodeList& sinks,
                                    unsigned int n,
                                    const Loop* loop)
{
  EdgeSet flow;

  for (unsigned i = 0; i < n; ++i) {
    PredecessorMap parent;
    const Node* sink = findPath(source, sinks, flow, parent, loop);
    if (!sink) {
      // Could not find a path.
      return false;
    }

    injectFlow(source, sink, parent, flow);
  }

  IF_DEBUG_DPD {
    // Extract the paths from the flow
    for (unsigned i = 0; i < n; ++i) {
      std::vector<const BasicBlock*> forwardpath;
      extractPath(source, sinks, flow, forwardpath);

      // Dump.
      llvm::errs() << "Path " << i << " is:\n";
      for (const BasicBlock* BB : forwardpath) {
        llvm::errs() << " -> ";
        BB->printAsOperand(llvm::errs(), false);
      }
      llvm::errs() << "\n";
    }
  }

  return true;
}

void DivPathDecider::injectFlow(const Node* start,
                                const Node* end,
                                const PredecessorMap& parent,
                                EdgeSet& flow)
{
  // Adjust flow for the next iteration
  while (end && end != start) {
    const Node* prev = parent.find(end)->second;
    if (flow.count({end, prev})){
      // Backwards edge reset
      flow.erase({end, prev});
    }
    else {
      // Ordinary edge insert
      flow.insert({prev, end});
    }

    end = prev;
  }
}

void DivPathDecider::extractPath(const Node* source,
                                 const NodeList& sinks,
                                 EdgeSet& flow,
                                 Path& path)
{
  const BasicBlock* pathblock = source->BB;
  path.push_back(source->BB);

  while (llvm::all_of(sinks, [=](const Node* sink) { return sink->BB != pathblock; })) {
    const Node* out = getOutNode(pathblock);
    for (const BasicBlock* succ : successors(pathblock)) {
      // Find a successor to which a flow of 1 is present.
      const Node* in = getInNode(succ);
      if (flow.count({out, in}) > 0) {
        flow.erase({out, in});
        path.push_back(succ);
        pathblock = succ;
        break;
      }
    }
  }
}

const DivPathDecider::Node* DivPathDecider::getInNode(const BasicBlock* BB) {
  auto found = innodes.find(BB);
  if (found != innodes.end()) {
    return found->second;
  }

  return innodes[BB] = new Node(Node::IN, BB);
}

const DivPathDecider::Node* DivPathDecider::getOutNode(const BasicBlock* BB) {
  auto found = outnodes.find(BB);
  if (found != outnodes.end()) {
    return found->second;
  }

  return outnodes[BB] = new Node(Node::OUT, BB);
}