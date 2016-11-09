//===- PathFinder.h -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//

#ifndef SRC_ANALYSIS_PATHFINDER_H_
#define SRC_ANALYSIS_PATHFINDER_H_


#include "SketchGraph.h"
#include <map>
#include "utils/rvTools.h"
#include "analysisCfg.h"

#ifdef VA_VERIFY
// #define PF_VERBOSE
#else
// #define PF_VERBOSE
#endif


namespace rv {

struct State : public std::pair<SketchNode*,SketchNode*> {
	NodeSet passedSet;

#ifdef PF_VERBOSE
	RV::PathType aPath;
	RV::PathType bPath;
#endif
	State(SketchGraph & graph, State * parent, SketchNode * a, SketchNode * b)
	: std::pair<SketchNode*,SketchNode*>()
#ifdef PF_VERBOSE
	, aPath(), bPath()
#endif
	{
		first = a;
		second = b;
		if (parent) {
			passedSet = parent->passedSet;
		}
		passedSet.insert(a);
		passedSet.insert(b);
#ifdef PF_VERBOSE
		if (parent) {
			aPath = parent->aPath;
			bPath = parent->bPath;
		}
		const BasicBlock * aBlock = graph.getBlock(*a);
		const BasicBlock * bBlock = graph.getBlock(*b);
		if (aPath.empty() || aPath.back() != aBlock) aPath.push_back(aBlock);
		if (bPath.empty() || bPath.back() != bBlock) bPath.push_back(bBlock);
#endif
	}
};



class PathFinder {
	SketchGraph & graph;
	NodeSet accept;

	inline bool validState(State s) const { return s.first != s.second; }
	inline bool acceptingState(State s) const {
		return validState(s) && accept.count(s.first) && accept.count(s.second);
	}

public:
	inline State make_state(State * parent, SketchNode * a, SketchNode * b) {
		return State(graph, parent, a, b);
	}
	// find paths from xStart to xAccept without passing a not @validState
	// this guarentees that at least one path uses an edge
	bool findPath(SketchNode * start);

	PathFinder(SketchGraph & _graph, NodeSet & _accept)
	: graph(_graph)
	, accept(_accept)
	{}
};

}

#endif /* SRC_ANALYSIS_PATHFINDER_H_ */

