//===- PathFinder.cpp -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @authors simon
//


#include "PathFinder.h"
#include <llvm/Support/raw_ostream.h>

namespace rv {

bool
PathFinder::findPath(SketchNode * start) {

	// invariant: only keep valid states on the stack
	std::vector<State> stack;
	std::set<State> visited;

#ifdef PF_VERBOSE
	errs() << "findPath(" << graph.getBlock(*start)->getName() << ")\n";
	errs() << "accepting states\n\t";
	graph.dumpSet(accept);
	errs() << "Analysis "; graph.dump();
#endif /* PF_VERBOSE */

// force a move in at least one state
	if (start->empty()) {
		 return false;
	}

	for (auto startSucc : *start) { // FIXME what a about self loops(?)
		State aSingleStepState = make_state(nullptr, start, startSucc);
		if (validState(aSingleStepState)) stack.push_back(aSingleStepState);
	}

// Path exploration loop
	while (! stack.empty()) {
		State state = stack.back();
		stack.pop_back();

		if (! visited.insert(state).second) {
			continue;
		}

		SketchNode * currA = state.first;
		SketchNode * currB = state.second;

		bool aFinished = accept.count(currA);
		bool bFinished = accept.count(currB);

		if (aFinished && bFinished) {
#ifdef PF_VERBOSE
			llvm::errs() << "Disjoint path according to new criterion\n";
			WFV::DumpPath(state.aPath);
			WFV::DumpPath(state.bPath);
#endif /* PF_VERBOSE */
			return true;
		}
		if (! bFinished) {
			for (auto bSucc : *currB) {
				if (! state.passedSet.count(bSucc)) {
					State succ = make_state(&state, currA, bSucc);
					if (validState(succ)) {
						stack.push_back(succ);
					}
				}
			}
		}
		if (! aFinished) {
			for (auto aSucc : *currA) {
				if (! state.passedSet.count(aSucc)) {
					State succ = make_state(&state, aSucc, currB);
					if (validState(succ)) {
						stack.push_back(succ);
					}
				}
			}
		}
		if (!aFinished && !bFinished) {
			for (auto aSucc : *currA) {
				for (auto bSucc : *currB) {
					if (! state.passedSet.count(aSucc) &&
						! state.passedSet.count(bSucc))
					{
						State succ = make_state(&state, aSucc, bSucc);
						if (validState(succ)) {
							stack.push_back(succ);
						}
					}
				}
			}
		}
	}

	return false;
}

}
