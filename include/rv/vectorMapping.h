//===- vectorMapping.h -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//

#ifndef INCLUDE_RV_VECTORMAPPING_H_
#define INCLUDE_RV_VECTORMAPPING_H_

#include <rv/vectorShape.h>
#include <initializer_list>

namespace llvm {
	class Function;
	class raw_ostream;
}

namespace rv {

struct VectorMapping {
	llvm::Function * scalarFn;
	llvm::Function * vectorFn;
	unsigned vectorWidth; // == 0 implies that this mapping is sound for all vectorWidths (no varying shapes), otw this mapping is only applicable for the specified width
	int maskPos; // < 0 => no mask argument, otw position of mask argument
	VectorShapeVec argShapes;
	VectorShape resultShape;
	// If scalarFn == vectorFn the following vectorization rules apply
	// 1. scalarFn has side effects -> function call will be replicated for all lanes at every call site
	// 2. resultShape == VARYING -> replicated
	// 3. the mapping contains at least one VARYING argument -> replicated
	// 4. the function is called with a VARYING parameter -> replicated for this call
	// 5. the function is called with a parameter of vector shape T that was not registered with a VectorMapping -> replicated for this call

	// example : get_thread_id()
	// -> resultShape == Consecutive, no side effects

	VectorMapping(llvm::Function * _scalarFn, llvm::Function * _vectorFn, unsigned _vectorWidth, int _maskPos, VectorShape _resultShape, std::initializer_list<VectorShape> argShapeList)
	: scalarFn(_scalarFn)
	, vectorFn(_vectorFn)
	, vectorWidth(_vectorWidth)
	, maskPos(_maskPos)
	, argShapes(argShapeList)
	, resultShape(_resultShape)
	{}

	VectorMapping(llvm::Function * _scalarFn, llvm::Function * _vectorFn, unsigned _vectorWidth, int _maskPos, VectorShape _resultShape, const VectorShapeVec & _argShapeVec)
	: scalarFn(_scalarFn)
	, vectorFn(_vectorFn)
	, vectorWidth(_vectorWidth)
	, maskPos(_maskPos)
	, argShapes(_argShapeVec)
	, resultShape(_resultShape)
	{}

	VectorMapping(llvm::Function * _scalarFn, llvm::Function * _vectorFn, unsigned _vectorWidth)
	: scalarFn(_scalarFn)
	, vectorFn(_vectorFn)
	, vectorWidth(_vectorWidth)
	, maskPos(-1)
	, argShapes()
	, resultShape()
	{}

	void dump(llvm::raw_ostream & out) const;
};


}



#endif /* INCLUDE_RV_VECTORMAPPING_H_ */
