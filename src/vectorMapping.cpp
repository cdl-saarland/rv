//===- vectorMapping.cpp -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @authors simon

#include "rv/vectorMapping.h"

#include <llvm/Support/raw_ostream.h>
#include <llvm/IR/Function.h>

using namespace llvm;

namespace rv {


void
VectorMapping::dump(llvm::raw_ostream & out) const {
	out
		<< "VectorMapping {\n"
		<< "\tscalarFn = " << (scalarFn ? scalarFn->getName() : "null") << "\n"
		<< "\tvectorFn = " << (vectorFn ? vectorFn->getName() : "null") << "\n"
		<< "\tvectorW  = " << vectorWidth << "\n"
		<< "\tmaskPos  = " << maskPos << "\n"
		<< "\tresultSh = " << resultShape.str() << "\n"
		<< "\tparamShs: {\n";
	auto itScalarArg= scalarFn->arg_begin();
	auto itVectorArg= vectorFn->arg_begin();

	uint i = 0;
	for (VectorShape argShape : argShapes) {
		out << "\t\t(" << i << ") " << *itScalarArg << " -> " << *itVectorArg << " : " << argShape.str() << "\n";
		++i; ++itScalarArg; ++itVectorArg;
	}

	out << "\t}\n";
	out << "}\n";
}


} /* namespace rv */


