//===- Utils.cpp -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @author montada

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Constants.h>
#include <utils/rvTools.h>
#include "Utils.h"

using namespace llvm;
using namespace rv;

Type *getVectorType(Type *type, unsigned width) {
  if (type->isVoidTy())
    return type;
  else
    return VectorType::get(type, width);
}

Value *createContiguousVector(unsigned width, Type *type, int start) {
  Constant *constants[width];
  for (unsigned i = 0; i < width; ++i) {
    constants[i] = ConstantInt::get(type, i + start);
  }
  return ConstantVector::get(ArrayRef<Constant *>(constants, width));
}

BasicBlock *createCascadeBlocks(Function *insertInto, unsigned vectorWidth,
                                std::vector<BasicBlock *> &condBlocks,
                                std::vector<BasicBlock *> &maskedBlocks) {
  BasicBlock *cond, *load;
  for (unsigned lane = 0; lane < vectorWidth; ++lane) {
    cond = BasicBlock::Create(insertInto->getContext(), "cascade_cond_" + std::to_string(lane),
                              insertInto);
    load = BasicBlock::Create(insertInto->getContext(), "cascade_masked_" + std::to_string(lane),
                              insertInto);
    condBlocks.push_back(cond);
    maskedBlocks.push_back(load);
  }
  return BasicBlock::Create(insertInto->getContext(), "cascade_end", insertInto);
}

void addSIMDMappingsFor(rv::PlatformInfo &platformInfo, Function *function) {
  // TODO: wait for Simons wisdom on how the fuck to do this
  // TODO: only add SLEEF mappings if target has no intrinsic
  LLVMContext &C = getGlobalContext();
  Module *mod = createModuleFromFile("sleefsrc/sleefsimdsp_avx.ll", C); // TODO: AVX2, SSE2, double
  unsigned vectorWidth = 8;

  for (auto it = mod->begin(), et = mod->end(); it != et; ++it) {
    Function *simdFunction = &*it;
    if ("x" + function->getName().str() == simdFunction->getName().str()) {
      VectorShapeVec argShapes;
      for (unsigned i = 0; i < simdFunction->getArgumentList().size(); ++i) {
        argShapes.push_back(VectorShape::varying(function->getParamAlignment(i)));
      }
      // TODO: this will cause a memory leak
      VectorMapping *mapping = new VectorMapping(function, simdFunction, vectorWidth, 0,
                                                 VectorShape::varying(simdFunction->getAlignment()), argShapes);
      platformInfo.addMapping(function, mapping);
    }
  }
}
