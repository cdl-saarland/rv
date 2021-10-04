//===- src/utils.cpp - asorted auxiliary functions --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "rv/utils.h"

#include "rv/PlatformInfo.h"
#include "rv/vectorMapping.h"
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/IRBuilder.h>

using namespace llvm;

namespace rv {

void MaterializeEntryMask(Function &F, rv::PlatformInfo &platInfo) {
  auto &entryMaskFunc =
      platInfo.requestRVIntrinsicFunc(rv::RVIntrinsic::EntryMask);
  auto &entry = F.getEntryBlock();

  // create a new dedicated entry block
  auto *guardedEntry = entry.splitBasicBlock(entry.begin());
  entry.getTerminator()->eraseFromParent();

  // insert a dedicated exit block right after @entry.
  auto itInsertBlock = F.begin();
  itInsertBlock++;
  auto &exitBlock =
      *BasicBlock::Create(F.getContext(), "", &F, &*itInsertBlock);

  IRBuilder<> builder(&entry);
  auto *entryMask = builder.CreateCall(&entryMaskFunc, {}, "implicit_wfv_mask");
  builder.CreateCondBr(entryMask, guardedEntry, &exitBlock);

  // return <undef>
  auto &retTy = *F.getFunctionType()->getReturnType();
  builder.SetInsertPoint(&exitBlock);
  if (retTy.isVoidTy()) {
    builder.CreateRetVoid();
  } else {
    builder.CreateRet(UndefValue::get(&retTy));
  }
  // TODO update DT, PDF (in FAM)
}

bool
parseVectorMapping(Function & scalarFn, StringRef & attribText, VectorMapping & mapping, bool createMissingDecl) {
  // FIXME use LLVM VectorUtils
  if (!attribText.startswith("_ZGV")) return false;

  if (attribText.size() < 6) return false;

  // parse general vector attribs
  // int vecRegisterBits = ParseRegisterWidth(attribText[4]); // TODO use ISA hint for rvConfig

  bool needsMask = attribText[5] == 'M';

  // parse vectorization factor
  char * pos; // = attribText.begin() + 6; // "_ZGV<API><NeedsMask><VectorWidth>"
  unsigned vectorWidth = strtol(attribText.begin() + 6, &pos, 10);

  // process arument shapes
  VectorShapeVec argShapes;

  auto * endText = attribText.end();

  for (; pos != endText && *pos != '_'; ) {
    char token = *pos;
    switch(token) {
      case 'v': argShapes.push_back(VectorShape::varying()); ++pos; break;
      case 'u': argShapes.push_back(VectorShape::uni()); ++pos; break;
      case 'a': {
        char * nextPos;
        ++pos;
        auto alignVal = strtol(pos, &nextPos, 10);
        pos = nextPos;

        int lastArgIdx = argShapes.size() - 1;
        assert(lastArgIdx >= 0);
        argShapes[lastArgIdx].setAlignment(alignVal);
      } break;
      case 'l': {
        ++pos;
        char * nextPos;
        auto strideVal = strtol(pos, &nextPos, 10);
        pos = nextPos;

        argShapes.push_back(VectorShape::strided(strideVal));
      } break;
      // case 's': // ??

      default:
        abort();
    }
  }

  Function * simdDecl = scalarFn.getParent()->getFunction(attribText);
  if (!createMissingDecl && !simdDecl) return false;

  mapping.scalarFn = &scalarFn;
  mapping.resultShape = VectorShape::varying();
  mapping.argShapes = argShapes;
  mapping.maskPos = needsMask ? std::max<int>(0, argShapes.size()) : -1; // TODO what's the right vector mask position?
  mapping.predMode = needsMask ? CallPredicateMode::PredicateArg : CallPredicateMode::Unpredicated;
  mapping.vectorWidth = vectorWidth;
  if (simdDecl) {
    mapping.vectorFn = simdDecl;
  } else {
    mapping.vectorFn = createVectorDeclaration(scalarFn, mapping.resultShape, mapping.argShapes, vectorWidth, mapping.maskPos);
    mapping.vectorFn->setName(attribText);
    mapping.vectorFn->setLinkage(GlobalValue::ExternalLinkage); // FIXME for debugging
  }

  return true;
}

Type*
vectorizeType(Type* scalarTy, VectorShape shape, unsigned vectorWidth)
{
    if (scalarTy->isVoidTy()) return scalarTy;
    if (!shape.isDefined() || shape.hasStridedShape()) return scalarTy;

    return FixedVectorType::get(scalarTy, vectorWidth);
}

Function*
createVectorDeclaration(Function& scalarFn, VectorShape resShape,
                        const VectorShapeVec& argShapes, unsigned vectorWidth,
                        int maskPos)
{
    auto* scalarFnTy = scalarFn.getFunctionType();

    auto* vectorRetTy = vectorizeType(scalarFnTy->getReturnType(), resShape, vectorWidth);

    std::vector<Type*> vectorArgTys;
    bool hasMaskArg = maskPos >= 0;
    int argIdx = 0; // scalar fn argument Idx
    for (int i = 0; i < (int) scalarFnTy->getNumParams() + hasMaskArg; ++i) {
        if (i != maskPos) {
          // actual scalar function argument
          auto* scalarArgTy = scalarFnTy->getParamType(argIdx);
          VectorShape argShape = argShapes[argIdx];
          vectorArgTys.push_back(vectorizeType(scalarArgTy, argShape, vectorWidth));
          ++argIdx;
        } else {
          // mask argument
          auto* boolTy = IntegerType::getInt1Ty(scalarFn.getContext());
          VectorShape argShape = VectorShape::varying();
          vectorArgTys.push_back(vectorizeType(boolTy, argShape, vectorWidth));
        }
    }

    auto* vectorFnTy = FunctionType::get(vectorRetTy, vectorArgTys, false);

    auto * vectorFn = llvm::Function::Create(vectorFnTy, scalarFn.getLinkage(), scalarFn.getName() + "_SIMD",
                                  scalarFn.getParent());

    if (scalarFn.hasPersonalityFn()) {
      vectorFn->setPersonalityFn(scalarFn.getPersonalityFn());
    }

    return vectorFn;
}

} // namespace rv
