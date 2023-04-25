//===- src/analysis/loopAnnotations.cpp - loop md reader/writer --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "rv/analysis/loopAnnotations.h"

#include <llvm/IR/Constants.h>
#include <llvm/IR/Metadata.h>
#include <llvm/Analysis/LoopInfo.h>

#include "rvConfig.h"

using namespace llvm;

namespace rv {

raw_ostream&
LoopMD::print(raw_ostream & out) const {
  out << "LoopMD {";
  if (alreadyVectorized.isSet()) out << "alreadyVectorized = " << alreadyVectorized.get() << ", ";
  if (vectorizeEnable.isSet()) out << "vectorizeEnable = " << vectorizeEnable.get() << ", ";
  if (minDepDist.isSet()) out << "minDepDist = " << DepDistToString(minDepDist.get()) << ", ";
  if (explicitVectorWidth.isSet()) out << "explicitVectorWidth = " << explicitVectorWidth.get() << ", ";
  out << "}";
  return out;
}

void
LoopMD::dump() const {
  print(errs()) << "\n";
}

std::string
DepDistToString(iter_t depDist) {
  if (depDist == ParallelDistance) {
    return "unbounded";
  } else {
    return std::to_string(depDist);
  }
}

// optimistic reading of two conflicting loop annotations
LoopMD
OptimisticJoin(LoopMD && A, LoopMD && B) {
  LoopMD md;

  // don't re-vectorize
  if (A.alreadyVectorized.isSet() || B.alreadyVectorized.isSet()) {
    md.vectorizeEnable = A.alreadyVectorized.safeGet(false) || B.alreadyVectorized.safeGet(false);
  }

  // enable vectorization if any hint says so
  if (A.vectorizeEnable.isSet() || B.vectorizeEnable.isSet()) {
    md.vectorizeEnable = A.vectorizeEnable.safeGet(false) || B.vectorizeEnable.safeGet(false);
  }

  // use the maximal annotated minimum dependence distance
  if (A.minDepDist.isSet() || B.minDepDist.isSet()) {
    md.minDepDist = std::max<iter_t>(A.minDepDist.safeGet(1), B.minDepDist.safeGet(1));
  }

  // use the smallest explicitVectorWidth
  if (A.explicitVectorWidth.isSet() || B.explicitVectorWidth.isSet()) {
    md.explicitVectorWidth = std::min<iter_t>(A.explicitVectorWidth.safeGet(ParallelDistance), B.explicitVectorWidth.safeGet(ParallelDistance));
  }

  return md;
}


// parse loop annotations for loop \p L
LoopMD
GetLoopAnnotation(llvm::Loop & L) {
  auto *LID = L.getLoopID();

  // try to recover from latch
  if (!LID) {
    auto * latch = L.getLoopLatch();
    LID = latch->getTerminator()->getMetadata("llvm.loop");
    if (LID) IF_DEBUG { errs() << "Recovered loop MD from latch!\n"; }
  }

  // no info
  if (!LID) return LoopMD();

  // LLVM standard annotations for loop vectorization
  LoopMD llvmAnnot;

  // RV extended/by-passing annotations
  LoopMD rvAnnot;

  for (int i = 0, e = LID->getNumOperands(); i < e; i++) {
    const MDOperand &Op = LID->getOperand(i);
    auto *OpMD = dyn_cast<MDNode>(Op);
    if (!OpMD || OpMD->getNumOperands() != 2)
      continue;

    auto *Str = dyn_cast<MDString>(OpMD->getOperand(0));
    auto *Cst = dyn_cast<ConstantAsMetadata>(OpMD->getOperand(1));
    if (!Str || !Cst)
      continue;

    StringRef text = Str->getString();

    if (text.equals("llvm.loop.vectorize.enable")) {
      const bool vectorizeEnable = !Cst->getValue()->isNullValue();
      llvmAnnot.vectorizeEnable = vectorizeEnable;
      if (vectorizeEnable && !llvmAnnot.minDepDist.isSet()) {
        // unless specified otherwise, the annotated loop is parallel
        llvmAnnot.minDepDist = ParallelDistance;
      }

    } else if (text.equals("llvm.loop.vectorize.width")) {
      llvmAnnot.explicitVectorWidth = cast<ConstantInt>(Cst->getValue())->getSExtValue();

    } else if (text.equals("rv.loop.vectorize.enable")) {
      const bool vectorizeEnable = !Cst->getValue()->isNullValue();
      rvAnnot.vectorizeEnable = vectorizeEnable;
      if (vectorizeEnable && !rvAnnot.minDepDist.isSet()) {
        // unless specified otherwise, the annotated loop is parallel
        rvAnnot.minDepDist = ParallelDistance;
      }

    } else if (text.equals("rv.loop.vectorize.width")) {
      rvAnnot.explicitVectorWidth = cast<ConstantInt>(Cst->getValue())->getSExtValue();

    } else if (text.equals("rv.loop.mindepdist")) {
      rvAnnot.minDepDist = cast<ConstantInt>(Cst->getValue())->getSExtValue();
    }
  }

  // consider parallel loops without vectorization hint
  if (L.isAnnotatedParallel()) {
    llvmAnnot.minDepDist = ParallelDistance;
  }

  // re-concile the conflicting readings (if any)
  return OptimisticJoin(std::move(llvmAnnot), std::move(rvAnnot));
}


// Clear all loop vectorize annotations from the loop \p L.
void
ClearLoopVectorizeAnnotations(llvm::Loop & L) {
  auto & ctx = L.getHeader()->getContext();

  // empty loop metadata
  llvm::Metadata* tmpNode        = llvm::MDNode::getTemporary(ctx, llvm::None).release();

  // build loopID
  llvm::MDNode *emptyLoopMetadata = llvm::MDNode::get(ctx, {tmpNode});
  emptyLoopMetadata->replaceOperandWith(0, emptyLoopMetadata);

  L.setLoopID(emptyLoopMetadata);
}

void
AppendMDEntries(LLVMContext & ctx, std::vector<Metadata*> & mdArgs, const LoopMD & llvmLoopMD) {
  if (llvmLoopMD.alreadyVectorized.isSet()) {
    llvm::Metadata *mdAlreadyVectorized[] = { llvm::MDString::get(ctx, "llvm.loop.isvectorized"),
                                              llvm::ConstantAsMetadata::get(llvm::ConstantInt::get(llvm::Type::getInt1Ty(ctx), llvmLoopMD.alreadyVectorized.get()))};
    mdArgs.push_back(llvm::MDNode::get(ctx, mdAlreadyVectorized));
  }
  if (llvmLoopMD.vectorizeEnable.isSet()) {
    llvm::Metadata *mdVectorizeEnable[] = { llvm::MDString::get(ctx, "llvm.loop.vectorize.enable"),
                                              llvm::ConstantAsMetadata::get(llvm::ConstantInt::get(llvm::Type::getInt1Ty(ctx), llvmLoopMD.vectorizeEnable.get()))};
    mdArgs.push_back(llvm::MDNode::get(ctx, mdVectorizeEnable));
  }
  if (llvmLoopMD.explicitVectorWidth.isSet()) {
    llvm::Metadata *mdVectorWidth[] = { llvm::MDString::get(ctx, "llvm.loop.vectorize.width"),
                                              llvm::ConstantAsMetadata::get(llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), llvmLoopMD.explicitVectorWidth.get()))};
    mdArgs.push_back(llvm::MDNode::get(ctx, mdVectorWidth));
  }
}

// Encode \p loopMD as LLVM LoopVectorizer Metadata hints for the loop \p L.
void
SetLLVMLoopAnnotations(llvm::Loop & L, LoopMD && llvmLoopMD) {
  auto & ctx = L.getHeader()->getContext();

  std::vector<Metadata*> mdArgs;

  AppendMDEntries(ctx, mdArgs, llvmLoopMD);

  llvm::MDNode *emptyLoopMetadata = llvm::MDNode::get(ctx, mdArgs);
  emptyLoopMetadata->replaceOperandWith(0, emptyLoopMetadata);

  L.setLoopID(emptyLoopMetadata);
}

} // namespace rv
