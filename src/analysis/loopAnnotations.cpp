#include "rv/analysis/loopAnnotations.h"

#include <llvm/IR/Metadata.h>
#include <llvm/Analysis/LoopInfo.h>

#include "rvConfig.h"

using namespace llvm;

namespace rv {

raw_ostream&
LoopMD::print(raw_ostream & out) const {
  out << "LoopMD {";
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

  // re-concile the conflicting readings (if any)
  return OptimisticJoin(std::move(llvmAnnot), std::move(rvAnnot));
}


} // namespace rv
