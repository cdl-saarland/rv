#ifndef RV_ANNOTATIONS_H
#define RV_ANNOTATIONS_H

#include "rv/analysis/reductions.h"

namespace llvm {
  class Function;
  class PHINode;
}

namespace rv {
  void MarkAsCriticalSection(llvm::Function & func);
  bool IsCriticalSection(const llvm::Function & func);

  void SetReductionHint(llvm::PHINode & loopHeaderPhi, RedKind redKind);
  RedKind ReadReductionHint(const llvm::PHINode & loopHeaderPhi);
}

#endif
