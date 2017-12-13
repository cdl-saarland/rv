#include "rv/annotations.h"
#include "rv/analysis/reductions.h"

#include "rvConfig.h"
#include <cassert>

#include "llvm/IR/Function.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Constants.h"

using namespace llvm;

namespace rv {

void
MarkAsCriticalSection(llvm::Function & func) {
  abort(); // TODO mark as noinline, add !rv_atomic MD node
}

void
SetReductionHint(llvm::PHINode & loopHeaderPhi, RedKind redKind) {
  StringRef redKindText = to_string(redKind);
  auto & ctx = loopHeaderPhi.getContext();

  auto * redKindNode = MDString::get(ctx, redKindText);
  auto * boxedNode = MDNode::get(ctx, redKindNode);
  loopHeaderPhi.setMetadata("rv_redkind", boxedNode);
}

RedKind
ReadReductionHint(llvm::PHINode & loopHeaderPhi) {
  auto * boxedHint = loopHeaderPhi.getMetadata("rv_redkind");
  if (!boxedHint) return RedKind::Bot; // unknown
  assert(boxedHint->getNumOperands() >= 1);

  auto * boxedRedCode = dyn_cast<MDString>(boxedHint->getOperand(0));
  assert(boxedRedCode);

  RedKind kind;
  bool parsed = from_string(boxedRedCode->getString(), kind);
  assert(parsed);
  return kind;
}

}
