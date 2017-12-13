#include "rv/annotations.h"
#include "rv/analysis/reductions.h"

#include "rvConfig.h"
#include <cassert>

#include "llvm/IR/Function.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Constants.h"

using namespace llvm;

namespace {
  const char* rv_atomic_string = "rv_atomic";
  const char* rv_redkind_string  = "rv_redkind";
}

namespace rv {

bool
IsCriticalSection(const llvm::Function & func) {
  auto * mdNode = func.getMetadata(rv_atomic_string);
  return (bool) mdNode;
}

void
MarkAsCriticalSection(llvm::Function & func) {
  // mark function as "noinline" to survive O3
  func.removeAttribute(AttributeList::FunctionIndex, Attribute::AlwaysInline);
  func.addAttribute(AttributeList::FunctionIndex, Attribute::NoInline);
  func.setMetadata(rv_atomic_string, MDNode::get(func.getContext(), {}));
}

void
SetReductionHint(llvm::PHINode & loopHeaderPhi, RedKind redKind) {
  StringRef redKindText = to_string(redKind);
  auto & ctx = loopHeaderPhi.getContext();

  auto * redKindNode = MDString::get(ctx, redKindText);
  auto * boxedNode = MDNode::get(ctx, redKindNode);
  loopHeaderPhi.setMetadata(rv_redkind_string, boxedNode);
}

RedKind
ReadReductionHint(const llvm::PHINode & loopHeaderPhi) {
  auto * boxedHint = loopHeaderPhi.getMetadata(rv_redkind_string);
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
