#include "rv/MaskBuilder.h"

#include "llvm/IR/PatternMatch.h"
#include <cassert>

using namespace llvm;

// TODO integrate with PatternMatch.h
static bool rv_m_Any(Value &condVal, Value *&oMask) {
  auto *call = dyn_cast<CallInst>(&condVal);
  if (!call)
    return false;

  auto *callee = dyn_cast<Function>(call->getCalledValue());
  if (!callee)
    return false;

  if (callee->getName() == "rv_any") {
    oMask = call->getArgOperand(0);
    return true;
  }

  return false;
}

static Value &CreatePredicateAnd(IRBuilder<> &builder, Value &lhs, Value &rhs,
                                 const Twine &name = Twine()) {
  using namespace llvm::PatternMatch;

  // Optimize for a common pattern
  Value *anyTestedMask;
  if (rv_m_Any(rhs, anyTestedMask)) {
    Value *X, *Y, *Z = nullptr;
    // lhs = and x (not y)
    // rhs = any (not y)
    if (match(&lhs, m_And(m_Value(X), m_Not(m_Value(Y)))) &&
        match(anyTestedMask, m_Not(m_Value(Z))) && (Y == Z)) {
      return lhs; // and lhs rhs
    }
  }

  return *builder.CreateAnd(&lhs, &rhs, name);
}

namespace rv {

Mask MaskBuilder::FoldAVL(llvm::IRBuilder<> &Builder, Mask M, Twine Name) {
  abort(); // TODO implement (reuse logic from ExpandVectorPredicateion.cpp)
}

Mask MaskBuilder::CreateOr(llvm::IRBuilder<> &Builder, Mask A, Mask B,
                           Twine Name) {
  if (A == B)
    return A;

  // take max vlen component
  Value *NewAVL = nullptr;
  if (A.getAVL() == B.getAVL()) {
    NewAVL = A.getAVL();
  } else if (A.getAVL() && B.getAVL()) {
    NewAVL = Builder.CreateMaximum(A.getAVL(), B.getAVL(), Name + ".vl");
  }

  // OR bitmask component
  Value *NewPred = nullptr;
  if (A.getPred() == B.getPred()) {
    NewPred = A.getPred();
  } else if (A.getPred() && B.getPred()) {
    NewPred = Builder.CreateOr(A.getPred(), B.getPred(), Name + ".m");
  }

  return Mask(NewPred, NewAVL);
}

Mask MaskBuilder::CreateAnd(llvm::IRBuilder<> &Builder, Mask A, Mask B,
                            Twine Name) {
  if (A == B)
    return A;

  // take min vlen component
  Value *NewAVL = A.getAVL() ? A.getAVL() : B.getAVL();
  if (A.getAVL() && B.getAVL()) {
    NewAVL = Builder.CreateMinimum(A.getAVL(), B.getAVL(), Name + ".vl");
  }

  // AND bitmask component
  Value *NewPred = A.getPred() ? A.getPred() : B.getPred();
  if (A.getPred() == B.getPred()) {
    NewPred = A.getPred();
  } else if (A.getPred() && B.getPred()) {
    NewPred =
        &CreatePredicateAnd(Builder, *A.getPred(), *B.getPred(), Name + ".m");
  }

  return Mask(NewPred, NewAVL);
}

Mask MaskBuilder::CreateNot(llvm::IRBuilder<> &Builder, Mask M, Twine Name) {
  // Lower AVL into pred
  if (M.getAVL()) {
    M = FoldAVL(Builder, M, Name);
  }

  // all-false case
  if (M.knownAllFalse()) {
    return Mask::getAllTrue();
  }

  // all-true corner case
  if (!M.getPred()) {
    return Mask::getAllFalse(Builder.getContext());
  }

  return M;
}

} // namespace rv
