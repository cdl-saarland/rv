#include "rv/MaskBuilder.h"

#include "utils/rvTools.h"
#include "llvm/IR/PatternMatch.h"
#include <cassert>

using namespace llvm;

static Value &MakeTotalOperation(Value &Val) {
  auto *inst = dyn_cast<Instruction>(&Val);
  if (inst) {
    rv::setTotalOperationTag(*inst);
  }
  return Val;
}

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

namespace rv {

llvm::Value &ScalarMaskBuilder::ComputeInstShape(llvm::Value &V) {
  auto *I = dyn_cast<Instruction>(&V);
  if (!I)
    return V;

  VectorShape Accu = VectorShape::undef();
  assert(!isa<PHINode>(I) && "cannot compute phi shapes locally");
  for (Use &OpUse : I->operands()) {
    auto *OpI = dyn_cast<Instruction>(OpUse.get());
    if (!OpI)
      continue;
    Accu = VectorShape::join(Accu, VecInfo.getVectorShape(*OpI));
  }
  VecInfo.setVectorShape(*I, Accu);
  return V;
}

llvm::Value &ScalarMaskBuilder::AddMaskVal(llvm::Value &Op) {
  return MakeTotalOperation(ComputeInstShape(Op));
}

Value &MaskBuilder::CreatePredicateAnd(IRBuilder<> &builder, Value &lhs,
                                       Value &rhs, const Twine &name) {
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
  Value *Anded = builder.CreateAnd(&lhs, &rhs, name);
  return AddMaskOp<>(*Anded);
}

Mask MaskBuilder::FoldAVL(llvm::IRBuilder<> &Builder, Mask M, Twine Name) {
  abort(); // TODO implement (reuse logic from ExpandVectorPredication.cpp)
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
    // max
    Value &IsLT = AddMaskOp<>(
        *Builder.CreateICmpULT(A.getAVL(), B.getAVL(), Name + ".lt.avl"));
    NewAVL = &AddMaskOp<>(*Builder.CreateSelect(&IsLT, B.getAVL(), A.getAVL(),
                                                Name + ".max.avl"));
  }

  // OR bitmask component
  Value *NewPred = nullptr;
  if (A.getPred() == B.getPred()) {
    NewPred = A.getPred();
  } else if (A.getPred() && B.getPred()) {
    NewPred =
        &AddMaskOp<>(*Builder.CreateOr(A.getPred(), B.getPred(), Name + ".m"));
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
    // min
    Value &IsLT = AddMaskOp<>(
        *Builder.CreateICmpULT(A.getAVL(), B.getAVL(), Name + ".lt.avl"));
    NewAVL = &AddMaskOp<>(*Builder.CreateSelect(&IsLT, A.getAVL(), B.getAVL(),
                                                Name + ".min.avl"));
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

  // Invert the predicate
  auto NotM = &AddMaskOp<>(
      *Builder.CreateNot(M.getPred(), "not." + M.getPred()->getName()));
  return Mask::inferFromPredicate(*NotM);
}

llvm::Value *MaskBuilder::CreateSelect(llvm::IRBuilder<> &Builder,
                                       Mask CondMask, llvm::Value *OnTrueVal,
                                       llvm::Value *OnFalseVal,
                                       llvm::Value *ContextEVL,
                                       llvm::Twine Name) {

  Mask ParentMsk =
      ContextEVL ? Mask::getAllTrue() : Mask::fromVectorLength(*ContextEVL);

  if (CondMask.knownImplies(ParentMsk)) {
    return OnTrueVal; // TODO emit llvm.vp.compose with \p ContextEVL mask
  }

  if (CondMask.knownAllFalsePred()) {
    // TODO account for AVL as well
    return OnFalseVal; // TODO emit llvm.vp.compose with \p ContextEVL mask
  }

  assert((ContextEVL == CondMask.getAVL()) &&
         "TODO implement composition with AVL");
  return &AddMaskOp<>(
      *Builder.CreateSelect(CondMask.getPred(), OnTrueVal, OnFalseVal, Name));
}

} // namespace rv
