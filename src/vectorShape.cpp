//===- vectorShape.cpp -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @author kloessner, simon

#include <iostream>
#include <sstream>
#include <cmath>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>

#include "utils/mathUtils.h"

#include "rv/vectorShape.h"
#include "rv/vectorizationInfo.h"

using namespace llvm;

namespace {

unsigned getAlignment(const Constant* c) {
  assert (c);

  if (isa<BasicBlock>(c) || isa<Function>(c))
    return 1;

  // An undef value is never aligned.
  if (isa<UndefValue>(c)) return 1;

  if (const ConstantInt* cint = dyn_cast<ConstantInt>(c)) {
    return static_cast<unsigned>(std::abs(cint->getSExtValue()));
  }

  // Other than that, only integer vector constants can be aligned.
  if (!c->getType()->isVectorTy()) return 1;

  // A zero-vector is aligned.
  if (isa<ConstantAggregateZero>(c)) return 0;

  if (const ConstantDataVector* cdv = dyn_cast<ConstantDataVector>(c)) {
    if (!cdv->getElementType()->isIntegerTy()) return 1;

    const int intValue = (int) cast<ConstantInt>(cdv->getAggregateElement(0U))->getSExtValue();

    return static_cast<unsigned>(std::abs(intValue));
  }

  assert (isa<ConstantVector>(c));
  const ConstantVector* cv = cast<ConstantVector>(c);

  if (!cv->getType()->getElementType()->isIntegerTy()) return 1;

  assert (isa<ConstantInt>(cv->getOperand(0)));
  const ConstantInt* celem = cast<ConstantInt>(cv->getOperand(0));
  const int intValue = (int) celem->getSExtValue();

  // The vector is aligned if its first element is aligned
  return static_cast<unsigned>(std::abs(intValue));
}

}

namespace rv {

VectorShape::VectorShape()
    : stride(0), hasConstantStride(false), alignment(0), defined(false) {}

VectorShape::VectorShape(uint _alignment)
    : stride(0), hasConstantStride(false), alignment(_alignment),
      defined(true) {}

// constant stride constructor
VectorShape::VectorShape(int _stride, unsigned _alignment)
    : stride(_stride), hasConstantStride(true), alignment(_alignment),
      defined(true) {}

VectorShape VectorShape::fromConstant(const Constant* C) {
  return VectorShape::uni(getAlignment(C));
}

unsigned VectorShape::getAlignmentGeneral() const {
  assert(defined && "alignment function called on undef value");

  if (hasConstantStride) {
    if (stride == 0)
      return alignment;
    else
      return gcd(alignment, (unsigned) std::abs(stride));
  }
  else
    return alignment; // General alignment in case of varying shape
}

bool VectorShape::operator==(const VectorShape &a) const {
  return
      // both undef
      (!defined && !a.defined) ||

      // both are defined shapes
      (defined && a.defined && alignment == a.alignment && (
           // either both shapes are varying (with same alignment)
           (!hasConstantStride && !a.hasConstantStride) ||
           // both shapes are strided with same alignment
           (hasConstantStride && a.hasConstantStride && stride == a.stride)
        )
      );
}

bool VectorShape::operator!=(const VectorShape &a) const {
  return !(*this == a);
}

bool VectorShape::operator<(const VectorShape &a) const {
  if (!a.isDefined())
    return false; // Cannot be more precise than bottom
  if (!isDefined())
    return true; // Bottom is more precise then any defined shape

  if (hasConstantStride && !a.hasConstantStride)
    return true; // strided < varying

  // If both are of the same shape, decide by alignment
  if ((!hasConstantStride && !a.hasConstantStride) ||
      (hasConstantStride && a.hasConstantStride && stride == a.stride))
    return alignment % a.alignment == 0 && alignment > a.alignment;

  return false;
}

VectorShape operator-(const VectorShape& a) {
  if (!a.defined || !a.hasConstantStride) return a;
  return VectorShape::strided(-a.stride, a.alignment);
}

VectorShape operator+(const VectorShape& a, const VectorShape& b) {
  if (!a.defined || !b.defined)
    return VectorShape::undef();

  if (!a.hasConstantStride || !b.hasConstantStride)
    return VectorShape::varying(gcd(a.getAlignmentGeneral(), b.getAlignmentGeneral()));

  return VectorShape::strided(a.stride + b.stride, gcd(a.alignment, b.alignment));
}

VectorShape operator-(const VectorShape& a, const VectorShape& b) {
  if (!a.defined || !b.defined)
    return VectorShape::undef();

  if (!a.hasConstantStride || !b.hasConstantStride)
    return VectorShape::varying(gcd(a.getAlignmentGeneral(), b.getAlignmentGeneral()));

  return VectorShape::strided(a.stride - b.stride, gcd(a.alignment, b.alignment));
}

VectorShape operator*(int m, const VectorShape& a) {
  if (!a.defined) return a;

  if (!a.hasConstantStride) return VectorShape::varying(((m > 0) ? m : -m) * a.alignment);

  return VectorShape::strided(m * a.stride, ((m > 0) ? m : -m) * a.alignment);
}

VectorShape VectorShape::join(VectorShape a, VectorShape b) {
  if (!a.isDefined())
    return b;
  if (!b.isDefined())
    return a;

  if (a.hasConstantStride && b.hasConstantStride && a.getStride() == b.getStride()) {
    return strided(a.stride, gcd<>(a.alignment, b.alignment));
  } else {
    return varying(gcd(a.getAlignmentGeneral(), b.getAlignmentGeneral()));
  }
}

std::string VectorShape::str() const {
  if (!isDefined()) {
    return "undef_shape";
  }

  std::stringstream ss;
  if (isVarying()) {
    ss << "varying";
  } else if (isUniform()) {
    ss << "uni";
  } else if (isContiguous()) {
    ss << "cont";
  } else {
    ss << "stride(" << stride << ")";
  }

  if (alignment > 1) {
    ss << ", alignment(" << alignment << ", " << getAlignmentGeneral() << ")";
  }

  return ss.str();
}

VectorShape truncateToTypeSize(const VectorShape &a, unsigned typeSize) {
  if (!a.isDefined()) return a;

  // FIXME
    // observed in SimpleBarrier2D:
    // %41 = <someInst> : stride(32)
    // %42 = trunc i64 i64 %41 to i32 : uni

  // truncate to uniform rule
  if (typeSize == 1 && a.hasStridedShape() && a.getStride() % 2 == 0) {
    return VectorShape::uni();
  }

  return a;
}


}
