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

#include "rv/utils/mathUtils.h"

#include "rv/pda/ProgramDependenceAnalysis.h"
#include "rv/vectorShape.h"
#include "rv/vectorizationInfo.h"

namespace rv {

VectorShape operator+(const VectorShape &a, const VectorShape &b) {
  const int aligned = a.alignment == b.alignment ? a.alignment : 1;

  if (a.isVarying() || b.isVarying())
    return VectorShape::varying(aligned);

  return VectorShape(a.stride + b.stride, aligned);
}

VectorShape operator-(const VectorShape &a, const VectorShape &b) {
  const int aligned = gcd<>(a.alignment, b.alignment);

  if (a.isVarying() || b.isVarying())
    return VectorShape::varying(aligned);

  return VectorShape(a.stride - b.stride, aligned);
}

VectorShape operator*(int strideMultiplier, const VectorShape &a) {
  const int aligned = strideMultiplier * a.alignment;

  if (a.isVarying())
    return VectorShape::varying(aligned);

  return VectorShape(a.stride * strideMultiplier, aligned);
}

VectorShape operator/(const VectorShape &a, int strideDivisor) {
  const int aligned =
      (a.alignment % strideDivisor == 0) ? (a.alignment / strideDivisor) : 1;

  if (a.isVarying() || a.stride % strideDivisor != 0)
    return VectorShape::varying(aligned);

  return VectorShape(a.stride / strideDivisor, aligned);
}

bool VectorShape::operator==(const VectorShape &a) const {
  return hasConstantStride == a.hasConstantStride && stride == a.stride &&
         alignment == a.alignment;
}

bool VectorShape::operator!=(const VectorShape &a) const {
  return !(*this == a);
}

bool VectorShape::operator<(const VectorShape &a) const {
  if (!isVarying() && a.isVarying())
    return true;

  // If both are of the same shape, decide by alignment
  if (!hasConstantStride && !a.hasConstantStride)
    return a.alignment % alignment == 0;

  if (hasConstantStride && a.hasConstantStride)
    if (stride == a.stride)
      return a.alignment % alignment == 0;

  return false;
}

VectorShape
VectorShape::join(VectorShape a, VectorShape b) {
      const unsigned aligned = gcd<>(a.alignment, b.alignment);
      if (a.hasStridedShape() && b.hasStridedShape() && a.getStride() == b.getStride()) {
	      return VectorShape(a.stride, aligned);
      } else {
	      return varying(aligned);
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
    ss << ", alignment(" << alignment << ")";
  }

  return ss.str();
}

}
