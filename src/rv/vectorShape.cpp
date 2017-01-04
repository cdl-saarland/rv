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

VectorShape::VectorShape()
        : stride(0), hasConstantStride(false), alignment(0), defined(false)
{}

VectorShape::VectorShape(uint _alignment)
        : stride(0), hasConstantStride(false), alignment(_alignment), defined(true)
{}

// constant stride constructor
VectorShape::VectorShape(int _stride, unsigned _alignment)
        : stride(_stride), hasConstantStride(true), alignment(_alignment), defined(true)
{}

bool VectorShape::operator==(const VectorShape &a) const {
  return !defined && !a.defined ||
          defined && a.defined && alignment == a.alignment &&
         (!hasConstantStride && !a.hasConstantStride ||
          hasConstantStride && a.hasConstantStride && stride == a.stride);
}

bool VectorShape::operator!=(const VectorShape &a) const {
  return !(*this == a);
}

bool VectorShape::operator<(const VectorShape &a) const {
  if (!a.isDefined()) return false; // Cannot be more precise than bottom
  if (!isDefined()) return true; // Bottom is most precise

  if (!hasConstantStride && a.hasConstantStride) return true; // strided < varying

  // If both are of the same shape, decide by alignment
  if (!hasConstantStride && !a.hasConstantStride ||
      hasConstantStride && a.hasConstantStride && stride == a.stride)
      return alignment % a.alignment == 0 && alignment > a.alignment;

  return false;
}

VectorShape
VectorShape::join(VectorShape a, VectorShape b) {
  if (!a.isDefined()) return b;
  if (!b.isDefined()) return a;

  const unsigned aligned = gcd<>(a.alignment, b.alignment);
  if (a.hasConstantStride && b.hasConstantStride && a.getStride() == b.getStride()) {
    return strided(a.stride, aligned);
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
