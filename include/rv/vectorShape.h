//===- vectorShape.h -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//

#ifndef INCLUDE_RV_VECTORSHAPE_H_
#define INCLUDE_RV_VECTORSHAPE_H_

#include <map>
#include <string>
#include <vector>

#include <llvm/Support/raw_ostream.h>

namespace llvm {
class Constant;
}

namespace rv {

// describes how the contents of a vector vary with the vectorized dimension
class VectorShape {
  int stride;
  bool hasConstantStride;
  uint alignment; // NOTE: General alignment if not hasConstantStride, else alignment of first
  bool defined;

  VectorShape(unsigned _alignment);              // varying
  VectorShape(int _stride, unsigned _alignment); // strided

public:
  VectorShape(); // undef

  bool isDefined() const { return defined; }
  int getStride() const { return stride; }
  unsigned getAlignmentFirst() const { return alignment; }

  // The maximum common alignment for every possible entry (<6, 8, 10, ...> -> 2)
  unsigned getAlignmentGeneral() const;

  void setAlignment(unsigned newAlignment) { alignment = newAlignment; }
  void setStride(int newStride) { hasConstantStride = true; stride = newStride; }
  void setVarying(uint newAlignment) { hasConstantStride = false; alignment = newAlignment; }

  bool isVarying() const { return defined && !hasConstantStride; }
  bool hasStridedShape() const { return defined && hasConstantStride; }
  bool isStrided(int ofStride) const { return hasStridedShape() && stride == ofStride; }
  bool isStrided() const { return hasStridedShape() && stride != 0 && stride != 1; }
  bool isUniform() const { return isStrided(0); }
  inline bool isContiguous() const { return isStrided(1); }

  static VectorShape varying(int aligned = 1) { return VectorShape(aligned); }
  static VectorShape strided(int stride, int aligned = 1) { return VectorShape(stride, aligned); }
  static inline VectorShape uni(int aligned = 1) { return strided(0, aligned); }
  static inline VectorShape cont(int aligned = 1) { return strided(1, aligned); }
  static VectorShape undef() { return VectorShape(); } // bot

  static VectorShape fromConstant(const llvm::Constant* C);

  static VectorShape join(VectorShape a, VectorShape b);

  bool operator==(const VectorShape &a) const;
  bool operator!=(const VectorShape &a) const;
  bool operator<(const VectorShape &a) const;

  friend VectorShape operator-(const VectorShape& a);
  friend VectorShape operator+(const VectorShape& a, const VectorShape& b);
  friend VectorShape operator-(const VectorShape& a, const VectorShape& b);
  friend VectorShape operator*(int m, const VectorShape& a);
  friend VectorShape truncateToTypeSize(const VectorShape &a, unsigned typeSize);

  std::string str() const;

  static VectorShape truncateToTypeSize(const VectorShape &a,
                                        unsigned typeSize);

  friend llvm::raw_ostream &operator<<(llvm::raw_ostream &O,
                                       const VectorShape &shape) {
    return O << shape.str();
  }
};

typedef std::vector<VectorShape> VectorShapeVec;
}

#endif /* INCLUDE_RV_VECTORSHAPE_H_ */
