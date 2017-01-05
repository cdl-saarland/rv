//===- vectorShape.h -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//

#ifndef INCLUDE_RV_VECTORSHAPE_H_
#define INCLUDE_RV_VECTORSHAPE_H_


#include <vector>
#include <string>
#include <map>

#include <llvm/Support/raw_ostream.h>

namespace rv {


// describes how the contents of a vector vary with the vectorized dimension
class VectorShape {
  bool defined;
	int stride;
	bool hasConstantStride;
	unsigned alignment;

  VectorShape(unsigned _alignment); // varying
  VectorShape(int _stride, unsigned _alignment); // strided

public:
  VectorShape(); // undef

  bool isDefined() const { return defined; }
	int getStride() const { return stride; }
	unsigned getAlignment() const { return alignment; }

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

  static VectorShape join(VectorShape a, VectorShape b);

	bool operator==(const VectorShape& a) const;
	bool operator!=(const VectorShape& a) const;
	bool operator< (const VectorShape& a) const;

	static VectorShape truncateToTypeSize(const VectorShape& a, unsigned typeSize)
	{
		// FIXME can this become unaligned?
		// This selects only the last typeSize digits
		unsigned lastDigitsMask = (1U << typeSize) - 1U;
		return VectorShape(a.getStride() & lastDigitsMask, a.alignment);
	}

	std::string str() const;

	friend llvm::raw_ostream& operator<<(llvm::raw_ostream& O, const VectorShape& shape)
	{
		return O << shape.str();
	}
};

typedef std::vector<VectorShape> VectorShapeVec;

}




#endif /* INCLUDE_RV_VECTORSHAPE_H_ */
