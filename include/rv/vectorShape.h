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
	int stride;
	bool hasConstantStride;
	unsigned alignment;

public:
	// Divergent constructor
	VectorShape(unsigned _alignment = 1U)
	: stride(0)
	, hasConstantStride(false)
	, alignment(_alignment)
	{}

	// constant stride constructor
	VectorShape(int _stride, unsigned _alignment)
	: stride(_stride)
	, hasConstantStride(true)
	, alignment(_alignment)
	{}

  bool isDefined() const { return !(!hasConstantStride && (alignment == 0) && (stride == 0)); }
  bool hasStride(int testStride) const { return hasConstantStride && stride == testStride; }
	bool hasStridedShape() const { return hasConstantStride; }
	int getStride() const { return stride; }
	unsigned getAlignment() const { return alignment; }

	bool isUniform() const { return hasStridedShape() && getStride() == 0; }
	bool isStrided() const { return hasStridedShape() && getStride() != 0 && getStride() != 1; }
	inline bool isContiguous() const { return hasStride(1); }
	bool isVarying() const { return !hasStridedShape(); }

	static VectorShape varying(int aligned = 1) { return VectorShape(aligned); }
	static VectorShape strided(int stride, int aligned = 1) { return VectorShape(stride, aligned); }
	static VectorShape uni(int aligned = 1) { return VectorShape(0, aligned); }
	static VectorShape cont(int aligned = 1) { return VectorShape(1, aligned); }
	static VectorShape undef(int aligned = 1) { return VectorShape(0); } // bot

        static VectorShape join(VectorShape a, VectorShape b);

	bool operator==(const VectorShape& a) const;
	bool operator!=(const VectorShape& a) const;
	bool operator< (const VectorShape& a) const;

	friend VectorShape operator+(const VectorShape& a, const VectorShape& b);
	friend VectorShape operator-(const VectorShape& a, const VectorShape& b);
	friend VectorShape operator*(int strideMultiplier, const VectorShape& a);
	friend VectorShape operator/(const VectorShape& a, int strideDivisor);

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
