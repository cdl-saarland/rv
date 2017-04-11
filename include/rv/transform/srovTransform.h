#ifndef RV_TRANSFORM_SROVTRANSFORM_H
#define RV_TRANSFORM_SROVTRANSFORM_H

#include <llvm/IR/Value.h>
#include <llvm/Transforms/Utils/ValueMapper.h>

#include "rv/PlatformInfo.h"
#include "rv/vectorShape.h"

namespace llvm {
  class AllocaInst;
  class DataLayout;
}

namespace rv {

class VectorizationInfo;

class SROVTransform {
  VectorizationInfo & vecInfo;
  const PlatformInfo & platInfo;

public:
  SROVTransform(VectorizationInfo & _vecInfo, const PlatformInfo & _platInfo);

  bool run();
};


} // namespace rv

#endif// RV_TRANSFORM_SROVTRANSFORM_H
