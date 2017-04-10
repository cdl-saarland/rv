#include "rv/transform/srovTransform.h"

#include <vector>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Instructions.h>
#include <llvm/ADT/SmallSet.h>
#include <llvm/Transforms/Utils/ValueMapper.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/DataLayout.h>

#include <rv/vectorizationInfo.h>


#include <rvConfig.h>

using namespace rv;
using namespace llvm;

#if 1
#define IF_DEBUG_SROV IF_DEBUG
#else
#define IF_DEBUG_SROV if (false)
#endif

namespace rv {

SROVTransform::SROVTransform(VectorizationInfo & _vecInfo, const PlatformInfo & _platInfo)
: vecInfo(_vecInfo)
, platInfo(_platInfo)
{}

bool
SROVTransform::run() {
  // TODO implement
  return false;
}

}
