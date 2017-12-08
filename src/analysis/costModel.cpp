#include "rv/analysis/costModel.h"

#include "rv/PlatformInfo.h"
#include "rv/vectorizationInfo.h"

namespace rv {


CostModel::CostModel(PlatformInfo & _platInfo)
: platInfo(_platInfo)
{}

size_t
CostModel::pickWidth(VectorizationInfo & vecInfo) {
}


}
