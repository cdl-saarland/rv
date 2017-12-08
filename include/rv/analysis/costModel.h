#ifndef RV_ANALYSIS_COSTMODEL_H
#define RV_ANALYSIS_COSTMODEL_H

#include <cstddef>

namespace rv {

class PlatformInfo;
class VectorizationInfo;

class CostModel {
  PlatformInfo & platInfo;

public:
  CostModel(PlatformInfo & _platInfo);
  size_t pickWidth(VectorizationInfo & vecInfo);
};

}

#endif // RV_ANALYSIS_COSTMODEL_H
