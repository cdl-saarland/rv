#ifndef RV_TRANSFORM_SINGLERETURNTRANS_H
#define RV_TRANSFORM_SINGLERETURNTRANS_H

#include <rv/region/Region.h>

namespace rv {

// join all return terminators in the region (needed for linearization in case there is control divergence around returns)
struct SingleReturnTrans {
  static bool run(Region & region);
};

}

#endif // RV_TRANSFORM_SINGLERETURNTRANS_H
