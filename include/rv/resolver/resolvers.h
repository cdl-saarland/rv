#ifndef RV_RESOLVERS_H
#define RV_RESOLVERS_H

#include "rv/config.h"
#include "rv/PlatformInfo.h"

namespace rv {
  // function resolver relying on TargetLibraryInfo.
  void addTLIResolver(const Config & config, PlatformInfo & platInfo);

  // Use the SLEEF library to implement math functions.
  // unit for ulpErrorBound is tenth of ULP (a value of 10 implies that an ULP error of <= 1.0 is acceptable)
  void addSleefResolver(const Config & config, PlatformInfo & platInfo, unsigned ulpErrorBound);

  // Vectorize functions that are declares with "pragma omp declare simd".
  void addOpenMPResolver(const Config & config, PlatformInfo & platInfo);
}

#endif
