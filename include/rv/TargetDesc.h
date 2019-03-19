#ifndef RV_TARGETDESC_H
#define RV_TARGETDESC_H

#include <llvm/Support/raw_ostream.h>

namespace llvm {
  class Function;
}

namespace rv {

struct TargetDesc {
// target features
  bool useVE;
  bool useSSE;
  bool useAVX;
  bool useAVX2;
  bool useAVX512;
  bool useNEON;
  bool useADVSIMD;

// VP
  bool hasActiveVectorLength() const;

  // create default configuration (RV_ARCH env var)
  static TargetDesc createDefaultConfig();

  // auto-detect target machine features (SIMD ISAs) for function \p F.
  static TargetDesc createForFunction(llvm::Function & F);

  TargetDesc();

  void print(llvm::raw_ostream&) const;
};

}

#endif // RV_TARGETDESC_H
