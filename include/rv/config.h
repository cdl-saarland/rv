#ifndef RV_CONFIG_H
#define RV_CONFIG_H

#include <llvm/Support/raw_ostream.h>

namespace llvm {
  class Function;
}

namespace rv {


struct Config {
  Config();

  // Vectorization Analysis lattice
  enum VAMethod {
    VA_Full = 0, // {varying(a)} \_/ {(s,a)}
    VA_TopBot = 1, // {varying, uniform}
    VA_Karrenberg = 2, // {varying, uniform, consecutive} x alignment
    VA_Coutinho = 3, // {varying, strided} // no alignment
  };

// va configuration (divergence)
  // set to VA_Full for complete lattice
  VAMethod vaMethod;

  // should all (non-loop exiting) branches be folded regardless of VA result?
  // set to false for partial linearization
  bool foldAllBranches;

// native configuration (backend)
  bool scalarizeIndexComputation;
  bool useScatterGatherIntrinsics;
  bool enableMaskedMove;
  bool enableInterleaved;
  bool useSafeDivisors; // blend-in safe divisors to eliminate spurious arithmetic exceptions

// optimization flags
  bool enableSplitAllocas;
  bool enableStructOpt;
  bool enableSROV;
  bool enableIRPolish;
  bool enableHeuristicBOSCC;
  bool enableCoherentIF;

// greedy inter-procedural vectorizatoin
  bool enableGreedyIPV;

  // maximum ULP error bound for math functions
  // unit for maxULPErrorBound is tenth of ULP (a value of 10 implies that an ULP error of <= 1.0 is acceptable)
  int maxULPErrorBound;

// target features
  bool useVE;
  bool useSSE;
  bool useAVX;
  bool useAVX2;
  bool useAVX512;
  bool useNEON;
  bool useADVSIMD;


  void print(llvm::raw_ostream&) const;

  // create default configuration (RV_ARCH env var)
  static Config createDefaultConfig();

  // auto-detect target machine features (SIMD ISAs) for function \p F.
  static Config createForFunction(llvm::Function & F);
};

std::string to_string(Config::VAMethod vam);

}


#endif // RV_CONFIG_H
