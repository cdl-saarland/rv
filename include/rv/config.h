#ifndef RV_CONFIG_H
#define RV_CONFIG_H

#include <llvm/Support/raw_ostream.h>

namespace rv {


struct Config {
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
  bool enablePseudoInterleaved;
  bool cropPseudoInterleaved;
  bool useSafeDivisors; // blend-in safe divisors to eliminate spurious arithmetic exceptions

// optimization flags
  bool enableStructOpt;
  bool enableSROV;
  bool enableIRPolish;
  bool enableHeuristicBOSCC;

// target features
  bool useSSE;
  bool useAVX;
  bool useAVX2;
  bool useAVX512;
  bool useNEON;
  bool useADVSIMD;
  bool useSLEEF;

  // initialize defaults
  Config();

  void print(llvm::raw_ostream&) const;

};

std::string to_string(Config::VAMethod vam);

}


#endif // RV_CONFIG_H
