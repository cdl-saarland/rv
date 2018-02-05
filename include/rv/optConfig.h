#ifndef RV_CONFIG_H
#define RV_CONFIG_H

namespace llvm { class raw_ostream; }

namespace rv {


struct OptConfig {
// native configuration (backend)
  bool scalarizeIndexComputation;
  bool useScatterGatherIntrinsics;
  bool enableMaskedMove;
  bool enableInterleaved;
  bool enablePseudoInterleaved;
  bool cropPseudoInterleaved;
  bool useSafeDivisors; // blend-in safe divisors to eliminate spurious arithmetic exceptions

// optimization flags
  bool enableSplitAllocas;
  bool enableStructOpt;
  bool enableSROV;
  bool enableIRPolish;
  bool enableHeuristicBOSCC;

// target features
  bool useSLEEF;

  // initialize defaults
  OptConfig();

  void print(llvm::raw_ostream&) const;
  void dump() const;

};

}


#endif // RV_CONFIG_H
