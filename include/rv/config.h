#ifndef RV_CONFIG_H
#define RV_CONFIG_H

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
  VAMethod vaMethod = VA_Full;

  // should all (non-loop exiting) branches be folded regardless of VA result?
  // set to false for partial linearization
  bool foldAllBranches = false;

// native configuration (backend)
  bool useScatterGatherIntrinsics = true;
  bool enableInterleaved = true;
  bool enablePseudoInterleaved = true;
  bool cropPseudoInterleaved = false;
};

}


#endif // RV_CONFIG_H
