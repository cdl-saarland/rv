#include "rv/config.h"
#include "report.h"

using namespace rv;

Config::Config()
: vaMethod(VA_Full)

  // should all (non-loop exiting) branches be folded regardless of VA result?
  // set to false for partial linearization
, foldAllBranches(false)

// backend defaults
, useScatterGatherIntrinsics(true)
, enableInterleaved(true)
, enablePseudoInterleaved(true)
, cropPseudoInterleaved(false)

// optimization defaults
, enableStructOpt(!CheckFlag("RV_DISABLE_STRUCTOPT"))
, enableSROV(!CheckFlag("RV_DISABLE_SROV"))
, enableIRPolish(!CheckFlag("RV_DISABLE_POLISH"))
{}

std::string
rv::to_string(Config::VAMethod vam) {
  switch(vam) {
    case Config::VA_Full: return "sa-lattice";
    case Config::VA_TopBot: return "topbot-lattice";
    case Config::VA_Karrenberg: return "karrenberg-lattice";
    case Config::VA_Coutinho: return "coutinho-lattice";
    default:
        abort(); // invalid lattice argument
  }
}

void
Config::print(llvm::raw_ostream & out) const {
  out
    << "RVConfig {\n"
    << "\tvaMethod = " << to_string(vaMethod) << ",\n"
    << "\tfoldAllBranches = " << foldAllBranches  << ",\n"
    << "\tuseScatterGather = " << useScatterGatherIntrinsics  << ",\n"
    << "\tenableInterleaved = " << enableInterleaved  << ",\n"
    << "\tenablePseudoIL = " << enablePseudoInterleaved  << ",\n"
    << "\tcropPseudoIL = " << cropPseudoInterleaved  << ",\n"
    << "\tenableStructOpt = " << enableStructOpt  << ",\n"
    << "\tenableSROV = " << enableSROV  << ",\n"
    << "\tenableIRPolish = " << enableIRPolish << ",\n"
  << "}\n";
}
