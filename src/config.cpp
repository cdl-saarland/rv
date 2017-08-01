#include "rv/config.h"
#include "report.h"

using namespace rv;

Config::Config()
: vaMethod(VA_Full)

  // should all (non-loop exiting) branches be folded regardless of VA result?
  // set to false for partial linearization
, foldAllBranches(false)

// backend defaults
, scalarizeIndexComputation(true)
, useScatterGatherIntrinsics(true)
, enableMaskedMove(true)
, enableInterleaved(false)
, enablePseudoInterleaved(false)
, cropPseudoInterleaved(false)
, useSafeDivisors(true)

// optimization defaults
, enableStructOpt(!CheckFlag("RV_DISABLE_STRUCTOPT"))
, enableSROV(!CheckFlag("RV_DISABLE_SROV"))
, enableIRPolish(!CheckFlag("RV_DISABLE_POLISH"))
, enableHeuristicBOSCC(CheckFlag("RV_EXP_BOSCC"))

// feature flags
, useSSE(false)
, useAVX(false)
, useAVX2(false)
, useNEON(false)
, useSLEEF(false)
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

static void
printVAFlags(const Config & config, llvm::raw_ostream & out) {
    out << "VA:   " << to_string(config.vaMethod) << ", foldAllBranches = " << config.foldAllBranches;
}

static void
printNativeFlags(const Config & config, llvm::raw_ostream & out) {
   out << "nat:  useScatterGather = " << config.useScatterGatherIntrinsics
       << ", enableInterleaved = " << config.enableInterleaved
       << ", enablePseudoIL = " << config.enablePseudoInterleaved
       << ", cropPseudoIL = " << config.cropPseudoInterleaved
       << ", useSafeDiv = " << config.useSafeDivisors;
}

static void
printOptFlags(const Config & config, llvm::raw_ostream & out) {
    out << "opts: enableStructOpt = " << config.enableStructOpt
        << ", enableSROV = " << config.enableSROV
        << ", enableHeuristicBOSCC = " << config.enableHeuristicBOSCC
        << ", enableIRPolish = " << config.enableIRPolish;
}

static void
printFeatureFlags(const Config & config, llvm::raw_ostream & out) {
  out << "arch: useSSE = " << config.useSSE << ", useAVX = " << config.useAVX << ", useAVX2 = " << config.useAVX2 << ", useNEON = " << config.useNEON << "\n";
}


void
Config::print(llvm::raw_ostream & out) const {
  out << "RVConfig {\n\t";
  printVAFlags(*this, out);
  out << "\n\t";
  printOptFlags(*this, out);
  out << "\n\t";
  printNativeFlags(*this, out);
  out << "\n\t";
  printFeatureFlags(*this, out);
  out << "\n}\n";
}
