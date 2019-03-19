#include "rv/config.h"
#include "report.h"

#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/Target/TargetMachine.h>

#include <sstream>

using namespace llvm;

namespace {

inline
std::string
ulp_to_string(int ulp) {
  std::stringstream ss;
  ss << (ulp / 10) << '.' << (ulp % 10);
  return ss.str();
}

}


namespace rv {


Config::Config()
: vaMethod(VA_Full)

  // should all (non-loop exiting) branches be folded regardless of VA result?
  // set to false for partial linearization
, foldAllBranches(CheckFlag("RV_FOLD_BRANCHES"))

// backend defaults
, scalarizeIndexComputation(true)
, useScatterGatherIntrinsics(true)
, enableMaskedMove(true)
, enableInterleaved(false)
, enablePseudoInterleaved(false)
, cropPseudoInterleaved(false)
, useSafeDivisors(true)

// optimization defaults
, enableSplitAllocas(!CheckFlag("RV_DISABLE_SPLITALLOCAS"))
, enableStructOpt(!CheckFlag("RV_DISABLE_STRUCTOPT"))
, enableSROV(!CheckFlag("RV_DISABLE_SROV"))
, enableIRPolish(CheckFlag("RV_ENABLE_POLISH"))
, enableHeuristicBOSCC(CheckFlag("RV_EXP_BOSCC"))

// enable greedy inter-procedural vectorization
, enableGreedyIPV(CheckFlag("RV_IPV"))
#ifdef LLVM_HAVE_VP
, enableVP(CheckFlag("RV_USE_VP"))
#else
, enableVP(false)
#endif
, desc()

, maxULPErrorBound(10)
{}

std::string
to_string(Config::VAMethod vam) {
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
    out << "opts: enableSplitAllocas = " << config.enableSplitAllocas
        << ", enableStructOpt = " << config.enableStructOpt
        << ", enableSROV = " << config.enableSROV
        << ", enableHeuristicBOSCC = " << config.enableHeuristicBOSCC
        << ", enableIRPolish = " << config.enableIRPolish
        << ", greedyIPV = " << config.enableGreedyIPV
        << ", maxULPErrorBound = " << ulp_to_string(config.maxULPErrorBound);
}


void
Config::print(llvm::raw_ostream & out) const {
  out << "RVConfig {\n\t";
  printVAFlags(*this, out);
  out << "\n\t";
  printOptFlags(*this, out);
  out << "\n\t";
  printNativeFlags(*this, out);
#ifdef LLVM_HAVE_VP
  out << "\n\t";
  out << "LLVM-VP build.\n";
  if (enableVP) out << "nat: using LLVM-VP intrinsics\n";
#endif
  desc.print(out);
  out << "}\n";
}


Config
Config::createDefaultConfig() { Config config; config.desc = TargetDesc::createDefaultConfig(); return config; }

  // auto-detect target machine features (SIMD ISAs) for function \p F.
Config
Config::createForFunction(llvm::Function & F) { Config config; config.desc = TargetDesc::createForFunction(F); return config; }
} // namespace rv
