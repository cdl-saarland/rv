//===- OptConfig.cpp ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//

#include "rv/optConfig.h"

#include <llvm/Support/raw_ostream.h>

#include "report.h"

namespace rv {

OptConfig::OptConfig()
// backend defaults
: scalarizeIndexComputation(true)
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

// enable SLEEF library sypport
, useSLEEF(false)
{}

static void
printNativeFlags(const OptConfig & config, llvm::raw_ostream & out) {
   out << "nat:  useScatterGather = " << config.useScatterGatherIntrinsics
       << ", enableInterleaved = " << config.enableInterleaved
       << ", enablePseudoIL = " << config.enablePseudoInterleaved
       << ", cropPseudoIL = " << config.cropPseudoInterleaved
       << ", useSafeDiv = " << config.useSafeDivisors;
}

static void
printOptFlags(const OptConfig & config, llvm::raw_ostream & out) {
    out << "opts: enableSplitAllocas = " << config.enableSplitAllocas
        << ", enableStructOpt = " << config.enableStructOpt
        << ", enableSROV = " << config.enableSROV
        << ", enableHeuristicBOSCC = " << config.enableHeuristicBOSCC
        << ", enableIRPolish = " << config.enableIRPolish;
}



void
OptConfig::print(llvm::raw_ostream & out) const {
  out << "RVConfig {\n\t";
  printOptFlags(*this, out);
  out << "\n\t";
  printNativeFlags(*this, out);
  out << "\n}\n";
}

void OptConfig::dump() const { print(llvm::errs()); }

}
