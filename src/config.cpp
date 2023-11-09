//===- src/config.cpp - vectorizer options --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "rv/config.h"
#include "report.h"

#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/Target/TargetMachine.h>

#include <cstdlib>
#include <sstream>
#include <map>
#include <functional>

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


Config::Config() :
// backend defaults
  scalarizeIndexComputation(true)
, useScatterGatherIntrinsics(true)
, enableMaskedMove(true)
, useSafeDivisors(true)

// optimization defaults
, enableIRPolish(CheckFlag("RV_ENABLE_POLISH"))

// enable greedy inter-procedural vectorization
, maxULPErrorBound(10)

// feature flags
, useSSE(false)
, useAVX(false)
, useAVX2(false)
, useAVX512(false)
, useADVSIMD(false)

// codegen flags
{
  const char *ULP = getenv("RV_ACCURACY");
  if (ULP) {
    int CustomBound = atoi(ULP);
    if (CustomBound > 0) maxULPErrorBound = CustomBound;
    else Report() << "ERROR: Expected an > 0 integer for RV_ACCURACY\n";
  }
}

Config
Config::createDefaultConfig() {
  rv::Config config;

  // override the RV target configuration
  char * rawArch = getenv("RV_ARCH");

  if (rawArch) {
    std::string arch = rawArch;
    if (arch == "avx2") {
      Report() << "RV_ARCH: configured for avx2!\n";
      config.useAVX2 = true;
      config.useSSE = true;
    } else if (arch == "avx512") {
      Report() << "RV_ARCH: configured for avx512!\n";
      config.useAVX512 = true;
      config.useAVX2 = true;
      config.useSSE = true;
    } else if (arch == "advsimd") {
      Report() << "RV_ARCH: configured for arm advsimd!\n";
      config.useADVSIMD = true;
    }
  }

  return config;
}

void
for_elems(StringRef listText, std::function<bool(StringRef)> UserFunc) {
  size_t NextPos;
  size_t Start = 0;

  if (listText.empty()) return;

  do {
    NextPos = listText.find(',', Start);
    size_t N = (NextPos == StringRef::npos) ? NextPos : NextPos - Start;
    auto elem = listText.substr(Start, N);
    bool CarryOn = UserFunc(elem);
    if (!CarryOn) return;

    Start = NextPos + 1;
  } while (NextPos != StringRef::npos);
}

Config
Config::createForFunction(Function & F) {
  Config config = createDefaultConfig();

  // maps a target-feature entry to a handler
  const std::map<std::string, std::function<void()>> handlerMap = {
      {"+sse2", [&config]() { config.useSSE = true; } },
      {"+avx", [&config]() { config.useAVX = true; } },
      {"+avx2", [&config]() { config.useAVX2 = true; } },
      {"+avx512f", [&config]() { config.useAVX512 = true; } },
      {"+neon", [&config]() { config.useADVSIMD = true; } }
  };

  auto attribSet = F.getAttributes().getFnAttrs();
  // parse SIMD signatures
  for (auto attrib : attribSet) {
    if (!attrib.isStringAttribute()) continue;
    StringRef attribText = attrib.getKindAsString();

    if (attribText.size() < 2) continue;

    if (attribText != "target-features") {
      continue;
    }

    // process all target-features
    for_elems(attrib.getValueAsString(), [&handlerMap](StringRef elem) {
      if (elem.size() == 0 || elem[0] != '+') return true;

      auto ItHandler = handlerMap.find(elem.str());
      if (ItHandler == handlerMap.end()) return true;
      ItHandler->second();
      return true;
    });

  }

  return config;
}

static void
printNativeFlags(const Config & config, llvm::raw_ostream & out) {
   out << "nat:  useScatterGather = " << config.useScatterGatherIntrinsics
       << ", useSafeDiv = " << config.useSafeDivisors;
}

static void
printOptFlags(const Config & config, llvm::raw_ostream & out) {
    out << "opts: enableIRPolish = " << config.enableIRPolish
        << ", maxULPErrorBound = " << ulp_to_string(config.maxULPErrorBound) << "\n";
}

static void
printFeatureFlags(const Config & config, llvm::raw_ostream & out) {
  out << "arch: useSSE = " << config.useSSE << ", useAVX = " << config.useAVX << ", useAVX2 = " << config.useAVX2 << ", useAVX512 = " << config.useAVX512 << ", useADVSIMD = " << config.useADVSIMD << "\n";
}


void
Config::print(llvm::raw_ostream & out) const {
  out << "RVConfig {\n\t";
  printOptFlags(*this, out);
  out << "\n\t";
  printNativeFlags(*this, out);
  out << "\n\t";
  printFeatureFlags(*this, out);
  out << "\n}\n";
}



} // namespace rv
