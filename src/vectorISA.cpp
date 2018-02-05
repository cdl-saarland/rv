//===- OptConfig.cpp ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//

#include "rv/vectorISA.h"

#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Attributes.h>

#include "report.h"

using namespace llvm;

namespace rv {

VectorISA::VectorISA()
: hasSSE(false)
, hasAVX(false)
, hasAVX2(false)
, hasAVX512(false)
, hasNEON(false)
, hasADVSIMD(false)
{
  char * rawArch = getenv("RV_ARCH");
  if (!rawArch) return;

  std::string arch = rawArch;
  if (arch == "avx2") {
    Report() << "RV_ARCH: configured for avx2!\n";
    hasAVX2 = true;
    hasSSE = true;
  } else if (arch == "avx512") {
    Report() << "RV_ARCH: configured for avx512!\n";
    hasAVX512 = true;
    hasAVX2 = true;
    hasSSE = true;
  } else if (arch == "advsimd") {
    Report() << "RV_ARCH: configured for arm advsimd!\n";
    hasADVSIMD = true;
  }
}

void
VectorISA::print(llvm::raw_ostream & out) const {
  out << "VectorISA (SSE = " << hasSSE << ", AVX = " << hasAVX << ", AVX2 = " << hasAVX2 << ", AVX512 = " << hasAVX512 << ", NEON = " << hasNEON << ", AdvSIMD = " << hasADVSIMD << ")\n";
}
void VectorISA::dump() const { print(llvm::errs()); }

bool
VectorISA::hasNone() const {
  return (!hasSSE && !hasAVX &&! hasAVX2 && !hasAVX512 && !hasNEON && !hasADVSIMD);
}

VectorISA
VectorISA::infer(llvm::Module & mod) {
  for (auto & func : mod) {
    auto funcISA = infer(func);
    if (!funcISA.hasNone()) {
      return funcISA;
    }
  }

  return VectorISA();
}

VectorISA
VectorISA::infer(llvm::Function & func) {
  const char * featureFlag = "target-features";

  AttributeSet fnAttribs = func.getAttributes().getFnAttributes();
  if (!fnAttribs.hasAttribute(featureFlag)) return VectorISA();

  StringRef targetFeatures = fnAttribs.getAttribute(featureFlag).getValueAsString();

  VectorISA vectorIsa;
// x86 variety
  vectorIsa.hasAVX512 = targetFeatures.contains("+avx512f");
  vectorIsa.hasAVX2 = vectorIsa.hasAVX512 || targetFeatures.contains("+avx2");
  vectorIsa.hasAVX = vectorIsa.hasAVX2 || targetFeatures.contains("+avx");
  vectorIsa.hasSSE = vectorIsa.hasAVX || targetFeatures.contains("+sse2");

// ARM variety
  vectorIsa.hasADVSIMD = targetFeatures.contains("+neon");
  vectorIsa.hasNEON = vectorIsa.hasADVSIMD; // TODO

  return vectorIsa;
}

}
