#include "rv/TargetDesc.h"

#include <llvm/IR/Module.h>
#include <llvm/IR/Function.h>
#include <llvm/Target/TargetMachine.h>

#include "report.h"

#include <sstream>


using namespace llvm;

namespace rv {

// feature flags
TargetDesc::TargetDesc()
: useVE(false)
, useSSE(false)
, useAVX(false)
, useAVX2(false)
, useAVX512(false)
, useNEON(false)
, useADVSIMD(false)
{}

TargetDesc
TargetDesc::createDefaultConfig() {
  rv::TargetDesc desc;

  char * rawArch = getenv("RV_ARCH");
  if (!rawArch) return desc;

  std::string arch = rawArch;
  if (arch == "avx2") {
    Report() << "RV_ARCH: configured for avx2!\n";
    desc.useAVX2 = true;
    desc.useSSE = true;
  } else if (arch == "avx512") {
    Report() << "RV_ARCH: configured for avx512!\n";
    desc.useAVX512 = true;
    desc.useAVX2 = true;
    desc.useSSE = true;
  } else if (arch == "advsimd") {
    Report() << "RV_ARCH: configured for arm advsimd!\n";
    desc.useADVSIMD = true;
  } else if (arch == "ve") {
    Report() << "RV_ARCH: configured for NEC SX-Aurora!\n";
    desc.useVE = true;
  }

  return desc;
}

bool
TargetDesc::hasActiveVectorLength() const { return useVE; }

void
for_elems(StringRef listText, std::function<bool(StringRef elem)> UserFunc) {
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

TargetDesc
TargetDesc::createForFunction(Function & F) {
  TargetDesc desc;

  std::string triple = F.getParent()->getTargetTriple();
  if (StringRef(triple).startswith("ve-")) {
    desc.useVE = true;
    return desc;
  }


  // maps a target-feature entry to a handler
  const std::map<std::string, std::function<void()>> handlerMap = {
      {"+sse2", [&desc]() { desc.useSSE = true; } },
      {"+avx", [&desc]() { desc.useAVX = true; } },
      {"+avx2", [&desc]() { desc.useAVX2 = true; } },
      {"+avx512f", [&desc]() { desc.useAVX512 = true; } },
      {"+neon", [&desc]() { desc.useADVSIMD = true; desc.useNEON = true; } }
  };

  auto attribSet = F.getAttributes().getFnAttributes();
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

  return desc;
}

void
TargetDesc::print(raw_ostream & out) const {
  out << "TargetDesc: useSSE = " << useSSE << ", useAVX = " << useAVX << ", useAVX2 = " << useAVX2 << ", useAVX512 = " << useAVX512 << ", useNEON = " << useNEON << ", useADVSIMD = " << useADVSIMD << ", useVE = " << useVE << "\n";
}

} // namespace rv
