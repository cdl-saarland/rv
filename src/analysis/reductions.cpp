#include "rv/analysis/reductions.h"

#include "llvm/IR/Instructions.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Constants.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/StringRef.h"

#include <string.h>

using namespace llvm;

#ifdef RV_DEBUG
#define IF_DEBUG_RED if (true)
#else
#define IF_DEBUG_RED if (false)
#endif


namespace rv {

RedKind JoinKinds(RedKind A, RedKind B) {
  // default bottom rules
  if (A == RedKind::Bot) return B;
  if (B == RedKind::Bot) return A;

  // fallback rule
  if (A != B) return RedKind::Top;
  return A; // A == B
}


StringRef
to_string(RedKind red) {
  switch (red) {
    case RedKind::Bot: return "Bot";
    case RedKind::Top: return "Top";
    case RedKind::Add: return "Add";
    case RedKind::Mul: return "Mul";
    case RedKind::And: return "And";
    case RedKind::Or: return "Or";
    case RedKind::Max: return "Max";
    case RedKind::Min: return "Min";
    default:
      abort();
  }
}

bool
from_string(StringRef redKindText, RedKind & oRedKind) {
  for (int64_t itRed = (int64_t) RedKind::Enum_Begin;
      itRed < (int64_t) RedKind::Enum_End;
      ++itRed) {
    RedKind kind = (RedKind) itRed;
    if (redKindText == to_string(kind)) {
        oRedKind = kind;
        return true;
    }
  }
  return false;
}


Constant&
GetNeutralElement(RedKind redKind, Type & chainTy) {
  switch(redKind) {
    case RedKind::Or:
      assert(chainTy.isIntegerTy());
      return *ConstantInt::getNullValue(&chainTy);
    case RedKind::And:
      assert(chainTy.isIntegerTy());
      return *ConstantInt::getAllOnesValue(&chainTy);

    case RedKind::Add:
      return *(chainTy.isFloatTy() ? ConstantFP::get(&chainTy, 0.0) : ConstantInt::getNullValue(&chainTy));

    case RedKind::Mul:
      return *(chainTy.isFloatTy() ? ConstantFP::get(&chainTy, 1.0) : ConstantInt::get(&chainTy, 1, false));

    default:
      IF_DEBUG_RED { errs() << "red: Unknown neutral element for " << to_string(redKind) << "\n"; }
      abort();
  }
}

}
