#ifndef RV_ANALYSIS_REDUCTIONS_H
#define RV_ANALYSIS_REDUCTIONS_H

#include <stdint.h>
#include "llvm/ADT/StringRef.h"

namespace llvm {
  class Constant;
  class Instruction;
  class Type;
}

namespace rv {

enum class RedKind : int64_t {
  Enum_Begin = -1,
  Top = -1, // not a recognized reduction
  Bot = 0, // not yet analyzed or recurrence
  Add = 1,
  Mul = 2,
  And = 3,
  Or = 4,
  Max = 5,
  Min = 6,

  Enum_End = 7
};

// join operator
RedKind JoinKinds(RedKind A, RedKind B);

llvm::StringRef to_string(RedKind red);
bool from_string(llvm::StringRef redKindText, RedKind & oRedKind);

// get the neutral element for this reduction kind and data type
llvm::Constant& GetNeutralElement(RedKind redKind, llvm::Type & chainType);

}

#endif // RV_ANALYSIS_REDUCTIONS_H
