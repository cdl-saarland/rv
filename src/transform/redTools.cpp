#include "rv/transform/redTools.h"

using namespace llvm;

namespace rv {

// materialize a single instance of firstArg [[RedKind~OpCode]] secondArg
Instruction&
CreateReduce(IRBuilder<> & builder, Value & firstArg, Value & secondArg) {
  abort();
}

// reduce the vector @vectorVal to a scalar value (using redKind)
Value &
CreateVectorReduce(IRBuilder<> & builder, RedKind redKind, Value & vectorVal) {
  abort();
}


}
