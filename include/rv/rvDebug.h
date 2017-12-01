#ifndef RV_RVDEBUG_H
#define RV_RVDEBUG_H

namespace llvm {
  class Value;
  class Module;
}

namespace rv {
  void Dump(const llvm::Value & val);
  void Dump(const llvm::Module & mod);
}
#endif
