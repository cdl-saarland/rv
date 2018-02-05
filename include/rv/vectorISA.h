#ifndef RV_VECTORISA_H
#define RV_VECTORISA_H

namespace llvm {
  class raw_ostream;
  class Module;
  class Function;
}

namespace rv {


// available Vector ISA extensions for this target
struct
VectorISA {
  bool hasSSE;
  bool hasAVX;
  bool hasAVX2;
  bool hasAVX512;
  bool hasNEON;
  bool hasADVSIMD;

  bool hasNone() const;

  // no SIMD capabilities
  VectorISA();

  // infer available ISAs from module
  static VectorISA infer(llvm::Module & mod);

  // infer available ISAs from module
  static VectorISA infer(llvm::Function & func);

  void print(llvm::raw_ostream & out) const;
  void dump() const;
};


}

#endif // RV_VECTORISA_H
