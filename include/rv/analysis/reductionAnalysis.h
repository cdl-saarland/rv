#ifndef RV_REDUCTIONANALYSIS_H
#define RV_REDUCTIONANALYSIS_H

#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/Constant.h>
#include <llvm/Transforms/Utils/ValueMapper.h>

#include "rv/vectorShape.h"

#include <map>

namespace rv {

// recognized kinds of reductions/recurrences
enum class RedKind : int32_t {
  Top = -1, // not a recognized reduction
  Bot = 0, // not yet analyzed
  Add = 1,
  Mul = 2,
  And = 3,
  Or = 4
}

static const char* to_string(RedKind red);

// get the neutral element for this reduction kind and data type
static Constant& getNeutralElement(RedKind redKind, Type & chainType);

// try to infer the reduction kind of the operator implemented by inst
static RedKind InferRedKind(Instruction & inst);

// materialize a single instance of firstArg [[RedKind~OpCode]] secondArg
static Instruction& CreateReduce(IRBuilder<> & builder, Value & firstArg, Value & secondArg);

// reduce the vector @vectorVal to a scalar value (using redKind)
static Value & CreateVectorReduce(IRBuilder<> & builder, RedKind redKind, Value & vectorVal);


struct Reduction {
  // all users outside of @levelLoop may reduce any of the instructions in @elements using a @kind reduction
  Loop * levelLoop;
  // the kind of reduction pattern
  RedKind kind;
  // the instructions that make up this reduction pattern
  std::set<Instruction*> elements;

  Reduction(Loop & _levelLoop, Instruction & _seedElem)
  : levelLoop(&_levelLoop)
  , kind(RedKind::Bot)
  , elements()
  {
    elements.insert(&_seedElem);
  }

  void dump() const;
  void print(llvm::raw_ostream & out) const;
};

// infer the shape of a reduction
static rv::VectorShape InferShape(Reduction & red);

class ReductionAnalysis {
  std::map<llvm::Instruction*, Reduction*> reductMap;

  const llvm::LoopInfo & loopInfo;

  void analyze(llvm::Loop & loop);
  Reduction* tryInferReduction(llvm::PHINode & headerPhi);

public:
  ReductionAnalysis(llvm::Function & _func, const llvm::LoopInfo & _loopInfo);
  ~ReductionAnalysis();

  void analyze();

  // create reduction for the clones as well
  void updateForClones(llvm::LoopInfo & LI, llvm::ValueToValueMapTy & cloneMap);

  // look up a reduction by its constituent
  Reduction * getReductionInfo(llvm::Instruction & inst) const;
};


} // namespace rv



#endif // RV_REDUCTIONANALYSIS_H
