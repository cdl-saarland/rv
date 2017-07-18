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

// reduction-kind lattice
enum class RedKind : int32_t {
  Top = -1, // not a recognized reduction
  Bot = 0, // not yet analyzed
  Add = 1,
  Mul = 2,
  And = 3,
  Or = 4
}

// join operator
RedKind JoinKinds(RedKind A, RedKind B);

const char* to_string(RedKind red);

// get the neutral element for this reduction kind and data type
Constant& GetNeutralElement(RedKind redKind, Type & chainType);

// try to infer the reduction kind of the operator implemented by inst (this ignores the operands of instruction)
RedKind InferRedKind(Instruction & inst);

// materialize a single instance of firstArg [[RedKind~OpCode]] secondArg
Instruction& CreateReduce(IRBuilder<> & builder, Value & firstArg, Value & secondArg);

// reduce the vector @vectorVal to a scalar value (using redKind)
Value & CreateVectorReduce(IRBuilder<> & builder, RedKind redKind, Value & vectorVal);



struct Reduction {
  // all users outside of @levelLoop may reduce any of the instructions in @elements using a @kind reduction
  Loop * levelLoop;
  // the kind of reduction pattern
  RedKind kind;
  // the instructions that make up this reduction pattern
  std::set<Instruction*> elements;

  Reduction(Loop & _levelLoop, RedKind _kind)
  : levelLoop(&_levelLoop)
  , kind(_kind)
  {}


  Reduction(Loop & _levelLoop, Instruction & _seedElem)
  : levelLoop(&_levelLoop)
  , kind(RedKind::Bot)
  , elements()
  {
    elements.insert(&_seedElem);
  }

  // checks whether this value dependence chain can be privatized
  // - all reductors receive other elements of this reduction on one input only
  // - only reducible reduction elements are being used (atm the header phi latch input)
  // - all uses of reduction elements are either elements themselves or live outside of @levelLoop
  bool canPrivatize() const { false; } // TODO implement
  VectorShape getShape(int vectorWidth); // infer a suitable vector shape

  // shorthands
  bool contains(const Instruction & elem) const { return elements.count(&elem); }
  bool add(Instruction & elem) { return elements.insert(&elem).second; }

  void dump() const;
  void print(llvm::raw_ostream & out) const;
};

// infer the shape of a reduction
static rv::VectorShape InferShape(Reduction & red);

class ReductionAnalysis {
  std::map<llvm::Instruction*, Reduction*> reductMap;

  const llvm::LoopInfo & loopInfo;

  // void analyze(llvm::Loop & loop);
  // Reduction* tryInferReduction(llvm::PHINode & headerPhi);

  // adds the unseen instruction @inst to @redGroup in all mappings
  bool addToGroup(Reduction & redGroup, Instruction & inst);

  // this moves all elements of @srcGropu into @destGroup and replaces all instructions mappings
  // srcGroup is disconnected from all maps after this operation and can be deleted
  void foldIntoGroup(Reduction & destGroup, Reduction & srcGroup);

  bool isHeaderPhi(llvm::Instruction & inst, llvm::Loop & loop) const;
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
