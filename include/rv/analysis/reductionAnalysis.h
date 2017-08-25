#ifndef RV_REDUCTIONANALYSIS_H
#define RV_REDUCTIONANALYSIS_H

#include <llvm/IR/Function.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/Constant.h>
#include <llvm/Transforms/Utils/ValueMapper.h>

#include "rv/vectorShape.h"

#include <set>
#include <map>

namespace llvm {
  class Constant;
  class Loop;
}

namespace rv {

using InstSet = std::set<llvm::Instruction*>;


// reduction-kind lattice
enum class RedKind : int32_t {
  Top = -1, // not a recognized reduction
  Bot = 0, // not yet analyzed
  Add = 1,
  Mul = 2,
  And = 3,
  Or = 4,
  Max = 5,
  Min = 6
};

// join operator
RedKind JoinKinds(RedKind A, RedKind B);

const char* to_string(RedKind red);

// get the neutral element for this reduction kind and data type
llvm::Constant& GetNeutralElement(RedKind redKind, llvm::Type & chainType);

// try to infer the reduction kind of the operator implemented by inst (this ignores the operands of instruction)
RedKind InferRedKind(llvm::Instruction & inst);

// stride recurrence
struct StridePattern {
  int loopInitIdx;
  int latchIdx;
  llvm::PHINode * phi;

  llvm::Instruction * reductor;
  int64_t inc; // increment value (already accounts to sign change by Instruction::Sub)

  VectorShape getShape(int vectorWidth) const {
    return VectorShape::strided(inc, vectorWidth * inc); // TODO fix alignment
  }

  void print(llvm::raw_ostream & out) const;
  void dump() const;
};

// general reduction pattern
struct Reduction {
  // all users outside of @levelLoop may reduce any of the instructions in @elements using a @kind reduction
  llvm::Loop * levelLoop;
  // the kind of reduction pattern
  RedKind kind;
  // the instructions that make up this reduction pattern
  InstSet elements;

  Reduction(InstSet _elements)
  : levelLoop(nullptr)
  , kind(RedKind::Bot)
  , elements(_elements)
  {}

  Reduction(llvm::Loop & _levelLoop, RedKind _kind)
  : levelLoop(&_levelLoop)
  , kind(_kind)
  {}


  Reduction(llvm::Loop & _levelLoop, llvm::Instruction & _seedElem)
  : levelLoop(&_levelLoop)
  , kind(RedKind::Bot)
  , elements()
  {
    elements.insert(&_seedElem);
  }

#if 0
  // TODO move somewhere else
  // checks whether this value dependence chain can be privatized
  // - all reductors receive other elements of this reduction on one input only
  // - only reducible reduction elements are being used (atm the header phi latch input)
  // - all uses of reduction elements are either elements themselves or live outside of @levelLoop
  bool canPrivatize() const { false; } // TODO implement
#endif

  VectorShape getShape(int vectorWidth) const { return kind == RedKind::Bot ? VectorShape::undef() : VectorShape::varying(); } // infer a suitable vector shape

  // shorthands
  bool contains(llvm::Instruction & elem) const { return elements.find(&elem) != elements.end(); }
  bool add(llvm::Instruction & elem) { return elements.insert(&elem).second; }
  void erase(llvm::Instruction & elem) { elements.erase(&elem); }

  void dump() const;
  void print(llvm::raw_ostream & out) const;
};

// infer the shape of a reduction
rv::VectorShape InferShape(Reduction & red);

class ReductionAnalysis {
  std::map<llvm::Instruction*, StridePattern*> stridePatternMap;
  std::map<llvm::Instruction*, Reduction*> reductMap;

  const llvm::LoopInfo & loopInfo;

  // adds the unseen instruction @inst to @redGroup in all mappings
  bool addToGroup(Reduction & redGroup, llvm::Instruction & inst);
  bool changeGroup(Reduction & redGroup, llvm::Instruction & inst);

  // this moves all elements of @srcGropu into @destGroup and replaces all instructions mappings
  // srcGroup is disconnected from all maps after this operation and can be deleted
  void foldIntoGroup(Reduction & destGroup, Reduction & srcGroup);

  bool isHeaderPhi(llvm::Instruction & inst, llvm::Loop & loop) const;

  // check whether this instruction has a general stride pattern
  StridePattern * tryMatchStridePattern(llvm::PHINode & headerPhi);


  // returns true if the value of this instruction can be recomputed even if loop iterations execute in parallel/or SIMD fashing
  bool canReconstructInductively(llvm::Instruction & inst) const { return getStrideInfo(inst); }

  // reset internal state
  void clear();
public:
  ReductionAnalysis(llvm::Function & _func, const llvm::LoopInfo & _loopInfo);
  ~ReductionAnalysis();

  // analyze all recurrence patterns inside @hostLoop
  void analyze(llvm::Loop & hostLoop);

  // look up a (general) reduction by its constituent
  Reduction * getReductionInfo(llvm::Instruction & inst) const;

  // look up the specific stride pattern for inst
  StridePattern * getStrideInfo(llvm::Instruction & inst) const;

  // print reduction result
  void print(llvm::raw_ostream & out) const;
  void dump() const;
};


} // namespace rv



#endif // RV_REDUCTIONANALYSIS_H
