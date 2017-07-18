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

// reduction-kind lattice
enum class RedKind : int32_t {
  Top = -1, // not a recognized reduction
  Bot = 0, // not yet analyzed
  Add = 1,
  Mul = 2,
  And = 3,
  Or = 4
};

// join operator
RedKind JoinKinds(RedKind A, RedKind B);

const char* to_string(RedKind red);

// get the neutral element for this reduction kind and data type
llvm::Constant& GetNeutralElement(RedKind redKind, llvm::Type & chainType);

// try to infer the reduction kind of the operator implemented by inst (this ignores the operands of instruction)
RedKind InferRedKind(llvm::Instruction & inst);

struct Reduction; // forward
struct StridePattern {
  const Reduction * red;

  int loopInitIdx;
  int latchIdx;
  llvm::PHINode * phi;

  llvm::Instruction * reductor;
  int64_t inc; // increment value (already accounts to sign change by Instruction::Sub)

  VectorShape getShape(int vectorWidth) {
    return VectorShape::strided(inc, vectorWidth * inc); // TODO fix alignment
  }
};


struct Reduction {
  // all users outside of @levelLoop may reduce any of the instructions in @elements using a @kind reduction
  llvm::Loop * levelLoop;
  // the kind of reduction pattern
  RedKind kind;
  // the instructions that make up this reduction pattern
  std::set<llvm::Instruction*> elements;

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

  // checks whether this value dependence chain can be privatized
  // - all reductors receive other elements of this reduction on one input only
  // - only reducible reduction elements are being used (atm the header phi latch input)
  // - all uses of reduction elements are either elements themselves or live outside of @levelLoop
  bool canPrivatize() const { false; } // TODO implement
  VectorShape getShape(int vectorWidth) const; // infer a suitable vector shape
  bool matchStridedPattern(StridePattern & pat) const;

  // shorthands
  bool contains(llvm::Instruction & elem) const { return elements.find(&elem) != elements.end(); }
  bool add(llvm::Instruction & elem) { return elements.insert(&elem).second; }

  void dump() const;
  void print(llvm::raw_ostream & out) const;
};

// infer the shape of a reduction
rv::VectorShape InferShape(Reduction & red);

class ReductionAnalysis {
  std::map<llvm::Instruction*, Reduction*> reductMap;

  const llvm::LoopInfo & loopInfo;

  // adds the unseen instruction @inst to @redGroup in all mappings
  bool addToGroup(Reduction & redGroup, llvm::Instruction & inst);

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
