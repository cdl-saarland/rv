//===- vectorizationInfo.h -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//

#ifndef INCLUDE_RV_VECTORIZATIONINFO_H_
#define INCLUDE_RV_VECTORIZATIONINFO_H_

namespace llvm {
	class BasicBlock;
	class Instruction;
	class Value;
    class Loop;
}

using namespace llvm;

#include "vectorShape.h"
#include "vectorMapping.h"
#include <map>
#include <set>


namespace rv {

class Region;

// provides vectorization information (vector shapes, block predicates) for a function
class VectorizationInfo {
  VectorMapping mapping;
  std::map<const BasicBlock*, Value*> 	predicates;
  std::map<const Value*, VectorShape> 	shapes;
  std::map<const BasicBlock*, const Loop*> 	loopAsDivergenceLevel;

  std::set<const BasicBlock*> ABABlocks;
  std::set<const BasicBlock*> ABAONBlocks;
  std::set<const BasicBlock*> NotABABlocks;

  std::set<const BasicBlock*> MandatoryBlocks;

  Region * region;

public:
  Region * getRegion() const { return region; }
  const VectorMapping & getMapping() const { return mapping; }
  uint getVectorWidth() const { return mapping.vectorWidth; }

  VectorizationInfo(VectorMapping _mapping);
  VectorizationInfo(llvm::Function & parentFn, uint vectorWidth, Region & _region);

  bool hasKnownShape(const Value& val) const;
  VectorShape getVectorShape(const Value & val) const;
  void setVectorShape(const Value & val, VectorShape shape);
  void dropVectorShape(const Value & val);

  // return the predicate value for this instruction
  Value* getPredicate(const BasicBlock & block) const;
  void setPredicate(const BasicBlock & block, Value & predicate);
  void dropPredicate(const BasicBlock & block);
  void remapPredicate(Value & dest, Value & old);

  void dump() const;

  void setDivergenceLevel(const BasicBlock& block, const Loop* level);
  bool isDivergent(const BasicBlock& block, const Loop* level = nullptr) const;
  bool isDivergentLoop(const Loop* loop) const;
  bool isDivergentLoopTopLevel(const Loop* loop) const;

  void markAlwaysByAll(const BasicBlock* block);
  void markAlwaysByAllOrNone(const BasicBlock* block);
  void markNotAlwaysByAll(const BasicBlock* block);

  bool isAlwaysByAll(const BasicBlock* block);
  bool isAlwaysByAllOrNone(const BasicBlock* block);
  bool isNotAlwaysByAll(const BasicBlock* block);

  void markMandatory(const BasicBlock* block);
  bool isMandatory(const BasicBlock* block);
};


}

#endif /* INCLUDE_RV_VECTORIZATIONINFO_H_ */
