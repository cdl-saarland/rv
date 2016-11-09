//===- vectorShape.h -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @author kloessner, simon
//

#include "rv/vectorizationInfo.h"

#include <llvm/IR/Instruction.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Function.h>
#include <llvm/Analysis/LoopInfo.h>

#include "rv/Region/Region.h"

using namespace llvm;

namespace rv {

void
VectorizationInfo::remapPredicate(Value & dest, Value & old) {
  for (auto it : predicates) {
    if (it.second == &old) {
      predicates[it.first] = &dest;
    }
  }
}


void
VectorizationInfo::dump() const {
       llvm::raw_ostream & out = llvm::errs();
	out << "VectorizationInfo ";

        if (region) {
          out << " for Region "; region->print(out); out << "\n";
        } else {
          out << " for function " << mapping.scalarFn->getName() << "\n";
        }

        out <<"CFG {\n";
	mapping.dump(out);

	for (const BasicBlock & block : *mapping.scalarFn) {
		const Value * predicate = getPredicate(block);

		out << "Block " << block.getName() << ", predicate ";
		if (predicate) out << *predicate; else out << "null";
		out << "\n";

		for (const Instruction & inst : block) {
			if (hasKnownShape(inst)) {
				out << inst << " : " << getVectorShape(inst).str() << "\n";
			} else {
				out << inst << " : unknown shape\n";
			}
		}
		out << "\n";
	}

	out << "}\n";
}

VectorizationInfo::VectorizationInfo(llvm::Function & parentFn, uint vectorWidth, Region & _region)
: mapping(&parentFn, &parentFn, vectorWidth)
, region(&_region)
{
  mapping.resultShape = VectorShape::uni();
  for (auto & arg : parentFn.getArgumentList()) {
    mapping.argShapes.push_back(VectorShape::uni());
  }
}

// VectorizationInfo
VectorizationInfo::VectorizationInfo(VectorMapping _mapping)
: mapping(_mapping)
, region(nullptr)
{
  auto & argList = mapping.scalarFn->getArgumentList();
  auto it = argList.begin();
  for (auto argShape : mapping.argShapes) {
    setVectorShape(*it, argShape);
    ++it;
  }
}

bool
VectorizationInfo::hasKnownShape(const llvm::Value & val) const {
  return (bool) shapes.count(&val);
}

VectorShape
VectorizationInfo::getVectorShape(const llvm::Value & val) const {
  auto it = shapes.find(&val);
  assert (it != shapes.end());
  return it->second;
}

void
VectorizationInfo::dropVectorShape(const Value & val) {
  auto it = shapes.find(&val);
  if (it == shapes.end()) return;
  shapes.erase(it);
}

void
VectorizationInfo::dropPredicate(const BasicBlock & block) {
  auto it = predicates.find(&block);
  if (it == predicates.end()) return;
  predicates.erase(it);
}

void
VectorizationInfo::setVectorShape(const llvm::Value & val, VectorShape shape) {
  shapes[&val] = shape;
}

llvm::Value *
VectorizationInfo::getPredicate(const llvm::BasicBlock & block) const {
	auto it = predicates.find(&block);
	if (it == predicates.end()) return nullptr;
	else return it->second;
}

void
VectorizationInfo::setPredicate(const llvm::BasicBlock & block, llvm::Value & predicate) {
	predicates[&block] = &predicate;
}

void
VectorizationInfo::setDivergenceLevel(const llvm::BasicBlock& block, const llvm::Loop* level)
{
	/* outs() << "Setting divergence level of block " + block.getName() + " as loop: ";
	if (level) level->dump(); else outs() << "top-level";
	outs() << "\n\n"; */

	auto loopit = loopAsDivergenceLevel.find(&block);

	if (loopit != loopAsDivergenceLevel.end() &&
		loopit->second->contains(level))
		return;

	loopAsDivergenceLevel[&block] = level;
}

bool
VectorizationInfo::isDivergent(const llvm::BasicBlock& block, const llvm::Loop* level)
const
{
	auto loopit = loopAsDivergenceLevel.find(&block);

	//not divergent at all
	if (loopit == loopAsDivergenceLevel.end())
		return false;

	//top-level-divergent
	if (!level)
		return &*loopit->second == nullptr;

	//divergent respective to level
	return level == loopit->second || loopit->second->contains(level);
}

bool
VectorizationInfo::isDivergentLoop(const llvm::Loop* loop)
const
{
	return isDivergent(*loop->getHeader(), loop);
}

bool
VectorizationInfo::isDivergentLoopTopLevel(const llvm::Loop* loop)
const
{
	Loop* parent = loop->getParentLoop();

	return isDivergentLoop(loop) &&
		   (!parent || !isDivergentLoop(parent));
}


void
VectorizationInfo::markAlwaysByAll(const llvm::BasicBlock* BB)
{
	ABABlocks.insert(BB);
}

void
VectorizationInfo::markAlwaysByAllOrNone(const llvm::BasicBlock* BB)
{
	ABAONBlocks.insert(BB);
}

void
VectorizationInfo::markNotAlwaysByAll(const llvm::BasicBlock* BB)
{
	NotABABlocks.insert(BB);
}

bool
VectorizationInfo::isAlwaysByAll(const llvm::BasicBlock* BB)
{
	return (bool)ABABlocks.count(BB);
}

bool
VectorizationInfo::isAlwaysByAllOrNone(const llvm::BasicBlock* BB)
{
	return (bool)ABAONBlocks.count(BB);
}

bool
VectorizationInfo::isNotAlwaysByAll(const llvm::BasicBlock* BB)
{
	return (bool)NotABABlocks.count(BB);
}

void
VectorizationInfo::markMandatory(const BasicBlock *BB)
{
	MandatoryBlocks.insert(BB);
}

bool
VectorizationInfo::isMandatory(const BasicBlock* BB)
{
	return (bool)MandatoryBlocks.count(BB);
}

} /* namespace rv */


