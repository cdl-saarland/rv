//===----------------------- vectorizationInfo.cpp --------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//

#include "rv/vectorizationInfo.h"

#include <llvm/IR/Instruction.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Function.h>
#include <llvm/Analysis/LoopInfo.h>

#include "rv/region/Region.h"
#include "rvConfig.h"

using namespace llvm;

namespace rv
{

bool
VectorizationInfo::inRegion(const BasicBlock & block) const {
  return region.contains(&block);
}

bool
VectorizationInfo::inRegion(const Instruction & inst) const {
  return region.contains(inst.getParent());
}

void
VectorizationInfo::remapPredicate(Value& dest, Value& old)
{
    for (auto it : predicates)
    {
        if (it.second == &old)
        {
            predicates[it.first] = &dest;
        }
    }
}

void
VectorizationInfo::dump(const Value * val) const {
  print(val, errs());
}

void
VectorizationInfo::print(const Value * val, llvm::raw_ostream & out) const {
  if (!val) return;

  auto * block = dyn_cast<const BasicBlock>(val);
  if (block && inRegion(*block)) {
    printBlockInfo(*block, out);
  }

  if (hasKnownShape(*val)) {
  	out << *val << " : " << getVectorShape(*val).str() << "\n";
  } else {
  	out << *val << " : unknown shape\n";
  }
}

void
VectorizationInfo::dumpBlockInfo(const BasicBlock & block) const {
  printBlockInfo(block, errs());
}

void
VectorizationInfo::printBlockInfo(const BasicBlock & block, llvm::raw_ostream & out) const {
  const Value * predicate = getPredicate(block);

  out << "Block ";
  block.printAsOperand(out, false);
  if (predicate) { out << ", predicate " << *predicate; }
  if (isDivergentLoopExit(block)) { out << ", divLoopExit"; }
  out << "\n";

  for (const Instruction & inst : block) {
    print(&inst, out);
  }
  out << "\n";
}

void VectorizationInfo::dumpArguments() const {
  printArguments(errs());
}

void VectorizationInfo::printArguments(llvm::raw_ostream & out) const {
  const Function* F = mapping.scalarFn;

  out << "\nArguments:\n";

  for (auto& arg : F->args()) {
    out << arg << " : " << (hasKnownShape(arg) ? getVectorShape(arg).str() : "missing") << "\n";
  }

  out << "\n";
}

void
VectorizationInfo::dump() const {
  print(errs());
}

void
VectorizationInfo::print(llvm::raw_ostream & out) const
{
    out << "VectorizationInfo ";

    out << "for " << region.str() << "\n";

    printArguments(out);

    for (const BasicBlock & block : *mapping.scalarFn) {
      if (!inRegion(block)) continue;
      printBlockInfo(block, out);
    }

    out << "}\n";
}

VectorizationInfo::VectorizationInfo(llvm::Function& parentFn, unsigned vectorWidth, Region& _region)
: DL(parentFn.getParent()->getDataLayout()), region(_region), mapping(&parentFn, &parentFn, vectorWidth, CallPredicateMode::SafeWithoutPredicate)
{
    mapping.resultShape = VectorShape::uni();
    for (auto& arg : parentFn.args()) {
      RV_UNUSED(arg);
      mapping.argShapes.push_back(VectorShape::uni());
    }
}

// VectorizationInfo
VectorizationInfo::VectorizationInfo(Region & _region, VectorMapping _mapping)
: DL(_region.getFunction().getParent()->getDataLayout()), region(_region), mapping(_mapping)
{
  assert(mapping.argShapes.size() == mapping.scalarFn->arg_size());
  auto it = mapping.scalarFn->arg_begin();
  for (auto argShape : mapping.argShapes)
  {
    auto & arg = *it;
    setPinned(arg);
    setVectorShape(arg, argShape);
    ++it;
  }
}

bool
VectorizationInfo::hasKnownShape(const llvm::Value& val) const {

  // explicit shape annotation take precedence
    if ((bool) shapes.count(&val)) return true;

  // in-region instruction must have an explicit shape
    auto * inst = dyn_cast<Instruction>(&val);
    if (inst && inRegion(*inst)) return false;

  // out-of-region values default to uniform
    return true;
}

VectorShape
VectorizationInfo::getObservedShape(const LoopInfo & LI, const BasicBlock & observerBlock, const llvm::Value & val) const {
  auto valShape = getVectorShape(val);
  uint alignment = valShape.getAlignmentGeneral();

  if (isTemporalDivergent(LI, observerBlock, val)) {
    return VectorShape::varying(alignment);
  }

  return valShape;
}

VectorShape
VectorizationInfo::getVectorShape(const llvm::Value& val) const
{
// Undef short-cut
  if (isa<UndefValue>(val)) return VectorShape::undef();
   auto it = shapes.find(&val);

// give precedence to user shapes
  if (it != shapes.end()) {
    return it->second;
  }

// return default shape for constants
  auto * constVal = dyn_cast<Constant>(&val);
  if (constVal) {
    return VectorShape::fromConstant(constVal);
  }

// out-of-region values default to uniform
  auto * inst = dyn_cast<Instruction>(&val);
  if (!inst || (inst && !inRegion(*inst))) {
    return VectorShape::uni(); // TODO getAlignment(*inst));
  }

// otw, the shape is undefined
  return VectorShape::undef();
}

const DataLayout &
VectorizationInfo::getDataLayout() const { return DL; }

void
VectorizationInfo::dropVectorShape(const Value& val)
{
    auto it = shapes.find(&val);
    if (it == shapes.end()) return;
    shapes.erase(it);
}


void
VectorizationInfo::setVectorShape(const llvm::Value& val, VectorShape shape)
{
    shapes[&val] = shape;
}


// predicate handling
void
VectorizationInfo::dropPredicate(const BasicBlock& block)
{
    auto it = predicates.find(&block);
    if (it == predicates.end()) return;
    predicates.erase(it);
}

llvm::Value*
VectorizationInfo::getPredicate(const llvm::BasicBlock& block) const
{
    auto it = predicates.find(&block);
    if (it == predicates.end())
    {
        return nullptr;
    }
    else
    {
        return it->second;
    }
}

void
VectorizationInfo::setPredicate(const llvm::BasicBlock& block, llvm::Value& predicate)
{
    predicates[&block] = &predicate;
}


// loop divergence
bool
VectorizationInfo::addDivergentLoop(const Loop & loop) {
  return mDivergentLoops.insert(&loop).second;
}

void
VectorizationInfo::removeDivergentLoop(const Loop & loop) {
  mDivergentLoops.erase(&loop);
}

bool
VectorizationInfo::isDivergentLoop(const llvm::Loop& loop) const
{
    return mDivergentLoops.find(&loop) != mDivergentLoops.end();
}

bool
VectorizationInfo::isDivergentLoopTopLevel(const llvm::Loop& loop) const
{
    Loop* parent = loop.getParentLoop();

    return isDivergentLoop(loop) && (!parent || !isDivergentLoop(*parent));
}


// loop exit divergence
bool
VectorizationInfo::isDivergentLoopExit(const BasicBlock& BB) const {
    return DivergentLoopExits.find(&BB) != DivergentLoopExits.end();
}

bool
VectorizationInfo::addDivergentLoopExit(const BasicBlock& block) {
    return DivergentLoopExits.insert(&block).second;
}

void
VectorizationInfo::removeDivergentLoopExit(const BasicBlock& block) {
    DivergentLoopExits.erase(&block);
}


// pinned shape handling
bool VectorizationInfo::isPinned(const Value& V) const {
  return pinned.count(&V) != 0;
}

void VectorizationInfo::setPinned(const Value& V) {
  pinned.insert(&V);
}



LLVMContext &
VectorizationInfo::getContext() const { return mapping.scalarFn->getContext(); }

BasicBlock&
VectorizationInfo::getEntry() const {
  return region.getRegionEntry();
}

bool
VectorizationInfo::isTemporalDivergent(const LoopInfo & LI,
                                       const BasicBlock &ObservingBlock,
                                       const Value &Val) const {
  const auto *Inst = dyn_cast<const Instruction>(&Val);
  if (!Inst)
    return false;
  // check whether any divergent loop carrying Val terminates before control
  // proceeds to ObservingBlock
  for (const auto *Loop = LI.getLoopFor(Inst->getParent());
       Loop && inRegion(*Loop->getHeader()) && !Loop->contains(&ObservingBlock);
       Loop = Loop->getParentLoop())
  {
    if (isDivergentLoop(*Loop)) {
      return true;
    }
  }

  return false;
}

} /* namespace rv */


