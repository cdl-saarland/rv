//===- rv/vectorizationInfo.cpp - vectorizer IR using an overlay object --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//

#include "rv/vectorizationInfo.h"

#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Instruction.h>

#include "rv/region/Region.h"
#include "utils/rvTools.h"
#include "rvConfig.h"

using namespace llvm;

static bool IsLCSSA = true;

namespace rv {

bool VectorizationInfo::inRegion(const BasicBlock &block) const {
  return region.contains(&block);
}

bool VectorizationInfo::inRegion(const Instruction &inst) const {
  return region.contains(inst.getParent());
}

void VectorizationInfo::dump(const Value *val) const { print(val, errs()); }

void VectorizationInfo::print(const Value *val, llvm::raw_ostream &out) const {
  if (!val)
    return;

  auto *block = dyn_cast<const BasicBlock>(val);
  if (block && inRegion(*block)) {
    printBlockInfo(*block, out);
  }

  out << *val;

  // show shadow input (if any)
  auto * phi = dyn_cast<PHINode>(val);
  if (phi) {
    const Value * shadowIn = getShadowInput(*phi);
    if (shadowIn) {
      out << ", shadow(";
        shadowIn->printAsOperand(out, false, getScalarFunction().getParent());
      out << ")";
    }
  }

  // attach vector shape
  if (hasKnownShape(*val)) {
    out << " : " << getVectorShape(*val).str() << "\n";
  } else {
    out << " : <n/a>\n";
  }
}

void VectorizationInfo::dumpBlockInfo(const BasicBlock &block) const {
  printBlockInfo(block, errs());
}

void VectorizationInfo::printBlockInfo(const BasicBlock &block,
                                       llvm::raw_ostream &out) const {
  // block name
  out << "Block ";
  block.printAsOperand(out, false);

  // block annotations
  out << " [";
  {
    bool needsComma = false;
    bool hasVaryingPredicate = false;
    if (getVaryingPredicateFlag(block, hasVaryingPredicate)) {
      if (hasVaryingPredicate) out << "var-pred";
      else out << "uni-pred";
      needsComma = true;
    }

    if (hasMask(block)) {
      if (needsComma) {
        out << ", ";
      }
      getMask(block).print(out);
      needsComma = true;
    }

    if (isDivergentLoopExit(block)) {
      if (needsComma) {
        out << ", ";
      }
      out << "divLoopExit";
      needsComma = true;
    }
  }
  out << "]";
  out << "\n";

  // instructions
  for (const Instruction &inst : block) {
    print(&inst, out);
  }
  out << "\n";
}

void VectorizationInfo::dumpArguments() const { printArguments(errs()); }

void VectorizationInfo::printArguments(llvm::raw_ostream &out) const {
  const Function *F = mapping.scalarFn;

  out << "\nArguments:\n";

  for (auto &arg : F->args()) {
    out << arg << " : "
        << (hasKnownShape(arg) ? getVectorShape(arg).str() : "missing") << "\n";
  }

  out << "\n";
}

void VectorizationInfo::dump() const { print(errs()); }

void VectorizationInfo::print(llvm::raw_ostream &out) const {
  out << "VectorizationInfo ";

  out << "for " << region.str() << "\n";

  printArguments(out);

  if (EntryAVL) {
    out << "Entry AVL: " << *EntryAVL << "\n";
  }

  for (const BasicBlock &block : *mapping.scalarFn) {
    if (!inRegion(block))
      continue;
    printBlockInfo(block, out);
  }

  out << "}\n";
}

VectorizationInfo::VectorizationInfo(llvm::Function &parentFn,
                                     unsigned vectorWidth, Region &_region)
    : DL(parentFn.getParent()->getDataLayout()), EntryAVL(nullptr),
      region(_region), mapping(&parentFn, &parentFn, vectorWidth,
                               CallPredicateMode::SafeWithoutPredicate) {
  mapping.resultShape = VectorShape::uni();
  for (auto &arg : parentFn.args()) {
    RV_UNUSED(arg);
    mapping.argShapes.push_back(VectorShape::uni());
  }
}

// VectorizationInfo
VectorizationInfo::VectorizationInfo(Region &_region, VectorMapping _mapping)
    : DL(_region.getFunction().getParent()->getDataLayout()),
      EntryAVL(nullptr), region(_region), mapping(_mapping) {
  assert(mapping.argShapes.size() == mapping.scalarFn->arg_size());
  auto it = mapping.scalarFn->arg_begin();
  for (auto argShape : mapping.argShapes) {
    auto &arg = *it;
    setPinned(arg);
    setVectorShape(arg, argShape);
    ++it;
  }
}

bool VectorizationInfo::hasKnownShape(const llvm::Value &val) const {

  // explicit shape annotation take precedence
  if ((bool)shapes.count(&val))
    return true;

  // in-region instruction must have an explicit shape
  auto *inst = dyn_cast<Instruction>(&val);
  if (inst && inRegion(*inst))
    return false;

  // out-of-region values default to uniform
  return true;
}

VectorShape VectorizationInfo::getObservedShape(const LoopInfo &LI,
                                                const BasicBlock &observerBlock,
                                                const llvm::Value &val) const {
  auto valShape = getVectorShape(val);
  uint alignment = valShape.getAlignmentGeneral();

  if (isTemporalDivergent(LI, observerBlock, val)) {
    return VectorShape::varying(alignment);
  }

  return valShape;
}

VectorShape
VectorizationInfo::getVectorShape(const Mask &M) const {
  VectorShape avlMaskingShape = M.knownAllTrueAVL() ? VectorShape::uni() : VectorShape::varying();
  VectorShape predMaskingShape = M.getPred() ? getVectorShape(*M.getPred()) : VectorShape::uni();
  return VectorShape::join(predMaskingShape, avlMaskingShape);
}


VectorShape VectorizationInfo::getVectorShape(const llvm::Value &val) const {
  // Undef short-cut
  if (isa<UndefValue>(val))
    return VectorShape::undef();
  auto it = shapes.find(&val);

  // give precedence to user shapes
  if (it != shapes.end()) {
    return it->second;
  }

  // return default shape for constants
  auto *constVal = dyn_cast<Constant>(&val);
  if (constVal) {
    return VectorShape::fromConstant(constVal);
  }

  // out-of-region values default to uniform
  auto *inst = dyn_cast<Instruction>(&val);
  if (!inst || (inst && !inRegion(*inst))) {
    return VectorShape::uni(); // TODO getAlignment(*inst));
  }

  // otw, the shape is undefined
  return VectorShape::undef();
}

const DataLayout &VectorizationInfo::getDataLayout() const { return DL; }

void
VectorizationInfo::forgetInferredProperties() {
  VaryingPredicateBlocks.clear();
  mDivergentLoops.clear();
  DivergentLoopExits.clear();
  JoinDivergentBlocks.clear();

  std::set<const Value*> ForgetValues;
  for (auto ItValShape : shapes) {
    if (pinned.count(ItValShape.first)) continue;
    ForgetValues.insert(ItValShape.first);
  }
  for (const Value * ForgetVal : ForgetValues) {
    auto It = shapes.find(ForgetVal);
    assert(It != shapes.end());
    shapes.erase(It);
  }
}

void VectorizationInfo::dropVectorShape(const Value &val) {
  auto it = shapes.find(&val);
  if (it == shapes.end())
    return;
  shapes.erase(it);
}

// does not make sense since the mask shape is a join of two value shapes..
// which value to ascribe the shape to?
#if 0
void VectorizationInfo::setVectorShape(const Mask &M,
                                       VectorShape S) {
  // Check that M does have a predicate if S is a non-uniform shape
  if (!S.isUniform() && S.isDefined()) {
    assert(M.getPred());
  }

  // set the AVL to uniform (if it has not happened yet)
  if (M.getAVL() && !getVectorShape(*M.getAVL()).isDefined()) {
    setVectorShape(*M.getAVL(), VectorShape::uni());
  }

  // Otw, set the shape on the mask predicate
  if (!M.getPred()) return;
  setVectorShape(*M.getPred(), S);
}
#endif

void VectorizationInfo::setVectorShape(const llvm::Value &val,
                                       VectorShape shape) {
  shapes[&val] = shape;
}

// tenative predicate handling
bool
VectorizationInfo::getVaryingPredicateFlag(const llvm::BasicBlock &BB, bool & oIsVarying) const {
  auto it = VaryingPredicateBlocks.find(&BB);
  if (it == VaryingPredicateBlocks.end()) return false;
  oIsVarying = it->second;
  return true;
}

void
VectorizationInfo::setVaryingPredicateFlag(const llvm::BasicBlock & BB, bool toVarying) {
  VaryingPredicateBlocks[&BB] = toVarying;
}

void
VectorizationInfo::removeVaryingPredicateFlag(const llvm::BasicBlock & BB) {
  VaryingPredicateBlocks.erase(&BB);
}

bool
VectorizationInfo::hasMask(const BasicBlock & block) const {
  auto it = masks.find(&block);
  return it != masks.end();
}

Mask&
VectorizationInfo::requestMask(const llvm::BasicBlock & block) {
  auto it = masks.find(&block);
  if (it == masks.end()) {
    auto ItInserted = masks.insert(std::pair<const BasicBlock*, Mask>(&block, Mask()));
    return ItInserted.first->second;

  } else {
    return it->second;
  }
}

const Mask&
VectorizationInfo::getMask(const llvm::BasicBlock & block) const {
  auto it = masks.find(&block);
  assert(it != masks.end());
  return it->second;
}

// predicate handling
void VectorizationInfo::dropMask(const BasicBlock &block) {
  auto it = masks.find(&block);
  if (it == masks.end())
    return;
  masks.erase(it);
}

void VectorizationInfo::setMask(const llvm::BasicBlock &block, Mask NewMask) {
  requestMask(block) = NewMask;
}

llvm::Value*
VectorizationInfo::getPredicate(const llvm::BasicBlock &block) const {
  if (hasMask(block)) {
    return getMask(block).getPred();
  }
  return nullptr;
}

void VectorizationInfo::setPredicate(const llvm::BasicBlock &block,
                                     llvm::Value &NewPred) {
  requestMask(block).setPred(&NewPred);
}

void VectorizationInfo::remapPredicate(Value &Dest, Value &Old) {
  for (auto it : masks) {
    if (it.second.getPred() == &Old) {
      it.second.setPred(&Dest);
    }
  }
}

// loop divergence
bool VectorizationInfo::addDivergentLoop(const Loop &loop) {
  return mDivergentLoops.insert(&loop).second;
}

void VectorizationInfo::removeDivergentLoop(const Loop &loop) {
  mDivergentLoops.erase(&loop);
}

bool VectorizationInfo::isDivergentLoop(const llvm::Loop &loop) const {
  return mDivergentLoops.find(&loop) != mDivergentLoops.end();
}

bool VectorizationInfo::isDivergentLoopTopLevel(const llvm::Loop &loop) const {
  Loop *parent = loop.getParentLoop();

  return isDivergentLoop(loop) && (!parent || !isDivergentLoop(*parent));
}

// loop exit divergence
bool VectorizationInfo::isDivergentLoopExit(const BasicBlock &BB) const {
  return DivergentLoopExits.find(&BB) != DivergentLoopExits.end();
}

bool VectorizationInfo::addDivergentLoopExit(const BasicBlock &block) {
  return DivergentLoopExits.insert(&block).second;
}

void VectorizationInfo::removeDivergentLoopExit(const BasicBlock &block) {
  DivergentLoopExits.erase(&block);
}

// pinned shape handling
bool VectorizationInfo::isPinned(const Value &V) const {
  return pinned.count(&V) != 0;
}

void VectorizationInfo::setPinned(const Value &V) { pinned.insert(&V); }

LLVMContext &VectorizationInfo::getContext() const {
  return mapping.scalarFn->getContext();
}

BasicBlock &VectorizationInfo::getEntry() const {
  return region.getRegionEntry();
}

bool VectorizationInfo::isTemporalDivergent(const LoopInfo &LI,
                                            const BasicBlock &ObservingBlock,
                                            const Value &Val) const {
  const auto *Inst = dyn_cast<const Instruction>(&Val);
  if (!Inst)
    return false;

  const auto *DefLoop = LI.getLoopFor(Inst->getParent());
  if (!DefLoop || DefLoop->contains(&ObservingBlock)) {
    return false;
  }

  // FIXME this is imprecise (liveouts of uniform exits appear varying, eventhough they are uniform)
  if (!IsLCSSA) {
    // check whether any divergent loop carrying Val terminates before control
    // proceeds to ObservingBlock
    for (const auto *Loop = DefLoop;
         Loop && inRegion(*Loop->getHeader()) && !Loop->contains(&ObservingBlock);
         Loop = Loop->getParentLoop()) {
      if (isDivergentLoop(*Loop)) {
        return true;
      }
    }

  } else {
    // all loop live-outs are funneled through LCSSA phis that sit on immediate exit blocks.
    // As such, only LCSSA phi nodes can observed temporal divergence.
    return isDivergentLoopExit(ObservingBlock);
  }

  return false;
}

} // namespace rv
