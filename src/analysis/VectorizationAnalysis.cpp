//===- VectorizationAnalysis.cpp -----------------------------===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// @authors haffner, kloessner, simon
//

#include "rv/analysis/VectorizationAnalysis.h"
#include "rv/intrinsics.h"

#include "rvConfig.h"
#include "utils/rvTools.h"
#include "utils/mathUtils.h"

#include "llvm/IR/Dominators.h"
#include "llvm/Analysis/PostDominators.h"

#include "rv/analysis/AllocaSSA.h"
#include <numeric>

#if 1
#define IF_DEBUG_VA IF_DEBUG
#else
#define IF_DEBUG_VA if (false)
#endif

//
//
// generic transfer functions
rv::VectorShape
GenericTransfer(rv::VectorShape a) {
  return a.isUniform() ? rv::VectorShape::uni() : rv::VectorShape::varying();
}

template<class ... Shapes>
rv::VectorShape
GenericTransfer(rv::VectorShape a, Shapes... nextShapes) {
  if (!a.isUniform()) return rv::VectorShape::varying();
  else return GenericTransfer(nextShapes...);
}



using namespace llvm;

namespace rv {

char VAWrapperPass::ID = 0;

void
VAWrapperPass::getAnalysisUsage(AnalysisUsage& Info) const {
  Info.addRequired<DominatorTreeWrapperPass>();
  Info.addRequired<PostDominatorTreeWrapperPass>();
  Info.addRequired<LoopInfoWrapperPass>();
  Info.addRequired<VectorizationInfoProxyPass>();

  Info.setPreservesAll();
}

bool
VAWrapperPass::runOnFunction(Function& F) {
  auto& Vecinfo = getAnalysis<VectorizationInfoProxyPass>().getInfo();
  auto& platInfo = getAnalysis<VectorizationInfoProxyPass>().getPlatformInfo();

  const auto& domTree = getAnalysis<DominatorTreeWrapperPass>().getDomTree();
  const auto& postDomTree = getAnalysis<PostDominatorTreeWrapperPass>().getPostDomTree();
  const LoopInfo& LoopInfo = getAnalysis<LoopInfoWrapperPass>().getLoopInfo();

  VectorizationAnalysis vea(config, platInfo, Vecinfo, domTree, postDomTree, LoopInfo);
  vea.analyze();

  return false;
}

VectorizationAnalysis::VectorizationAnalysis(Config _config,
                                             PlatformInfo& platInfo,
                                             VectorizationInfo& VecInfo,
                                             const DominatorTree & domTree,
                                             const PostDominatorTree& postDomTree,
                                             const LoopInfo& LoopInfo)

        : config(_config),
          platInfo(platInfo),
          vecInfo(VecInfo),
          layout(platInfo.getDataLayout()),
          mLoopInfo(LoopInfo),
          BDA(*vecInfo.getRegion(), domTree, postDomTree, LoopInfo),
          funcRegion(vecInfo.getScalarFunction()),
          funcRegionWrapper(funcRegion), // FIXME
          allocaSSA(funcRegionWrapper)
{
  // compute pointer provenance
  allocaSSA.compute();
  IF_DEBUG_VA allocaSSA.print(errs());
}

bool
VectorizationAnalysis::putOnWorklist(const llvm::Instruction& inst) {
  if (mOnWorklist.insert(&inst).second) { mWorklist.push(&inst); return true; }
  else return false;
}

const Instruction*
VectorizationAnalysis::takeFromWorklist() {
  if (mWorklist.empty()) return nullptr;
  const Instruction* I = mWorklist.front();
  mWorklist.pop();
  mOnWorklist.erase(I);
  return I;
}

void
VectorizationAnalysis::updateAnalysis(InstVec & updateList) {
  auto & F = vecInfo.getScalarFunction();
  assert (!F.isDeclaration());

  // start from instructions on the update list
  for (auto * inst : updateList) putOnWorklist(*inst);

  compute(F);
  fixUndefinedShapes(F);
  computeLoopDivergence();

  // mark all non-loop exiting branches as divergent to trigger a full linearization
  if (config.foldAllBranches) {
    for (auto & BB: F) {
      auto & term = *BB.getTerminator();
      if (term.getNumSuccessors() <= 1) continue; // uninteresting

      if (!vecInfo.inRegion(BB)) continue; // no begin vectorized

      auto * loop = mLoopInfo.getLoopFor(&BB);
      bool keepShape = loop && loop->isLoopExiting(&BB);

      if (!keepShape) {
        vecInfo.setVectorShape(term, VectorShape::varying());
      }
    }
  }


  IF_DEBUG_VA {
    errs() << "VecInfo after VA:\n";
    vecInfo.dump();
  }
}
void
VectorizationAnalysis::analyze() {
  auto & F = vecInfo.getScalarFunction();
  assert (!F.isDeclaration());

  init(F);
  compute(F);
  fixUndefinedShapes(F);
  computeLoopDivergence();

  // mark all non-loop exiting branches as divergent to trigger a full linearization
  if (config.foldAllBranches) {
    for (auto & BB: F) {
      auto & term = *BB.getTerminator();
      if (term.getNumSuccessors() <= 1) continue; // uninteresting

      if (!vecInfo.inRegion(BB)) continue; // no begin vectorized

      auto * loop = mLoopInfo.getLoopFor(&BB);
      bool keepShape = loop && loop->isLoopExiting(&BB);

      if (!keepShape) {
        vecInfo.setVectorShape(term, VectorShape::varying());
      }
    }
  }


  IF_DEBUG_VA {
    errs() << "VecInfo after VA:\n";
    vecInfo.dump();
  }
}

void VectorizationAnalysis::fixUndefinedShapes(const Function& F) {
  for (const BasicBlock& BB : F) {
    if (!vecInfo.inRegion(BB)) continue;
    for (const Instruction& I : BB) {
      if (!getShape(I).isDefined())
        vecInfo.setVectorShape(I, VectorShape::uni());
    }
  }
}

void VectorizationAnalysis::adjustValueShapes(const Function& F) {
  // Enforce shapes to be existing, if absent, set to VectorShape::undef()
  // If already there, also optimize alignment in case of pointer type

  // Arguments
  for (auto& arg : F.args()) {
    if (!vecInfo.hasKnownShape(arg)) {
      // assert(vecInfo.getRegion() && "will only default function args if in region mode");
      // set argument shapes to uniform if not known better
      vecInfo.setVectorShape(arg, VectorShape::uni());
    } else {
      // Adjust pointer argument alignment
      if (arg.getType()->isPointerTy()) {
        VectorShape argShape = vecInfo.getVectorShape(arg);
        unsigned minAlignment = getBaseAlignment(arg, layout);
        // max is the more precise one
        argShape.setAlignment(std::max<unsigned>(minAlignment, argShape.getAlignmentFirst()));
        vecInfo.setVectorShape(arg, argShape);
      }
    }
  }

  // Instructions in region(!)
  for (auto& BB : F) {
    if (vecInfo.inRegion(BB)) {
      for (auto& I : BB) {
        if (!vecInfo.hasKnownShape(I))
          vecInfo.setVectorShape(I, VectorShape::undef());
      }
    }
  }
}

void VectorizationAnalysis::init(const Function& F) {
  adjustValueShapes(F);

  // Propagation of vector shapes starts at values that do not depend on other values:
  // - function argument's users
  // - Allocas (which are uniform at the beginning)
  // - PHIs with constants as incoming values
  // - Calls without arguments

// push all users of pinned values
  for (auto * val : vecInfo.pinned_values()) {
    addDependentValuesToWL(val);
  }

// push non-user instructions
  for (const BasicBlock& BB : F) {
    vecInfo.setVectorShape(BB, VectorShape::uni());

    for (const Instruction& I : BB) {
      if (isa<AllocaInst>(&I)) {
        update(&I, VectorShape::uni(vecInfo.getMapping().vectorWidth));
      } else if (const CallInst* call = dyn_cast<CallInst>(&I)) {
        if (call->getNumArgOperands() != 0) continue;

        putOnWorklist(I);

        IF_DEBUG_VA {
          errs() << "Inserted call in initialization: ";
          I.printAsOperand(errs(), false);
          errs() << "\n";
        };
      } else if (isa<PHINode>(I) && any_of(I.operands(), isa<Constant, Use>)) {
        // Phis that depend on constants are added to the WL
        putOnWorklist(I);

        IF_DEBUG_VA {
          errs() << "Inserted PHI in initialization: ";
          I.printAsOperand(errs(), false);
          errs() << "\n";
        };
      }
    }
  }
}

void VectorizationAnalysis::update(const Value* const V, VectorShape AT) {
  bool changed = updateShape(V, AT);
  auto * term = dyn_cast<TerminatorInst>(V);
  if (changed && term && term->getNumSuccessors() > 1)
    analyzeDivergence(*term);
}

bool VectorizationAnalysis::updateShape(const Value* const V, VectorShape AT) {
  VectorShape Old = getShape(*V);
  VectorShape New = VectorShape::join(Old, AT);

  if (Old == New) {
    return false;// nothing changed
  }

  IF_DEBUG_VA {
    errs() << "Marking " << New << ": ";
    V->print(errs(), false);
    errs() << "\n";
  };

  vecInfo.setVectorShape(*V, New);

  // Add dependent elements to worklist
  addDependentValuesToWL(V);

  return true;
}

void VectorizationAnalysis::analyzeDivergence(const TerminatorInst & termInst) {
  // Vectorization is caused by non-uniform branches
  if (getShape(termInst).isUniform()) return;

  // Find out which regions diverge because of this non-uniform branch
  // The branch is regarded as varying, even if its condition is only strided
  for (const auto* BB : BDA.join_blocks(termInst)) {
    if (!vecInfo.inRegion(*BB) || getShape(*BB).isVarying()) {
      continue;
    } // filter out irrelevant nodes (FIXME filter out directly in BDA)

    vecInfo.setVectorShape(*BB, VectorShape::varying());

    // Loop exit handling
    if (BB->getUniquePredecessor()) {
      vecInfo.setNotKillExit(BB);
    }

    IF_DEBUG_VA {
      errs() << "\nThe block:\n    ";
      BB->printAsOperand(errs(), false);
      errs() << "\nis divergent because of the non-uniform control in:\n    ";
      termInst.getParent()->printAsOperand(errs(), false);
      errs() << "\n\n";
    }

    // induce divergence into allocas
    const Join * allocaJoin = allocaSSA.getJoinNode(*BB);
    if (allocaJoin) {
      for (const auto * allocInst : allocaJoin->provSet.allocs) {
        updateShape(allocInst, VectorShape::varying());
      }
    }

    // add LCSSA phis to worklist
    for (auto & inst : *BB) {
      if (!isa<PHINode>(inst)) break;
      putOnWorklist(inst);

      IF_DEBUG_VA {
        errs() << "Inserted LCSSA PHI: ";
        inst.printAsOperand(errs(), false);
        errs() << "\n";
      };
    }
  }
}

void VectorizationAnalysis::addDependentValuesToWL(const Value* V) {
  // Push users of this value
  for (const auto user : V->users()) {
    if (!isa<Instruction>(user)) continue;
    const Instruction* inst = cast<Instruction>(user);

    // We are only analyzing the region
    if (!vecInfo.inRegion(*inst)) continue;

    // Ignore calls without return value
    if (const CallInst* callI = dyn_cast<CallInst>(inst)) {
      if (callI->getFunctionType()->getReturnType()->isVoidTy()) {
        continue;
      }
    }

    putOnWorklist(*inst);
    IF_DEBUG_VA errs() << "Inserted users of " << *V << ":" << *user << "\n";
  }
}

VectorShape VectorizationAnalysis::joinIncomingValues(const PHINode& phi) {
  VectorShape Join = VectorShape::undef();
  for (size_t i = 0; i < phi.getNumIncomingValues(); ++i) {
    Join = VectorShape::join(Join, getShape(*phi.getIncomingValue(i)));
  }
  return Join;
}

bool VectorizationAnalysis::pushMissingOperands(const Instruction* I) {
  auto pushIfMissing = [this](bool prevpushed, Value* op)
  {
    bool push = isa<Instruction>(op) && !getShape(*op).isDefined();
    if (push) {
      auto & opInst = *cast<Instruction>(op);
      IF_DEBUG_VA { errs() << "\tmissing op shape " << opInst << "!\n"; }
      putOnWorklist(opInst);
    }

    return prevpushed || push;
  };

  return std::accumulate(I->op_begin(), I->op_end(), false, pushIfMissing);
}

VectorShape VectorizationAnalysis::computePHIShape(const PHINode & phi) {
   // The PHI node is not actually varying iff all input operands are the same
   // If the block is divergent the phi is varying
   if (getShape(*phi.getParent()).isVarying()) {
     // TODO infer greatest common alignment
     return VectorShape::varying();
   } else {
     return joinIncomingValues(phi);
   }
}

void VectorizationAnalysis::compute(const Function& F) {
  IF_DEBUG_VA { errs() << "\n\n-- VA::compute() log -- \n"; }

  // main fixed point loop
  while (true) {
    const Instruction * I = takeFromWorklist();
    if (!I) break; // worklist is empty

    if (vecInfo.isPinned(*I)) {
      continue;
    }

    IF_DEBUG_VA { errs() << "# next: " << *I << "\n"; }

    // compute the output shape
    VectorShape New;
    if (isa<PHINode>(I)) {
      // allow incomplete inputs for PHI nodes
      New = computePHIShape(cast<PHINode>(*I));
      if (!New.isDefined()) pushMissingOperands(I);
    } else if (pushMissingOperands(I)) {
      // If any operand is bottom put them in the work list.
      continue;
    } else {
      // Otw, we can compute the instruction shape
      SmallValVec taintedPtrOps;
      New = computeShapeForInst(I, taintedPtrOps);

      // taint allocas
      for (auto * ptr : taintedPtrOps) {
        const auto & prov = allocaSSA.getProvenance(*ptr);
        for (const auto * allocaInst : prov.allocs) {
          if (updateShape(allocaInst, VectorShape::varying())) {
            addDependentValuesToWL(allocaInst);
          }
        }
      }

      if (config.vaMethod == Config::VA_Karrenberg) {
        // degrade non-cont negatively strided shapes

        if (New.isVarying()) {
          // keep
        } else if (New.getStride() < 0) {
          // negative stride
          New = VectorShape::varying();
        } else if (New.isUniform()) {
          // keep uniform
        } else {
          // contiguous case
          int64_t contStride = 1;

          if (I->getType()->isPointerTy()) {
            // contiguous stride for pointers (bytes)
            auto *elemTy = I->getType()->getPointerElementType();
            contStride = layout.getTypeStoreSize(elemTy);
          }

          if (New.getStride() != contStride) {
            New = VectorShape::varying();
          }
        }

      } else if (config.vaMethod == Config::VA_Coutinho) {
        // only degrade alignment information
        New.setAlignment(1);
      }
    }

    IF_DEBUG_VA { errs() << "\t computed: " << New.str() << "\n"; }

    // if no output shape could be computed, skip.
    if (!New.isDefined()) continue;

    // shape is non-bottom. Apply general refinement rules.
    if (I->getType()->isPointerTy()) {
      // adjust result type to match alignment
      unsigned minAlignment = getBaseAlignment(*I, layout);
      New.setAlignment(std::max<unsigned>(minAlignment, New.getAlignmentFirst()));
    } else if (I->getType()->isFloatingPointTy()) {
      // allow strided/aligned fp values only in fast math mode
      FastMathFlags flags = I->getFastMathFlags();
      if (!flags.isFast() && !New.isUniform()) {
        New = VectorShape::varying();
      }
    }

    // if shape changed put users on worklist
    update(I, New);
  };
}


static
bool
HasSideEffects(const Function & func) {
  if (func.hasFnAttribute(Attribute::ReadOnly) || func.hasFnAttribute(Attribute::ReadNone)) {
    return false;
  }

  return true;
}

VectorShape VectorizationAnalysis::computeShapeForInst(const Instruction* I, SmallValVec & taintedOps) {
  // always default to the naive transformer (only top or bottom)
  if (config.vaMethod == Config::VA_TopBot) { return computeGenericArithmeticTransfer(*I); }

  if (I->isBinaryOp()) return computeShapeForBinaryInst(cast<BinaryOperator>(I));
  if (I->isCast()) return computeShapeForCastInst(cast<CastInst>(I));

  switch (I->getOpcode()) {
    case Instruction::Alloca:
    {
      const int alignment = vecInfo.getMapping().vectorWidth;
      auto* AllocatedType = I->getType()->getPointerElementType();
      const bool Vectorizable = false;

      if (Vectorizable) {
        int typeStoreSize = (int)(layout.getTypeStoreSize(AllocatedType));
        return VectorShape::strided(typeStoreSize, alignment);
      }

      return VectorShape::varying();
    }
    case Instruction::Br:
    {
      const BranchInst* branch = cast<BranchInst>(I);
      assert(branch->isConditional());
      return getShape(*branch->getCondition());
    }
    case Instruction::Switch:
    {
      const SwitchInst* sw = cast<SwitchInst>(I);
      return getShape(*sw->getCondition());
    }
    case Instruction::ICmp:
    {
      const Value* op1 = I->getOperand(0);
      const Value* op2 = I->getOperand(1);

      // Get a shape for op1 - op2 and see if it compares uniform to a full zero-vector
      VectorShape diffShape = getShape(*op1) - getShape(*op2);
      if (diffShape.isVarying())
        return diffShape;

      CmpInst::Predicate predicate = cast<CmpInst>(I)->getPredicate();
      switch (predicate) {
        case CmpInst::Predicate::ICMP_SGT:
        case CmpInst::Predicate::ICMP_UGT:
        case CmpInst::Predicate::ICMP_SLE:
        case CmpInst::Predicate::ICMP_ULE:
          diffShape = -diffShape; // Negate and handle like LESS/GREATER_EQUAL
          // fallthrough
        case CmpInst::Predicate::ICMP_SLT:
        case CmpInst::Predicate::ICMP_ULT:
        case CmpInst::Predicate::ICMP_SGE:
        case CmpInst::Predicate::ICMP_UGE:
        // These coincide because a >= b is equivalent to !(a < b)
        {
          const int vectorWidth = (int)vecInfo.getMapping().vectorWidth;
          const int stride = diffShape.getStride();
          const int alignment = diffShape.getAlignmentFirst();

          if (stride >= 0 && alignment >= stride * vectorWidth)
            return VectorShape::uni();

        }
      break;

        case CmpInst::Predicate::ICMP_EQ:
        case CmpInst::Predicate::ICMP_NE:
        {
          if (diffShape.getStride() == 0)
            return VectorShape::uni();

        }
      break;

        default:
          break;
      }

      return VectorShape::varying();
    }

    case Instruction::GetElementPtr:
    {
      const GetElementPtrInst* gep = cast<GetElementPtrInst>(I);
      const Value* pointer = gep->getPointerOperand();

      VectorShape result = getShape(*pointer);
      Type* subT = gep->getPointerOperandType();

      for (const Value* index : make_range(gep->idx_begin(), gep->idx_end())) {
        if (StructType* Struct = dyn_cast<StructType>(subT)) {
          if (!isa<ConstantInt>(index)) return VectorShape::varying();

          subT = Struct->getTypeAtIndex(index);

          auto structlayout = layout.getStructLayout(Struct);
          unsigned idxconst = (unsigned) cast<ConstantInt>(index)->getSExtValue();
          unsigned elemoffset = (unsigned) structlayout->getElementOffset(idxconst);

          // Behaves like addition, pointer + offset and offset is uniform, hence the stride stays
          unsigned newalignment = gcd(result.getAlignmentFirst(), elemoffset);
          result.setAlignment(newalignment);
        } else {
          // NOTE: If indexShape is varying, this still reasons about alignment
          subT = isa<PointerType>(subT) ? subT->getPointerElementType() : subT->getSequentialElementType();

          const int typeSizeInBytes = (int)layout.getTypeStoreSize(subT);
          result = result + typeSizeInBytes * getShape(*index);
        }
      }

      return result;
    }

    case Instruction::Call:
    {
      auto & call = *cast<CallInst>(I);
      const auto* calledValue = call.getCalledValue();
      const Function * callee = dyn_cast<Function>(calledValue);
      if (!callee) return VectorShape::varying(); // calling a non-function

      // memcpy shape is uniform if src ptr shape is uniform
      // TODO re-factor into resolver
      Intrinsic::ID id = callee->getIntrinsicID();
      if (id == Intrinsic::memcpy) {
        auto & mcInst = cast<MemCpyInst>(call);
        auto srcShape = getShape(*mcInst.getSource());
        if (!srcShape.isUniform()) taintedOps.push_back(mcInst.getDest());
        return srcShape.isUniform() ? srcShape : VectorShape::varying();
      } else if (id == Intrinsic::memmove) {
        auto & movInst = cast<MemMoveInst>(call);
        auto srcShape = getShape(*movInst.getSource());
        if (!srcShape.isUniform()) taintedOps.push_back(movInst.getDest());
        return srcShape.isUniform() ? srcShape : VectorShape::varying();
      }

      // If the function is rv_align, use the alignment information
      if (IsIntrinsic(call, RVIntrinsic::Align)) {
        auto shape = getShape(*I->getOperand(0));
        shape.setAlignment(cast<ConstantInt>(I->getOperand(1))->getZExtValue());
        return shape;
      }

      // collect required argument shapes
      bool allArgsUniform = true;
      size_t numParams = call.getNumArgOperands();
      VectorShapeVec callArgShapes;
      for (size_t i = 0; i < numParams; ++i) {
        auto& op = *call.getArgOperand(i);
        auto argShape = getShape(op);
        allArgsUniform &= argShape.isUniform();
        callArgShapes.push_back(argShape);
      }

      bool isUniformCall = allArgsUniform && !HasSideEffects(*callee);

      // query available user mappings (vla aware simd mappings of scalar functions)
      bool needsPredication = false; // FIXME whether the call needs predication due to the call context
      VecMappingShortVec matchVec;
      platInfo.getMappingsForCall(matchVec, *callee, callArgShapes, vecInfo.getVectorWidth(), needsPredication);

      // pick mapping with the most precise result shape
      if (!matchVec.empty()) {
        VectorShape bestResultShape = VectorShape::varying();
        for (const auto & mapping : matchVec) {
          bestResultShape = mapping.resultShape.morePreciseThan(bestResultShape) ? mapping.resultShape : bestResultShape;
        }

        return bestResultShape;
      }

      // default behavior for non-VLA functions
      if (isUniformCall) return VectorShape::uni();

      // next: query resolver mechanism // TODO account for predicate
      auto resolver = platInfo.getResolver(callee->getName(), *callee->getFunctionType(), callArgShapes, vecInfo.getVectorWidth());
      if (resolver) {
        return resolver->requestResultShape();
      }

      return VectorShape::varying();
    }

    case Instruction::Load:
    {
      const Value* pointer = I->getOperand(0);
      return VectorShape::join(VectorShape::uni(), getShape(*pointer));
    }

    case Instruction::Store:
    {
      auto & storeInst = cast<StoreInst>(*I);
      auto valShape = getShape(*storeInst.getValueOperand());
      auto ptrShape = getShape(*storeInst.getPointerOperand());
      if (!valShape.isUniform()) taintedOps.push_back(storeInst.getPointerOperand());
      return VectorShape::join(ptrShape, valShape.isUniform() ? valShape : VectorShape::varying());
    }

    case Instruction::Select:
    {
      const Value* condition = I->getOperand(0);
      const Value* selection1 = I->getOperand(1);
      const Value* selection2 = I->getOperand(2);

      const VectorShape& condShape = getShape(*condition);
      const VectorShape& sel1Shape = getShape(*selection1);
      const VectorShape& sel2Shape = getShape(*selection2);

      if (!condShape.isUniform()) return VectorShape::varying();

      return VectorShape::join(sel1Shape, sel2Shape);
    }

      // use the generic transfer
    default:
      break;
  }

  return computeGenericArithmeticTransfer(*I);
}

VectorShape VectorizationAnalysis::computeGenericArithmeticTransfer(const Instruction & I) {
  assert(I.getNumOperands() > 0 && "can not compute arithmetic transfer for instructions w/o operands");
  // generic transfer function
  for (unsigned i = 0; i < I.getNumOperands(); ++i) {
    if (!getShape(*I.getOperand(i)).isUniform()) return VectorShape::varying();
  }
  return VectorShape::uni();
}

VectorShape VectorizationAnalysis::computeShapeForBinaryInst(const BinaryOperator* I) {
  Value* op1 = I->getOperand(0);
  Value* op2 = I->getOperand(1);

  // Assume constants are on the RHS
  if (!isa<Constant>(op2) && I->isCommutative()) std::swap(op1, op2);

  const VectorShape& shape1 = getShape(*op1);
  const VectorShape& shape2 = getShape(*op2);

  const int stride1 = shape1.getStride();

  const unsigned alignment1 = shape1.getAlignmentFirst();
  const unsigned alignment2 = shape2.getAlignmentFirst();

  const unsigned generalalignment1 = shape1.getAlignmentGeneral();
  const unsigned generalalignment2 = shape2.getAlignmentGeneral();

  switch (I->getOpcode()) {
    case Instruction::Add:
    case Instruction::FAdd:
      return shape1 + shape2;

    case Instruction::Sub:
    case Instruction::FSub:
      return shape1 - shape2;

    // Integer multiplication with a constant simply multiplies the shape offset
    // if existent with the constant value
    // Alignment constants are multiplied
    case Instruction::Mul:
    {
      if (shape1.isVarying() || shape2.isVarying())
        return VectorShape::varying(generalalignment1 * generalalignment2);

      if (shape1.isUniform() && shape2.isUniform())
        return VectorShape::uni(alignment1 * alignment2);

      // If the constant is known, compute the new shape directly
      if (const ConstantInt* constantOp = dyn_cast<ConstantInt>(op2)) {
        const int c = (int) constantOp->getSExtValue();
        return c * shape1;
      }

      return VectorShape::varying(generalalignment1 * generalalignment2);
    }

    case Instruction::Or: {
    // In some cases Or-with-constant can be interpreted as an Add-with-constant
      // this holds e.g. for: <4, 6, 8, 10> | 1 = <5, 7, 9, 11>
      if (!isa<ConstantInt>(op2)) break;

      unsigned orConst = cast<ConstantInt>(op2)->getZExtValue();
      VectorShape otherShape = getShape(*op1);

      if (orConst == 0) {
        // no-op
        return otherShape;
      }

      unsigned laneAlignment = otherShape.getAlignmentGeneral();
      if (laneAlignment <= 1) break;

      // all bits above constTopBit are zero
      unsigned constTopBit = static_cast<unsigned>(highest_bit(orConst));

    // there is an overlap between the bits of the constant and a possible lane value
      if (constTopBit >= laneAlignment) {
        break;
      }

    // from this point we know that the Or behaves like an Add
      if (otherShape.hasStridedShape()) {
        // the Or operates like an Add with an uniform value
        auto resAlignment = gcd<unsigned>(orConst, otherShape.getAlignmentFirst());
        return VectorShape::strided(otherShape.getStride(), resAlignment);

      } else {
        return VectorShape::varying(gcd<unsigned>(laneAlignment, orConst));
      }

      break;
    }

    case Instruction::Shl: {
      // interpret shift by constant as multiplication
      if (auto* shiftCI = dyn_cast<ConstantInt>(op2)) {
        int shiftAmount = (int) shiftCI->getSExtValue();
        if (shiftAmount > 0) {
          int factor = 1 << shiftAmount;
          return factor * shape1;
        }
      }

    } break;

    case Instruction::SDiv:
    case Instruction::UDiv:
    {
      const ConstantInt* constDivisor = dyn_cast<ConstantInt>(op2);
      if (shape1.hasStridedShape() && constDivisor) {
        const int64_t c  = constDivisor->getSExtValue();// FIXME proper code path for UDiv

        if (c == 0) return VectorShape::uni(1); // FIXME divide by zero?
        if ((alignment1 % c == 0) && // c divides the alignment
            (stride1 % c == 0))      // c divides the stride
        {
          return VectorShape::strided(stride1 / c, alignment1 / std::abs(c));
        }
      }

      if (shape1.isUniform() && shape2.isUniform()) {
        return VectorShape::uni(1); // division destroyes alignment in general
      }

      return VectorShape::varying(1);
    }

    default:
      break;
  }

  return GenericTransfer(shape1, shape2);
}

VectorShape VectorizationAnalysis::computeShapeForCastInst(const CastInst* castI) {
  const Value* castOp = castI->getOperand(0);
  const VectorShape& castOpShape = getShape(*castOp);
  const int castOpStride = castOpShape.getStride();

  const int aligned = !rv::returnsVoidPtr(*castI) ? castOpShape.getAlignmentFirst() : 1;

  if (castOpShape.isVarying()) return castOpShape;

  switch (castI->getOpcode()) {
    case Instruction::IntToPtr:
    {
      PointerType* DestType = cast<PointerType>(castI->getDestTy());
      Type* DestPointsTo = DestType->getPointerElementType();

      // FIXME: void pointers are char pointers (i8*), but what
      // difference is there between a real i8* and a void pointer?
      if (DestPointsTo->isIntegerTy(8)) return VectorShape::varying();

      unsigned typeSize = (unsigned) layout.getTypeStoreSize(DestPointsTo);

      if (castOpStride % typeSize != 0) return VectorShape::varying();

      return VectorShape::strided(castOpStride / typeSize, 1);
    }

    case Instruction::PtrToInt:
    {
      Type* SrcElemType = castI->getSrcTy()->getPointerElementType();

      unsigned typeSize = (unsigned) layout.getTypeStoreSize(SrcElemType);

      return VectorShape::strided(typeSize * castOpStride, aligned);
    }

      // Truncation reinterprets the stride modulo the target type width
      // i16 to i1: stride(odd) -> consecutive, stride(even) -> uniform
    case Instruction::Trunc:
    {
      Type* destTy = castI->getDestTy();

      return truncateToTypeSize(castOpShape, (unsigned) layout.getTypeStoreSize(destTy));
    }

    // FIXME: is this correct?
    case Instruction::ZExt:
    case Instruction::SExt:
    case Instruction::FPExt:
      // NOTE: This consciously ignores large absolute values above 2²⁴
    case Instruction::UIToFP:
    case Instruction::SIToFP:
    {
      return castOpShape;
    }

    // Rounds towards zero: <-1.5f, -0.5f, 0.5f, 1.5f> -> <-1, 0, 0, 1>
    // "If the cast produces an inexact result, how rounding is performed [...]
    // is undefined."
    case Instruction::FPTrunc:
    case Instruction::FPToSI:
    case Instruction::FPToUI:
    {
      return VectorShape::join(VectorShape::uni(aligned), castOpShape);
    }

    case Instruction::BitCast:
    {
      Type* srcType = castI->getSrcTy();
      Type* destType = castI->getDestTy();

      // no floating point value involved: keep shape since int<->ptr are compatible
      if (!srcType->isFloatingPointTy() && !destType->isFloatingPointTy()) {
        return castOpShape;
      }

      // Uniform values stay uniform
      if (castOpShape.isUniform()) return castOpShape;

      // BC from fp<->int/ptr is incomatible -> default to varying shape
      return VectorShape::varying();
    }

    default:
      return VectorShape::join(VectorShape::uni(aligned), castOpShape);
  }
}

VectorShape VectorizationAnalysis::getShape(const Value& V) {
  return vecInfo.getVectorShape(V);
}

FunctionPass* createVectorizationAnalysisPass(rv::Config config) {
  return new VAWrapperPass(config);
}

void VectorizationAnalysis::computeLoopDivergence() {
  std::stack<const Loop*> loops;
  for (const Loop* l : mLoopInfo) {
    loops.push(l);
  }

  while (!loops.empty()) {
    const Loop* l = loops.top();
    loops.pop();

    SmallVector<BasicBlock*, 4> exits;
    l->getExitBlocks(exits);

    for (const BasicBlock* exit : exits) {
      if (vecInfo.getVectorShape(*exit).isVarying()) {
        vecInfo.setLoopDivergence(*l, false);
        break;
      }
    }

    for (const Loop* subLoop : l->getSubLoops()) {
      loops.push(subLoop);
    }
  }
}



}
