#include <rv/transform/structOpt.h>

#include <vector>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Instructions.h>
#include <llvm/ADT/SmallSet.h>

#include <rv/vectorizationInfo.h>


#include <rvConfig.h>

using namespace rv;
using namespace llvm;

#if 1
#define IF_DEBUG_SO IF_DEBUG
#else
#define IF_DEBUG_SO if (false)
#endif

// TODO move this into vecInfo
VectorShape
rv::StructOpt::getVectorShape(llvm::Value & val) const {
  if (vecInfo.hasKnownShape(val)) return vecInfo.getVectorShape(val);
  else if (isa<Constant>(val)) return VectorShape::uni();
  else return VectorShape::undef();
}

rv::StructOpt::StructOpt(VectorizationInfo & _vecInfo)
: vecInfo(_vecInfo)
{}


bool
rv::StructOpt::mayChangeLayout(llvm::AllocaInst & allocInst) {
  return false;
}

/// whether any address computation on this alloc is uniform
/// the alloca can still be varying because of stored varying values
bool
rv::StructOpt::allUniformGeps(llvm::AllocaInst & allocaInst) {
  SmallSet<Instruction*, 16> seen;
  std::vector<Instruction*> allocaUsers;
  allocaUsers.push_back(&allocaInst);

  while (!allocaUsers.empty()) {
    auto * inst = allocaUsers.back();
    allocaUsers.pop_back();

  // have seen this user
    if (!seen.insert(inst).second) continue;

  // inspect users
    for (auto user : inst->users()) {
      auto * userInst = dyn_cast<Instruction>(user);

      if (!userInst) continue;

      if (isa<GetElementPtrInst>(userInst)) allocaUsers.push_back(userInst);
      //
      // skip if the an alloca derived value is stored somewhere
      else if (isa<StoreInst>(userInst)) {
        if (userInst->getOperand(0) == inst) { // leaking the value!
          return false;
        }
      }

      // we dont care about (indirect) alloc loads
      else if (isa<LoadInst>(userInst)) continue;

      // skip unforeseen users
      else if (!isa<PHINode>(userInst)) return false;

      // otw, descend into user
      allocaUsers.push_back(userInst);
    }

  // verify uniform address criterion
    auto * gep = dyn_cast<GetElementPtrInst>(inst);
    if (gep) {
      // check whether all gep operands are uniform (except the baseptr)
      for (int i = 1; i < gep->getNumOperands(); ++i) {
        if (!getVectorShape(*gep->getOperand(i)).isUniform()) {
          IF_DEBUG_SO { errs() << "skip: non uniform gep: " << *gep << " at index " << i << " : " << *gep->getOperand(i) << "\n"; }
          return false;
        }
      }
    }
  }

  return true;
}

/// try to optimize the layout of this alloca
bool
rv::StructOpt::optimizeAlloca(llvm::AllocaInst & allocaInst) {
  IF_DEBUG_SO {errs() << "\n# trying to optimize Alloca " << allocaInst << "\n"; }

  if (getVectorShape(allocaInst).isUniform()) {
    IF_DEBUG_SO {errs() << "skip: uniform.n"; }
    return false;
  }

  // does this alloca have a vectorizable type?
  auto * vecAllocTy = vectorizeType(*allocaInst.getAllocatedType());
  if (!vecAllocTy) {
    IF_DEBUG_SO { errs() << "skip: non-vectorizable type.\n"; }
    return false;
  }
  IF_DEBUG_SO { errs() << "vectorized type: " << *vecAllocTy << "\n"; }
  //
  // this alloca may only be:
  // loaded from, stored to (must note store the pointer) use to derive addresses (with uniform indicies) or passsed through phi nodes
  if (!allUniformGeps(allocaInst)) return false;
  IF_DEBUG_SO { errs() << "vectorizable uses!\n"; }

  return false;
}

llvm::Type *
rv::StructOpt::vectorizeType(llvm::Type & scalarAllocaTy) {
// primite type -> vector
  if (scalarAllocaTy.isIntegerTy() ||
      scalarAllocaTy.isFloatingPointTy())
    return VectorType::get(&scalarAllocaTy, vecInfo.getVectorWidth());

// finite aggrgate -> aggrgate of vectorized elemnts
  if (scalarAllocaTy.isStructTy()) {
    std::vector<Type*> elemTyVec;
    for (int i = 0; i < scalarAllocaTy.getStructNumElements(); ++i) {
      auto * vecElem = vectorizeType(*scalarAllocaTy.getStructElementType(i));
      if (!vecElem) return nullptr;
      elemTyVec.push_back(vecElem);
    }
    return StructType::get(scalarAllocaTy.getContext(), elemTyVec, false); // TODO packed?
  }

  bool isVectorTy = isa<VectorType>(scalarAllocaTy);
  bool isArrayTy = isa<ArrayType>(scalarAllocaTy);

  if (isVectorTy || isArrayTy) {
    auto * elemTy = scalarAllocaTy.getSequentialElementType();
    auto numElements = isArrayTy ? scalarAllocaTy.getArrayNumElements() : scalarAllocaTy.getVectorNumElements();

    auto * vecElem = vectorizeType(*elemTy);
    if (!vecElem) return nullptr;
    // we create an arraytype in any case since we may promote scalar types (ints) to vectors
    return ArrayType::get(vecElem, numElements);
  }

  // unforeseen
  return nullptr;
}

bool
rv::StructOpt::run() {
  IF_DEBUG_SO { errs() << "-- struct opt log --\n"; }

  bool change = false;
  for (auto & bb : vecInfo.getScalarFunction()) {
    for (auto & inst : bb) {
      if (!isa<AllocaInst>(inst)) continue;
      change |= optimizeAlloca(cast<AllocaInst>(inst));
    }
  }

  IF_DEBUG_SO { errs() << "-- end of struct opt log --\n"; }

  return change;
}

