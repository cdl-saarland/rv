#include <rv/transform/structOpt.h>

#include <vector>

#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Instructions.h>
#include <llvm/ADT/SmallSet.h>
#include <llvm/Transforms/Utils/ValueMapper.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/Transforms/Utils/SSAUpdater.h>

#include <rv/vectorizationInfo.h>


#include <rvConfig.h>
#include "report.h"

using namespace llvm;

#if 1
#define IF_DEBUG_SO IF_DEBUG
#else
#define IF_DEBUG_SO if (false)
#endif

namespace rv {

static bool
IsDecomposable(Type & ty) {
  return isa<StructType>(ty) || isa<ArrayType>(ty) || isa<VectorType>(ty);
}

static bool
IsLifetimeUse(Instruction & userInst) {
  // look through bc (to i8 presumably)
  auto * bc = dyn_cast<BitCastInst>(&userInst);
  auto & ptrUser = bc ? *bc : userInst;

  // that pointer must only be used in lifetimes
  auto * call = dyn_cast<CallInst>(&ptrUser);
  if (!call) return false;
  auto * callee = dyn_cast<Function>(call->getCalledValue());
  if (!callee) return false;

  if ((callee->getIntrinsicID() != Intrinsic::lifetime_start) &&
      (callee->getIntrinsicID() != Intrinsic::lifetime_end)) {
    return false;
  }

  // ok
  return true;
}

static bool
IsLoadStoreIntrinsicUse(Value * userInst) {
  if (auto call = dyn_cast<CallInst>(userInst)) {
    auto * callee = dyn_cast<Function>(call->getCalledValue());
    return callee && (callee->getName() == "rv_load" || callee->getName() == "rv_store");
  } else if (auto bitcast = dyn_cast<BitCastInst>(userInst)) {
    for (auto user : bitcast->users()) {
      if (!IsLoadStoreIntrinsicUse(user)) return false;
    }
    return true;
  }

  return false;
}

static void
RemapLoadStoreIntrinsicShape(Value * userInst, VectorizationInfo & vecInfo) {
  if (auto bitcast = dyn_cast<BitCastInst>(userInst)) {
    for (auto user : bitcast->users()) {
      RemapLoadStoreIntrinsicShape(user, vecInfo);
    }
    vecInfo.setVectorShape(*userInst, VectorShape::uni());
  }
}

// TODO move this into vecInfo
VectorShape
StructOpt::getVectorShape(llvm::Value & val) const {
  if (vecInfo.hasKnownShape(val)) return vecInfo.getVectorShape(val);
  else if (isa<Constant>(val)) return VectorShape::uni();
  else return VectorShape::undef();
}

StructOpt::StructOpt(VectorizationInfo & _vecInfo, const DataLayout & _layout)
: vecInfo(_vecInfo)
, layout(_layout)
, numTransformed(0)
, numPromoted(0)
{}

Value *
StructOpt::transformLoadStore(IRBuilder<> & builder,
                              bool replaceInst,
                              Instruction * inst,
                              Type * scalarTy,
                              Value * vecPtrVal,
                              Value * storeVal) {
  auto * load = dyn_cast<LoadInst>(inst);
  auto * store = dyn_cast<StoreInst>(inst);

  if (scalarTy->isStructTy()) {
    // emit multiple loads/stores
    // loads must re-insert the smaller vector loads in the structure
    Value * structVal = nullptr;
    if (load) {
      structVal = UndefValue::get(scalarTy);
      vecInfo.setVectorShape(*structVal, vecInfo.getVectorShape(*load));
    }

    for (size_t i = 0; i < scalarTy->getStructNumElements(); ++i) {
      auto * vecGEP = builder.CreateGEP(vecPtrVal, { builder.getInt32(0), builder.getInt32(i) });
      auto * structElem = storeVal ? builder.CreateExtractValue(storeVal, i) : nullptr;
      auto * elemTy = scalarTy->getStructElementType(i);

      if (storeVal) vecInfo.setVectorShape(*structElem, vecInfo.getVectorShape(*storeVal));
      vecInfo.setVectorShape(*vecGEP, vecInfo.getVectorShape(*vecPtrVal));

      auto * vecElem = transformLoadStore(builder, false, inst, elemTy, vecGEP, structElem);
      if (structVal) {
        structVal = builder.CreateInsertValue(structVal, vecElem, i);
        vecInfo.setVectorShape(*structVal, vecInfo.getVectorShape(*load));
      }
    }

    if (replaceInst) {
      if (load) load->replaceAllUsesWith(structVal);
      else      vecInfo.dropVectorShape(*store);
    }
    return structVal;
  }

  // cast *<8 x float> to * float
  auto * vecElemTy = cast<VectorType>(vecPtrVal->getType()->getPointerElementType());
  auto * plainElemTy = vecElemTy->getElementType();

  auto * castElemTy = builder.CreatePointerCast(vecPtrVal, PointerType::getUnqual(plainElemTy));

  const uint alignment = layout.getTypeStoreSize(plainElemTy) * vecInfo.getVectorWidth();
  vecInfo.setVectorShape(*castElemTy, VectorShape::cont(alignment));

  if (load)  {
    auto * vecLoad = builder.CreateLoad(castElemTy, load->getName());
    vecInfo.setVectorShape(*vecLoad, vecInfo.getVectorShape(*load));
    vecLoad->setAlignment(alignment);

    if (replaceInst) load->replaceAllUsesWith(vecLoad);

    IF_DEBUG_SO { errs() << "\t\t result: " << *vecLoad << "\n"; }
    return vecLoad;
  } else {
    auto * vecStore = builder.CreateStore(storeVal, castElemTy, store->isVolatile());
    vecStore->setAlignment(alignment);
    vecInfo.setVectorShape(*vecStore, vecInfo.getVectorShape(*store));

    if (replaceInst) vecInfo.dropVectorShape(*store);

    IF_DEBUG_SO { errs() << "\t\t result: " << *vecStore << "\n"; }
    return nullptr;
  }
}

void
StructOpt::transformLayout(llvm::AllocaInst & allocaInst, ValueToValueMapTy & transformMap) {
  SmallSet<Value*, 16> seen;
  std::vector<Instruction*> allocaUsers;
  allocaUsers.push_back(&allocaInst);
  SmallSet<PHINode*,4> postProcessPhis;

  IF_DEBUG_SO { errs() << "-- transforming: " << allocaInst << "\n"; }
  while (!allocaUsers.empty()) {
    auto * inst = allocaUsers.back();
    allocaUsers.pop_back();

  // have seen this user
    if (!seen.insert(inst).second) continue;

    // auto * allocaInst = dyn_cast<AllocaInst>(inst);
    auto * store = dyn_cast<StoreInst>(inst);
    auto * load = dyn_cast<LoadInst>(inst);
    auto * gep = dyn_cast<GetElementPtrInst>(inst);
    auto * phi = dyn_cast<PHINode>(inst);

  // transform this (final) gep
    if (load || store) {
      IF_DEBUG_SO { errs() << "\t- transform load/store " << *inst << "\n"; }
      IRBuilder<> builder(inst->getParent(), inst->getIterator());

      auto * ptrVal = load ? load->getPointerOperand() : store->getPointerOperand();
      auto * storeVal = store ? store->getValueOperand() : nullptr;

      assert (transformMap.count(ptrVal));
      Value * vecPtrVal = transformMap[ptrVal];

      transformLoadStore(builder, true, inst, ptrVal->getType()->getPointerElementType(), vecPtrVal, storeVal);

      continue; // don't step across load/store

    } else if (gep) {
      IF_DEBUG_SO { errs() << "\t- transform gep " << *gep << "\n"; }
      auto vecBasePtr = transformMap[gep->getOperand(0)];

      std::vector<Value*> indexVec;
      for (size_t i = 1; i < gep->getNumOperands(); ++i) {
        indexVec.push_back(gep->getOperand(i));
      }

      auto * vecGep = GetElementPtrInst::Create(nullptr, vecBasePtr, indexVec, gep->getName(), gep);

      vecInfo.setVectorShape(*vecGep, VectorShape::uni());

      IF_DEBUG_SO { errs() << "\t\t result: " << *vecGep << "\n\t T:" << *vecGep->getType() << "\n"; }
      transformMap[gep] = vecGep;

    } else if (phi) {
      IF_DEBUG_SO { errs() << "\t- transform phi " << *phi << "\n"; }
      postProcessPhis.insert(phi);
      auto * orgPhiTy = phi->getIncomingValue(0)->getType();
      auto * vecPhiTy = PointerType::get(vectorizeType(*orgPhiTy->getPointerElementType()), orgPhiTy->getPointerAddressSpace());
      auto * vecPhi = PHINode::Create(vecPhiTy, phi->getNumIncomingValues(), phi->getName(), phi);
      IF_DEBUG_SO { errs() << "\t\t result: " << *vecPhi << "\n"; }

      vecInfo.setVectorShape(*vecPhi, VectorShape::uni());
      vecInfo.dropVectorShape(*phi);
      transformMap[phi] = vecPhi;

    // important: this case has to become *before* the CastInst case (lifetime pattern is more general)
    } else if (IsLifetimeUse(*inst)) {
      // remap BC operand
      inst->replaceUsesOfWith(&allocaInst, transformMap[&allocaInst]);
      continue; // skip lifetime/BC users

    } else if (IsLoadStoreIntrinsicUse(inst)) {
      for (size_t i = 0, n = inst->getNumOperands(); i < n; ++i) {
        if (transformMap.count(inst->getOperand(i)) != 0)
          inst->setOperand(i, transformMap[inst->getOperand(i)]);
      }

      RemapLoadStoreIntrinsicShape(inst, vecInfo);
      continue;

    } else {
      assert(isa<AllocaInst>(inst) && "unexpected instruction in alloca transformation");
    }

  // update users users
    for (auto user : inst->users()) {
      auto * userInst = dyn_cast<Instruction>(user);

      if (!userInst) continue;

      allocaUsers.push_back(userInst);
    }
  }

// repair phi nodes
  for (auto * phi : postProcessPhis) {
    auto * vecPhi = cast<PHINode>(transformMap[phi]);
    for (size_t i = 0; i < phi->getNumIncomingValues(); ++i) {
      vecPhi->addIncoming(transformMap[phi->getIncomingValue(i)], phi->getIncomingBlock(i));
    }
  }

// remove old code
  for (auto deadVal : seen) {
    auto *deadInst = cast<Instruction>(deadVal);
    assert(deadInst);
    // keep the load/store intrinsics
    if (IsLoadStoreIntrinsicUse(deadInst)) continue;

    if (!deadInst->getType()->isVoidTy()) {
      deadInst->replaceAllUsesWith(UndefValue::get(deadInst->getType()));
    }
    // the old alloca should be obsolete now
    vecInfo.dropVectorShape(*deadInst);
    deadInst->eraseFromParent();
  }

  transformMap.clear();
}

static bool VectorizableType(Type & type) {
  if (type.isStructTy()) {
    for (size_t i = 0; i < type.getStructNumElements(); ++i) {
      if (!VectorizableType(*type.getStructElementType(i))) return false;
    }
    return true;
  }
  return type.isIntegerTy() || type.isFloatingPointTy();
}

/// whether any address computation on this alloc is uniform
/// the alloca can still be varying because of stored varying values
bool
StructOpt::allUniformGeps(llvm::AllocaInst & allocaInst) {
  SmallSet<Value*, 16> seen;
  std::vector<Instruction*> allocaUsers;
  allocaUsers.push_back(&allocaInst);

  while (!allocaUsers.empty()) {
    auto * inst = allocaUsers.back();
    allocaUsers.pop_back();

  // have seen this user
    if (!seen.insert(inst).second) continue;

  // dont touch this alloca if its used on the outside (unless its the alloca itself)
    if (&allocaInst != inst && !vecInfo.inRegion(*inst)) {
      IF_DEBUG_SO { errs() << "skip: has user outside of region: " << *inst << "\n";  }
      return false;
    }

  // inspect users
    for (auto user : inst->users()) {
      auto * userInst = dyn_cast<Instruction>(user);
      IF_DEBUG_SO { errs() << "inspecting user " << *userInst << "\n"; }

      if (!userInst) continue;

      if (isa<GetElementPtrInst>(userInst)) allocaUsers.push_back(userInst);
      //
      // skip if the an alloca derived value is stored somewhere
      else if (isa<StoreInst>(userInst)) {
        if (userInst->getOperand(0) == inst) { // leaking the value!
          return false;
        }
        if (!VectorizableType(*cast<StoreInst>(userInst)->getValueOperand()->getType())) {
          IF_DEBUG_SO { errs() << "skip: accessing non-leaf element : " << *userInst << "\n"; }
          return false;
        }
      }

      // we dont care about (indirect) alloc loads
      else if (isa<LoadInst>(userInst)) {
        if (!VectorizableType(*cast<LoadInst>(userInst)->getType())) {
          IF_DEBUG_SO { errs() << "skip: accessing non-leaf element : " << *userInst << "\n"; }
          return false;
        }
        continue;
      }

      // use by lifetime.start/end marker / intrinsics
      else if (IsLifetimeUse(*userInst) || IsLoadStoreIntrinsicUse(userInst)) continue;

      // see through bitcasts
      else if (isa<CastInst>(userInst)) {
        IF_DEBUG_SO { errs() << "\t cast transition (restricted patterns apply):\n"; }
        if (!userInst->getType()->isPointerTy()) {
            IF_DEBUG_SO { errs() << "skip: casting alloca-derived pointer to int : " << *userInst << "\n"; }
          return false;
        }

        bool needCompatibleType = false;
        // TODO only accept a BC+store pattern (since we are here, the GEP itself seems to be valie)
        for (auto & bcUse : userInst->uses()) {
          auto * subInst = dyn_cast<Instruction>(bcUse.getUser());
          IF_DEBUG_SO { errs() << "sub use: " << *subInst << "\n"; }
          if (isa<StoreInst>(subInst)) {
            IF_DEBUG_SO { errs() << "sub store!\n"; }
            needCompatibleType = true;
            if (bcUse.getOperandNo() != 1) { // leaking the value!
              IF_DEBUG_SO { errs() << "skip: (BC guarded use) store leaks value: " << *subInst << "\n";  }
              return false;
            }
          }
          else if (isa<LoadInst>(subInst)) { IF_DEBUG_SO { errs() << "sub load!\n"; } needCompatibleType = true; continue; }
          else if (IsLifetimeUse(*subInst)) { IF_DEBUG_SO { errs() << "sub lifetime use!\n"; } continue; }
          else if (IsLoadStoreIntrinsicUse(subInst)) { IF_DEBUG_SO { errs() << "sub load/store intrinsic use!\n"; } needCompatibleType = true; continue; }
          else {
            IF_DEBUG_SO { errs() << "skip: (BC guarded use) will not accept other uses than loads and stores : " << *subInst << "\n"; }
            return false;
          }
        }

        // if the pointer is used to access data make sure that the store size is identical
        if (needCompatibleType) {
          if (layout.getTypeAllocSize(userInst->getType()->getPointerElementType()) != layout.getTypeAllocSize(inst->getType()->getPointerElementType())) {
            IF_DEBUG_SO { errs() << "skip: casting to non-aligned type (that is accessed) : " << *userInst << "\n"; }
            return false;
          }
        }
      }

      // skip unforeseen users
      else if (!isa<PHINode>(userInst)) return false;

      // otw, descend into user
      allocaUsers.push_back(userInst);
    }

  // verify uniform address criterion
    auto * gep = dyn_cast<GetElementPtrInst>(inst);
    if (gep) {
      // check whether all gep operands are uniform (except the baseptr)
      for (size_t i = 1; i < gep->getNumOperands(); ++i) {
        if (!getVectorShape(*gep->getOperand(i)).isUniform()) {
          IF_DEBUG_SO { errs() << "skip: non uniform gep: " << *gep << " at index " << i << " : " << *gep->getOperand(i) << "\n"; }
          return false;
        }
      }
    }
  }

// TODO verify that no unseen value sneaks in through phis
  for (auto * allocaUser : seen) {
    auto * phi = dyn_cast<PHINode>(allocaUser);
    if (!phi) continue;

    for (size_t i = 0; i < phi->getNumIncomingValues(); ++i) {
      auto * inVal = phi->getIncomingValue(i);
      if (isa<Instruction>(inVal) && !seen.count(inVal)) {
        IF_DEBUG_SO { errs() << "skip: alloca mixes with non-derived value " << *inVal << " at phi " << *phi << "\n"; }
        return false;
      }
    }
  }

  return true;
}

bool
StructOpt::shouldPromote(llvm::AllocaInst & allocaInst) {
  if (!IsDecomposable(*allocaInst.getType()->getPointerElementType())) return false;

// check that the alloca is only ever accessed as a whole (no GEPs)
  for (auto & use : allocaInst.uses()) {
    auto * inst = dyn_cast<Instruction>(use.getUser());
    if (!inst) continue;
    auto * load = dyn_cast<LoadInst>(inst);
    auto * store = dyn_cast<StoreInst>(inst);
    if (!load && !store && !IsLifetimeUse(*inst)) {
      IF_DEBUG_SO { errs() << "\t non-load/store user " << *inst << " -> can not promote\n";}
      return false;
    }
    if (store) {
      if (store->getValueOperand() == &allocaInst) {
        IF_DEBUG_SO { errs() << "\t alloca value stored away -> can not promote\n"; }
        return false;
      }
    }
  }

  return true;
}

void
StructOpt::promoteAlloca(llvm::AllocaInst & allocaInst) {
  SmallVector<PHINode*, 8> phiVec;

  SSAUpdater ssaUpdater(&phiVec);
  ssaUpdater.Initialize(allocaInst.getType()->getPointerElementType(), allocaInst.getName());

  SmallVector<Instruction*, 8> instVec;
  LoadAndStorePromoter promoter(instVec, ssaUpdater, allocaInst.getName());

  for (auto & use : allocaInst.uses()) {
    instVec.push_back(cast<Instruction>(use.getUser()));
  }

  promoter.run(instVec);

  // TODO set shapes
  for (auto * phi : phiVec) {
    vecInfo.setVectorShape(*phi, VectorShape::varying());
  }
}

/// try to optimize the layout of this alloca
bool
StructOpt::optimizeAlloca(llvm::AllocaInst & allocaInst) {
  IF_DEBUG_SO {errs() << "\n# trying to optimize Alloca " << allocaInst << "\n"; }

  // legality checks
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

  // TODO check if this value should be promoted to a value
  if (shouldPromote(allocaInst)) {
    promoteAlloca(allocaInst);
    IF_DEBUG_SO { errs() << "\t promoted!\n"; }
    numPromoted++;
    return true;
  }

  IF_DEBUG_SO { errs() << "vectorized type: " << *vecAllocTy << "\n"; }
  //
  // this alloca may only be:
  // loaded from, stored to (must note store the pointer) use to derive addresses (with uniform indicies) or passsed through phi nodes
  if (!allUniformGeps(allocaInst)) return false;
  IF_DEBUG_SO { errs() << "vectorizable uses!\n"; }


// we may transorm the alloc

  // replace alloca
  auto * vecAlloc = new AllocaInst(vecAllocTy, allocaInst.getType()->getAddressSpace(), allocaInst.getName(), &allocaInst);

  const uint alignment = layout.getPrefTypeAlignment(vecAllocTy); // TODO should enfore a stricter alignment at this point
  vecInfo.setVectorShape(*vecAlloc, VectorShape::uni(alignment));

  ValueToValueMapTy transformMap;

  transformMap[&allocaInst] = vecAlloc;

// update all gep/phi shapes
  transformLayout(allocaInst, transformMap);

  numTransformed++;

  return true;
}

llvm::Type *
StructOpt::vectorizeType(llvm::Type & scalarAllocaTy) {
// primite type -> vector
  if (scalarAllocaTy.isIntegerTy() ||
      scalarAllocaTy.isFloatingPointTy())
    return VectorType::get(&scalarAllocaTy, vecInfo.getVectorWidth());

// finite aggrgate -> aggrgate of vectorized elemnts
  if (scalarAllocaTy.isStructTy()) {
    std::vector<Type*> elemTyVec;
    for (size_t i = 0; i < scalarAllocaTy.getStructNumElements(); ++i) {
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
StructOpt::run() {
  IF_DEBUG_SO { errs() << "-- struct opt log --\n"; }

  numTransformed = 0;
  numPromoted = 0;

  std::vector<AllocaInst*> queue;
  for (auto & bb : vecInfo.getScalarFunction()) {
    auto itBegin = bb.begin(), itEnd = bb.end();
    for (auto it = itBegin; it != itEnd; ) {
      auto * allocaInst = dyn_cast<AllocaInst>(it++);
      if (allocaInst) queue.push_back(allocaInst);
    }
  }

  bool change = false;
  for (auto allocaInst : queue) {
    change |= optimizeAlloca(*allocaInst);
  }

  if (numTransformed > 0) {
    Report() << "structOpt: transformed " << numTransformed << " allocas to struct-of-vector layout\n";
  }
  if (numPromoted > 0) {
    Report() << "structOpt: promoted " << numPromoted << " allocas to values\n";
  }

  IF_DEBUG_SO { errs() << "-- end of struct opt log --\n"; }

  return change;
}


} // namespace rv
