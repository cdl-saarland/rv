#include "rv/transform/srovTransform.h"

#include <vector>
#include <sstream>

#include <llvm/IR/Function.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/CFG.h>

#include <llvm/ADT/SmallSet.h>
#include <llvm/ADT/PostOrderIterator.h>

#include <llvm/Transforms/Utils/ValueMapper.h>

#include "rv/vectorizationInfo.h"
#include "rv/PlatformInfo.h"

#include <rvConfig.h>
#include "report.h"

using namespace rv;
using namespace llvm;

#if 1
#define IF_DEBUG_SROV IF_DEBUG
#else
#define IF_DEBUG_SROV if (true)
#endif

// InsertElementInst operand accessors missing in LLVM 4.0
static Value*
GetVectorOperand(InsertElementInst & insertInst) {
  return insertInst.getOperand(0);
}

static Value*
GetElemOperand(InsertElementInst & insertInst) {
  return insertInst.getOperand(1);
}

static Value*
GetIndexOperand(InsertElementInst & insertInst) {
  return insertInst.getOperand(2);
}

namespace rv {

typedef std::vector<llvm::Value*> ValVec;
typedef std::map<llvm::Value*, ValVec> MultiValMap;
typedef std::vector<Type*> TypeVec;

struct
ReplicateMap {
  VectorizationInfo & vecInfo;

  // maps original values to scalar replicates
  MultiValMap replMap;

  bool hasReplicate(llvm::Value& val) {
    return replMap.count(&val);
  }

  void addReplicate(Value & val, ValVec scalarRepls) {
    auto aggregateShape = vecInfo.getVectorShape(val);
    for (auto * val : scalarRepls) {
      auto * replInst = dyn_cast<Instruction>(val);
      if (!replInst) continue;
      vecInfo.setVectorShape(*replInst, aggregateShape);
    }

    replMap[&val] = scalarRepls;
  }

  ValVec getReplVec(Value & val) { return replMap[&val]; }

  void clear() { replMap.clear(); }

  size_t size() const { return replMap.size(); }

  ReplicateMap(VectorizationInfo & _vecInfo)
  : vecInfo(_vecInfo)
  {}
};



// Impl - implementation class
struct Impl {
  Function & F;
  VectorizationInfo & vecInfo;
  const PlatformInfo & platInfo;

  // maps aggregates to their scalar replications
  ReplicateMap replMap;

  // replicated instructions that must not be stripped from the code
  SmallSet<Value*, 32> keepSet;
Impl(Function & _F, VectorizationInfo & _vecInfo, const PlatformInfo & _platInfo)
: F(_F)
, vecInfo(_vecInfo)
, platInfo(_platInfo)
, replMap(_vecInfo)
{}

// all instructions are mapped
// remap phi inputs
void
repairPhis() {
  for (auto itMapped : replMap.replMap) {
    auto * val = itMapped.first;
    auto * oldPhi = dyn_cast<PHINode>(val);
    if (!oldPhi) continue;

    ValVec phiRepls = itMapped.second;

  // fix up incoming values of PHI nodes
    // for every incoming edge
    for (size_t i = 0; i < oldPhi->getNumIncomingValues(); ++i) {
      auto * oldIncoming = oldPhi->getIncomingValue(i);

      ValVec inRepl = requestReplicate(*oldIncoming);

      // for every replicated slot
      for (size_t j = 0; j < phiRepls.size(); ++j) {
        auto & replPhi = cast<PHINode>(*phiRepls[j]);
        replPhi.addIncoming(inRepl[j], oldPhi->getIncomingBlock(i));
      }
    }
  }
}

// check whether all uses of this will-be-replicated instruction can be recovered from the scalare replicates
bool
canRepairUses(Value & val) {
  // allow SIMD operations on this type
  if (isa<VectorType>(val.getType())) {
    return true;
  }

  // can always repair constants
  if (isa<Constant>(val)) return true;

  if (isa<ExtractValueInst>(val)) return true;

  for (auto * user : val.users()) {
    if (!isa<PHINode>(user) && !isa<ExtractValueInst>(user) && !isa<SelectInst>(user) && !isa<InsertValueInst>(user) && !isa<InsertElementInst>(user) && !isa<ExtractElementInst>(user)) {
      IF_DEBUG_SROV { errs() << "can not repair use: " << *user << "\n"; }
      return false;
    }
  }

  return true;
}

typedef SmallSet<const Value*, 32> ConstValSet;

bool
canReplicate(llvm::Value & val, ConstValSet & checkedSet) {
  auto * constVal = dyn_cast<Constant>(&val);
  auto * selInst = dyn_cast<SelectInst>(&val);
  auto * phiInst = dyn_cast<PHINode>(&val);
  auto * insertValInst = dyn_cast<InsertValueInst>(&val);
  auto * insertElemInst = dyn_cast<InsertElementInst>(&val);

  // remark: checkedSet makes every value appear replicable on the second query
  // However, a single negative reponse for a recursive constituent makes canReplicate fail anyway
  if (!checkedSet.insert(&val).second) return true;

  // value was already replicated
  if (replMap.hasReplicate(val)) return true;

  // check whether we could repair all uses
  if (!canRepairUses(val)) return false;

  // check whether the instruction itself is replicatable
  if (constVal) return true;
  if (phiInst) {
    for (size_t i = 0; i < phiInst->getNumIncomingValues(); ++i) {
      if (!canReplicate(*phiInst->getIncomingValue(i), checkedSet)) {
        return false;
      }
    }
    return true;

  } else if (selInst) {
    return canReplicate(*selInst->getTrueValue(), checkedSet) && canReplicate(*selInst->getFalseValue(), checkedSet);

  } else if (insertValInst) {
    return canReplicate(*insertValInst->getAggregateOperand(), checkedSet);
  } else if (insertElemInst) {
    auto * vecOp = insertElemInst->getOperand(0);
    return canReplicate(*vecOp, checkedSet);
  } else if (isa<ExtractValueInst>(val)) {
    return true;
  } else if (isa<LoadInst>(val) || isa<StoreInst>(val)) {
    return true;
  } else if (isa<VectorType>(val.getType())) {
    return cast<Instruction>(val).isBinaryOp(); // allow replication of binary SIMD operators
  }

  return false;
}

Value * reaggregateInstruction(IRBuilder<> & builder, const ValVec & replVec, Type * aggTy, const VectorShape & vecShape, size_t & index) {
  if (aggTy->isStructTy()) {
    size_t n = aggTy->getStructNumElements();
    Value * aggVal = UndefValue::get(aggTy);
    vecInfo.setVectorShape(*aggVal, vecShape);
    for (size_t i = 0; i < n; i++) {
      auto elemVal = reaggregateInstruction(builder, replVec, aggTy->getStructElementType(i), vecShape, index);
      aggVal = builder.CreateInsertValue(aggVal, elemVal, i);
      vecInfo.setVectorShape(*aggVal, vecShape);
    }
    return aggVal;
  } else if (aggTy->isVectorTy()) {
    Value * aggVal = UndefValue::get(aggTy);
    vecInfo.setVectorShape(*aggVal, vecShape);
    size_t n = aggTy->getVectorNumElements();
    for (size_t i = 0; i < n; i++) {
      aggVal = builder.CreateInsertElement(aggVal, replVec[i], ConstantInt::get(Type::getInt32Ty(builder.getContext()), i));
      vecInfo.setVectorShape(*aggVal, vecShape);
    }
    return aggVal;

  } else {
    return replVec[index++];
  }
}

// re-aggregate @inst for external users of that value
void
repairExternalUses(Instruction & inst) {
  // no need to re-aggregate instructions with only one replicate
  auto replVec = replMap.getReplVec(inst);
  if (replVec.size() == 1) {
    inst.replaceAllUsesWith(replVec[0]);
    return;
  }

  IRBuilder<> builder(inst.getParent(), inst.getIterator());

  for (auto itUse = inst.use_begin(); itUse != inst.use_end(); ) {
    auto & use = *itUse++;
    auto * userInst = cast<Instruction>(use.getUser());

    // this use has been replicated
    if (replMap.hasReplicate(*userInst)) continue;

    // remap extractvalue inst to the scalar replicate
    auto * extractInst = isa<ExtractValueInst>(userInst) || isa<ExtractElementInst>(userInst) ? userInst : nullptr;

    if (extractInst) {
      ValVec aggRepl = requestReplicate(*extractInst);

      assert((GetNumReplicates(*extractInst->getType()) == 1) && "re-aggregation for non-replicated users not yet implemented");
      auto * elemVal = aggRepl[0];

      extractInst->replaceAllUsesWith(elemVal);
      continue;
    }

    size_t startIndex = 0;
    auto * aggVal = reaggregateInstruction(builder, replVec, inst.getType(), vecInfo.getVectorShape(inst), startIndex);
    inst.replaceAllUsesWith(aggVal);
  }
}

void
finalize() {
  // attach inputs to replicated PHI nodes
  repairPhis();

  // re-aggregate replicated values for external users
  for (auto itMapped : replMap.replMap) {
    if (keepSet.count(itMapped.first)) continue;

    auto * inst = dyn_cast<Instruction>(itMapped.first);
    if (!inst) continue;

    if (!inst->use_empty()) {
      // re-aggregate for external (non replicated) users
      repairExternalUses(*inst);
      // replace now dead code uses with undef
      inst->replaceAllUsesWith(UndefValue::get(inst->getType()));
    }

    assert(inst->use_empty() && "could not remap all uses");

    // all uses patched -> erase
    vecInfo.dropVectorShape(*inst);
    inst->eraseFromParent();
  }

  // drop replication info
  replMap.clear();
  keepSet.clear();
}


Type&
getLaneReplType(Type & type, int i) {
  return *type.getStructElementType(i);
}

template<typename T>
static
void
Append(std::vector<T> & dest, const std::vector<T> & src) {
  for (auto e : src) dest.push_back(e);
}

void
replicateType(Type & type, TypeVec & oTypes) {
  auto * structTy = dyn_cast<StructType>(&type);
  auto * arrTy = dyn_cast<ArrayType>(&type);
  auto * vecTy = dyn_cast<VectorType>(&type);

  if (structTy) {
    size_t numElems = structTy->getNumElements();
    oTypes.reserve(oTypes.size() + numElems);
    for (size_t i = 0; i < numElems; ++i) {
      replicateType(*structTy->getStructElementType(i), oTypes);
    }
  } else if (arrTy) {
    TypeVec elemTyVec;
    auto * elemTy = arrTy->getElementType();
    replicateType(*elemTy, elemTyVec);
    size_t numElems = arrTy->getNumElements();
    oTypes.reserve(oTypes.size() + elemTyVec.size() * numElems);

    for (size_t i = 0; i < numElems; ++i) {
      Append(oTypes, elemTyVec);
    }
  } else if (vecTy) {
    auto * elemTy = vecTy->getElementType();
    size_t numElems = vecTy->getNumElements();
    oTypes.reserve(oTypes.size() + numElems);
    for (size_t i = 0; i < numElems; ++i) {
      oTypes.push_back(elemTy);
    }

  } else {
    oTypes.push_back(&type);
  }
}

static
const Type&
GetAggregateOperand(const Type & type, size_t i) {
  auto * structTy = dyn_cast<StructType>(&type);
  auto * arrTy = dyn_cast<ArrayType>(&type);
  auto * vecTy = dyn_cast<VectorType>(&type);
  if (structTy) {
    return *structTy->getElementType(i);
  } else if (arrTy) {
    return *arrTy->getElementType();
  } else if (vecTy) {
    return *vecTy->getElementType();
  } else {
    abort();
  }
}

static
size_t
GetElementOffset(const Type & aggType, ArrayRef<uint> indices) {
  if (indices.size() == 0) {
    return 0;
  }

  size_t offset = 0;
  size_t aggOffset = indices.front();
  auto tailIndices = indices.drop_front(1);

  for (size_t i = 0; i < aggOffset; ++i) {
    offset += GetNumReplicates(GetAggregateOperand(aggType, i));
  }

  offset += GetElementOffset(GetAggregateOperand(aggType, aggOffset), tailIndices);
  return offset;
}

// returns the number of scalar values that can represent this type
// return 0 if such a representation does not exist
static
size_t
GetNumReplicates(const Type & type) {
  // scalar value
  if (type.isIntegerTy() || type.isFloatingPointTy()) return 1;

  auto * structTy = dyn_cast<const StructType>(&type);
  auto * arrTy = dyn_cast<const ArrayType>(&type);
  auto * vecTy = dyn_cast<const VectorType>(&type);

// check if this struct is composed entirely of int/bool or fp types
  int flatSize = 0;

  if (structTy) {
    // aggregate flattened element sizes
    for (size_t i = 0; i < structTy->getNumElements(); ++i) {
      auto & elemTy = *structTy->getElementType(i);
      auto flatElemSize = GetNumReplicates(elemTy);
      if (flatElemSize == 0) return 0; // can not replicate element
      flatSize += flatElemSize;
    }

  } else if (arrTy) {
    auto & elemTy = *arrTy->getArrayElementType();
    auto flatElemSize = GetNumReplicates(elemTy);
    if (flatElemSize == 0) return 0;
    flatSize = flatElemSize * arrTy->getArrayNumElements();

  } else if (vecTy) {
    // auto & elemTy = *vecTy->getArrayElementType();
    // auto flatElemSize = GetNumReplicates(elemTy);
    // if (flatElemSize == 0) return 0;
    flatSize = vecTy->getNumElements();
  }

// passed all tests
  return flatSize;
}

size_t flattenedLoadStore(IRBuilder<> & builder, Value * ptr, ValVec & replVec, size_t flatIdx, LoadInst * load, StoreInst * store) {
  auto ptrShape = vecInfo.getVectorShape(*ptr);
  auto * ptrElemTy = ptr->getType()->getPointerElementType();

  if (ptrElemTy->isStructTy()) {
    auto * intTy = Type::getInt32Ty(builder.getContext());
    size_t n = ptrElemTy->getStructNumElements();
    for (size_t i = 0; i < n; i++) {
      // load every member
      auto * elemGep = builder.CreateGEP(ptr, {ConstantInt::get(intTy, 0, true), ConstantInt::get(intTy, i, true)}, "srov_gep");
      vecInfo.setVectorShape(*elemGep, ptrShape); // FIXME alignment
      flatIdx = flattenedLoadStore(builder, elemGep, replVec, flatIdx, load, store);
    }
    return flatIdx;
  } else {
    // not a structure, just perform a normal load/store
    if (load) {
      // for a load, replVec will be filled with the loaded data
      assert(replVec.size() == flatIdx);
      auto * flatLoad = builder.CreateLoad(ptr, load->isVolatile());
      vecInfo.setVectorShape(*flatLoad, vecInfo.getVectorShape(*load));
      replVec.push_back(flatLoad);
    }
    if (store) {
      // for a store, replVec contains the replicated data to store
      assert(replVec.size() > flatIdx);
      auto * flatStore = builder.CreateStore(replVec[flatIdx], ptr, store->isVolatile());
      vecInfo.setVectorShape(*flatStore, vecInfo.getVectorShape(*store));
    }
    return flatIdx + 1;
  }
}

static
int GetShuffleIndex(Constant & shuffleMask, int i) {
  if (shuffleMask.isZeroValue()) return 0;
  auto & maskVec = cast<ConstantVector>(shuffleMask);
  return cast<ConstantInt>(maskVec.getOperand(i))->getSExtValue();
}


ValVec
requestInstructionReplicate(Instruction & inst, TypeVec & replTyVec) {
  ValVec replVec;

  auto * phi = dyn_cast<PHINode>(&inst);
  IRBuilder<> builder(inst.getParent(), inst.getIterator());
  std::string oldInstName = inst.getName().str();

  auto * vecTy = dyn_cast<VectorType>(inst.getType());

// phi replication logic (attach inputs later)
  if (phi) {
    for (size_t i = 0; i < replTyVec.size(); ++i) {
      std::stringstream ss;
      ss << oldInstName << ".repl." << i;
      std::string phiReplName = ss.str();

      auto * replPhi = builder.CreatePHI(replTyVec[i], phi->getNumIncomingValues(), phiReplName);
      replVec.push_back(replPhi);
    }

    // register PHI node as replicated
    replMap.addReplicate(*phi, replVec);

    // request operands
    for (size_t i = 0; i < phi->getNumIncomingValues(); ++i) {
      auto * inVal = phi->getIncomingValue(i);

      // descend into non-trivial operand instructions
      if (isa<Instruction>(inVal)) {// && !isa<InsertValueInst>(inVal)) {
        requestReplicate(*inVal);
      }
    }

    // phi node already registered
    return replVec;
// generic instruction replication
  } else if (isa<StoreInst>(inst) || isa<LoadInst>(inst)) {
    auto * load = dyn_cast<LoadInst>(&inst);
    auto * store = dyn_cast<StoreInst>(&inst);
    auto * ptr = load ? load->getPointerOperand() : store->getPointerOperand();
    if (store) replVec = requestReplicate(*store->getValueOperand());
    flattenedLoadStore(builder, ptr, replVec, 0, load, store);
  } else if (isa<SelectInst>(inst)) {
    auto & selectInst = cast<SelectInst>(inst);
    auto & selMask = *selectInst.getOperand(0);
    auto & selTrue = *selectInst.getOperand(1);
    auto & selFalse = *selectInst.getOperand(2);

    auto replTrueVec = requestReplicate(selTrue);
    auto replFalseVec = requestReplicate(selFalse);

    for (size_t i = 0; i < replTyVec.size(); ++i) {
      std::stringstream ss;
      ss << oldInstName << ".repl." << i;
      auto * replSelect = builder.CreateSelect(&selMask, replTrueVec[i], replFalseVec[i], ss.str());
      replVec.push_back(replSelect);
    }

  } else if (isa<InsertValueInst>(inst)) {
    auto & insertInst = cast<InsertValueInst>(inst);
    auto & aggVal = *insertInst.getAggregateOperand();
    auto & elemVal = *insertInst.getOperand(1);
    replVec = requestReplicate(aggVal);
    ValVec elemRepl = requestReplicate(elemVal);

    size_t elemStart = GetElementOffset(*aggVal.getType(), insertInst.getIndices());

    assert((elemStart + elemRepl.size() <= replVec.size()) && "out of bounds");
    for (size_t i = 0; i < elemRepl.size(); ++i) {
      replVec[elemStart + i] = elemRepl[i];
    }

  } else if (isa<InsertElementInst>(inst)) {
    auto & insertInst = cast<InsertElementInst>(inst);
    auto & vecVal = *GetVectorOperand(insertInst);
    auto & elemVal = *GetElemOperand(insertInst);
    auto & idxOp = *GetIndexOperand(insertInst);

    replVec = requestReplicate(vecVal);

    size_t elemStart = cast<ConstantInt>(idxOp).getSExtValue();
    replVec[elemStart] = &elemVal;

  } else if (isa<ExtractValueInst>(inst)) {
    auto & extInst = cast<ExtractValueInst>(inst);
    auto & aggVal = *extInst.getAggregateOperand();
    size_t start = GetElementOffset(*aggVal.getType(), extInst.getIndices());

    size_t numRepls = GetNumReplicates(*extInst.getType());
    ValVec aggRepl = requestReplicate(aggVal);

    assert((start + numRepls <= aggRepl.size()) && "OOB");
    for (size_t i = 0; i < numRepls; ++i) {
      replVec.push_back(aggRepl[start + i]);
    }

  } else if (isa<ExtractElementInst>(inst)) {
    auto & extInst = cast<ExtractElementInst>(inst);
    auto & vecVal = *extInst.getVectorOperand();
    int idx = cast<ConstantInt>(extInst.getIndexOperand())->getSExtValue();
    size_t start = GetElementOffset(*vecVal.getType(), idx);

    size_t numRepls = GetNumReplicates(*extInst.getType());
    ValVec aggRepl = requestReplicate(vecVal);

    assert((start + numRepls <= aggRepl.size()) && "OOB");
    for (size_t i = 0; i < numRepls; ++i) {
      replVec.push_back(aggRepl[start + i]);
    }
  } else if (isa<ShuffleVectorInst>(inst)) {
    const int width = vecTy->getNumElements();

    auto & shuffle = cast<ShuffleVectorInst>(inst);
    ValVec lhsVec = requestReplicate(*shuffle.getOperand(0));
    ValVec rhsVec = requestReplicate(*shuffle.getOperand(1));

    auto & shuffleMask = *shuffle.getMask();
    for (int c = 0; c < width; ++c) {
      int laneIdx = GetShuffleIndex(shuffleMask, c);
      Value * laneRepl;
      if (laneIdx >= 0) {
         laneRepl = laneIdx < width ? lhsVec[laneIdx] : rhsVec[laneIdx - width];
      } else {
        laneRepl = UndefValue::get(vecTy->getElementType());
      }
      replVec.push_back(laneRepl);
    }

  } else if (inst.isBinaryOp() && vecTy) {
    auto * flatTy = vecTy->getElementType();

    std::vector<Instruction*> repls;
    for (int c = 0; c < vecTy->getNumElements(); ++c) {
      auto * cloned = inst.clone();
      cloned->mutateType(flatTy);
      repls.push_back(cloned);
    }

    // remap operands
    for (int i = 0; i < inst.getNumOperands(); ++i) {
      ValVec opRepl = requestReplicate(*inst.getOperand(i));
      for (int c = 0; c < vecTy->getNumElements(); ++c) {
        repls[c]->setOperand(i, opRepl[c]);
      }
    }

    // insert op
    for (int c = 0; c < vecTy->getNumElements(); ++c) {
      builder.Insert(repls[c], ".r");
      replVec.push_back(repls[c]);
    }

  } else {
    assert(false && "un-replicatable operation");
    abort();
  }

// register replcate
  assert(!replVec.empty());
  replMap.addReplicate(inst, replVec);

  IF_DEBUG_SROV {
    errs() << "repls " << inst << ":\n";
    for (int i = 0; i < replVec.size(); ++i) {
      errs() << "\t" << i << " : " << *replVec[i] << "\n";
    }
  }

  return replVec;
}

ValVec
requestConstVectorReplicate(Constant & val) {
  ValVec res;
  auto & vecTy = cast<VectorType>(*val.getType());
  auto & elemTy = *vecTy.getElementType();
  const int width = vecTy.getNumElements();

  if (val.isZeroValue()) {
    for (int i = 0; i < width; ++i) {
      res.push_back(Constant::getNullValue(&elemTy));
    }

  } else {
    // generic const expresssion case
    for (int i = 0; i < val.getNumOperands(); ++i) {
      ValVec elemRepl = requestReplicate(*val.getOperand(i));
      Append(res, elemRepl);
    }
  }

  return res;
}

ValVec
requestReplicate(Value & val) {
  if (replMap.hasReplicate(val)) {
    return replMap.getReplVec(val);
  }

  TypeVec replTyVec;
  replicateType(*val.getType(), replTyVec);
  IF_DEBUG_SROV { errs() << "requestReplicate(" << val << ", elems=" << replTyVec.size() <<" )\n"; }

  ValVec replVec;

  assert(replTyVec.size() >= 1 && "un-replictable type");

// replication of atoms (unless this is an extract)
  if ((replTyVec.size() == 1) && !isa<ExtractValueInst>(val) && !isa<ExtractElementInst>(val)) {
    replVec.push_back(&val);
    return replVec;
  }

// replication of undef
  auto * undef = dyn_cast<UndefValue>(&val);
  if (undef) {
    for (size_t i = 0; i < replTyVec.size(); ++i) {
      replVec.push_back(UndefValue::get(replTyVec[i]));
    }
    replMap.addReplicate(val, replVec);
    return replVec;
  }

  auto * vecTy = dyn_cast<VectorType>(val.getType());

// replication of constant aggregates
  auto * constVal = dyn_cast<Constant>(&val);
  if (constVal) {
    if (vecTy) {
      replVec = requestConstVectorReplicate(*constVal);
    } else {
      for (int i = 0; i < constVal->getNumOperands(); ++i) {
        ValVec elemRepl = requestReplicate(*constVal->getOperand(i));
        Append(replVec, elemRepl);
      }
    }

    replMap.addReplicate(val, replVec);
    return replVec;
  }

// replication of instructions
  auto * inst = dyn_cast<Instruction>(&val);
  if (inst) {
    return requestInstructionReplicate(*inst, replTyVec);
  } else {
    abort(); // unsupported IR object
  }
}


bool
run() {
  bool changedCode = false;

  IF_DEBUG_SROV errs() << "---- SROV run log ----\n";

// replicate instructions
  ReversePostOrderTraversal<Function*> RPOT(&F);

  for (auto * BB : RPOT) {
    if (!vecInfo.inRegion(*BB)) continue;

    for (auto & I : *BB) {

    // only split varying values
      if (!vecInfo.getVectorShape(I).isVarying()) {
        // IF_DEBUG_SROV { errs() << "\tskpping (non-varying)\n"; }
        continue;
      }

      // only start replication from extractvalue
      if (isa<ExtractValueInst>(I)) {
        auto & extractedVal = *I.getOperand(0);

        int numScalarRepls = GetNumReplicates(*extractedVal.getType());
        if (numScalarRepls == 0) {
         IF_DEBUG_SROV { errs() << "SROV: can not replicate extractval operand: " << extractedVal << "). skipping..\n"; }
          continue;
        }

        // only try to replicate extract value insts
        ConstValSet checkedSet;
        if (!canReplicate(extractedVal, checkedSet)) {
          IF_DEBUG_SROV { errs() << "SROV: can not replicate dependent operation of: " << I << " " << extractedVal << ". skipping..\n"; }
          continue;
        }

        // go ahead and replicate
        IF_DEBUG_SROV { errs() << "SROV: scalar repl of: " << extractedVal << ") with " << numScalarRepls << " slots\n"; }
        requestReplicate(extractedVal);
        changedCode = true;

      } else if (isa<ExtractElementInst>(I)) {
        auto & extractedVal = *I.getOperand(0);

        int numScalarRepls = GetNumReplicates(*extractedVal.getType());
        if (numScalarRepls == 0) {
         IF_DEBUG_SROV { errs() << "SROV: can not replicate extractelem operand: " << extractedVal << "). skipping..\n"; }
          continue;
        }

        if (numScalarRepls >= vecInfo.getVectorWidth()) {
          IF_DEBUG_SROV { errs() << "SROV: will not replicate vectors that are larger than the target width for: " << extractedVal << "). skipping..\n"; }
          continue;
        }

        // only try to replicate extract value insts
        ConstValSet checkedSet;
        if (!canReplicate(extractedVal, checkedSet)) {
          IF_DEBUG_SROV { errs() << "SROV: can not replicate dependent operation of: " << I << " " << extractedVal << ". skipping..\n"; }
          continue;
        }

        // go ahead and replicate
        IF_DEBUG_SROV { errs() << "SROV: scalar repl of: " << extractedVal << ") with " << numScalarRepls << " slots\n"; }
        requestReplicate(extractedVal);
        changedCode = true;
      }
    }
  }

  if (replMap.size() > 0) {
    Report() << "srov: replicated " << replMap.size() << " instructions\n";
  }

// cleanup
  IF_DEBUG_SROV { errs() << "- finalizing -\n";  }
  finalize();

  IF_DEBUG_SROV { errs() << "-- SROV finished --\n";  }


  return changedCode;
}

};


// class SROVTransform - visible class
SROVTransform::SROVTransform(VectorizationInfo & _vecInfo, const PlatformInfo & _platInfo)
: vecInfo(_vecInfo)
, platInfo(_platInfo)
{}


bool
SROVTransform::run() {
  Impl impl(vecInfo.getScalarFunction(), vecInfo, platInfo);
  return impl.run();
}

}
