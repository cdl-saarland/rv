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

      // for every replicated slot
      for (size_t j = 0; j < phiRepls.size(); ++j) {
        auto & inRepl = *requestLaneReplicate(*oldIncoming, j);
        auto & replPhi = cast<PHINode>(*phiRepls[j]);
        replPhi.addIncoming(&inRepl, oldPhi->getIncomingBlock(i));
      }
    }
  }
}

// check whether all uses of this will-be-replicated instruction can be recovered from the scalare replicates
bool
canRepairUses(Value & val) {
  // can always repair constants
  if (isa<Constant>(val)) return true;

  if (isa<ExtractValueInst>(val)) return true;

  for (auto * user : val.users()) {
    if (!isa<PHINode>(user) && !isa<ExtractValueInst>(user) && !isa<SelectInst>(user) && !isa<InsertValueInst>(user)) {
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
  auto * insertInst = dyn_cast<InsertValueInst>(&val);

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

  } else if (insertInst) {
    return canReplicate(*insertInst->getAggregateOperand(), checkedSet);
  } else if (isa<ExtractValueInst>(val)) {
    IF_DEBUG_SROV { errs() << "srov: recursive replication not supported!\n"; }
    return false;
  } else if (isa<LoadInst>(val) || isa<StoreInst>(val)) {
    return true;
  }

  return false;
}

// re-aggregate @inst for external users of that value
void
repairExternalUses(Instruction & inst) {

  for (auto itUse = inst.use_begin(); itUse != inst.use_end(); ) {
    auto & use = *itUse++;
    auto * userInst = cast<Instruction>(use.getUser());

    // this use has been replicated
    if (replMap.hasReplicate(*userInst)) continue;

    // remap extractvalue inst to the scalar replicate
    auto * extractInst = dyn_cast<ExtractValueInst>(userInst);

    if (extractInst) {
      int extractOff = extractInst->getIndices()[0];
      extractInst->replaceAllUsesWith(requestLaneReplicate(*extractInst, extractOff));
      continue;
    }

    // TODO re-aggregate if need be
    errs() << "CAN NOT RE-aggregate " << *userInst << "\n";
    assert(false && "re-aggregation not yet implemented");
    abort();
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

TypeVec
replicateType(Type & type) {
  TypeVec tyReplVec;
  auto & structTy = cast<StructType>(type);
  for (size_t i = 0; i < structTy.getNumElements(); ++i) {
    tyReplVec.push_back(&getLaneReplType(structTy, i));
  }
  return tyReplVec;
}

Value*
requestLaneReplicate(Value & val, int i) {
// if this is a known replicated value reuse that one
  if (replMap.hasReplicate(val)) {
    return replMap.getReplVec(val)[i];
  }

  IF_DEBUG_SROV { errs() << "requestLaneReplicate(" << val << ")\n"; }

  auto * insertInst = dyn_cast<InsertValueInst>(&val);
  auto * extractInst = dyn_cast<ExtractValueInst>(&val);
  auto * constVal = dyn_cast<Constant>(&val);

// insertInst: if this is a matching insert return it, otw recursively descend into aggregate operand
  if (insertInst) {
    auto * intoVal = insertInst->getAggregateOperand();
    auto * elemVal = insertInst->getOperand(1);
    int32_t structOffset = insertInst->getIndices()[0];

    // we found our aggregate
    if (structOffset == i) {
      return elemVal;
    } else {
      // descend into other insertVals
      return requestLaneReplicate(*intoVal, i);
    }

// extractInst: forward to the scalar replicate
  } else if (extractInst) {
    auto * extractFrom = extractInst->getOperand(0);
    return requestLaneReplicate(*extractFrom, i);

  } else if (constVal) {
    auto & replTy = getLaneReplType(*constVal->getType(), i);
    if (isa<UndefValue>(constVal)) return UndefValue::get(&replTy);
    else if (constVal->isNullValue()) return Constant::getNullValue(&replTy);
    else return constVal->getOperand(i);
  }

// Otw: request a replication of @val and pick the replicate at this position
  requestReplicate(val);
  return replMap.getReplVec(val)[i];
}

ValVec
requestReplicate(Value & val) {
  if (replMap.hasReplicate(val)) {
    return replMap.getReplVec(val);
  }

  IF_DEBUG_SROV { errs() << "requestReplicate(" << val << ")\n"; }


  TypeVec replTyVec = replicateType(*val.getType());
  ValVec replVec;

  auto * undef = dyn_cast<UndefValue>(&val);
  if (undef) {
    for (size_t i = 0; i < replTyVec.size(); ++i) {
      replVec.push_back(UndefValue::get(replTyVec[i]));
    }
    replMap.addReplicate(val, replVec);
    return replVec;
  }

  auto * phi = dyn_cast<PHINode>(&val);
  auto * inst = dyn_cast<Instruction>(&val);
  assert(inst && "non replicatable value");
  IRBuilder<> builder(inst->getParent(), inst->getIterator());
  std::string oldInstName = inst->getName().str();

// phi replication logic (attach inputs later)
  if (phi) {
    for (size_t i = 0; i < replTyVec.size(); ++i) {
      std::stringstream ss;
      ss << oldInstName << ".repl." << i;
      std::string phiReplName = ss.str();

      auto * replPhi = builder.CreatePHI(replTyVec[i], phi->getNumIncomingValues(), phiReplName);
      replVec.push_back(replPhi);
      IF_DEBUG_SROV { errs() << "\t" << i << " : " << *replPhi << "\n"; }
    }

    // register PHI node as replicated
    replMap.addReplicate(*phi, replVec);

    // request operands
    for (size_t i = 0; i < phi->getNumIncomingValues(); ++i) {
      auto * inVal = phi->getIncomingValue(i);

      // descend into non-trivial operand instructions
      if (isa<Instruction>(inVal) && !isa<InsertValueInst>(inVal)) {
        requestReplicate(*inVal);
      }
    }

    // phi node already registered
    return replVec;
// generic instruction replication
  } else if (isa<StoreInst>(inst) || isa<LoadInst>(inst)) {
    auto * intTy = Type::getInt32Ty(builder.getContext());
    auto * load = dyn_cast<LoadInst>(inst);
    auto * store = dyn_cast<StoreInst>(inst);
    auto * ptr = load ? load->getPointerOperand() : store->getPointerOperand();
    VectorShape ptrShape = vecInfo.getVectorShape(*ptr);

    for (size_t i = 0; i < replTyVec.size(); ++i) {
      auto * elemGep = builder.CreateGEP(ptr, {ConstantInt::get(intTy, 0, true), ConstantInt::get(intTy, i, true)}, "srov_gep");
      vecInfo.setVectorShape(*elemGep, ptrShape); // FIXME alignment
      Value * replInst = nullptr;
      if (load) {
        replInst = builder.CreateLoad(elemGep);
      } else if (store) {
        assert(false && "not implemented!");
        // replInst = builder.CreateStore(
      }
      replVec.push_back(replInst);
    }

  } else if (isa<SelectInst>(inst)) {
    auto * selectInst = cast<SelectInst>(inst);
    auto * selMask = selectInst->getOperand(0);
    auto * selTrue = selectInst->getOperand(1);
    auto * selFalse = selectInst->getOperand(2);

    for (size_t i = 0; i < replTyVec.size(); ++i) {
      std::stringstream ss;
      ss << oldInstName << ".repl." << i;

      auto * replTrue = requestLaneReplicate(*selTrue, i);
      auto * replFalse = requestLaneReplicate(*selFalse, i);

      auto * replSelect = builder.CreateSelect(selMask, replTrue, replFalse, ss.str());
      replVec.push_back(replSelect);
      IF_DEBUG_SROV { errs() << "\t" << i << " : " << *replSelect << "\n"; }
    }

  } else if (isa<InsertValueInst>(inst)) {
    for (size_t i = 0; i < replTyVec.size(); ++i) {
      replVec.push_back(requestLaneReplicate(*inst, i));
    }
  } else {
    assert(false && "un-replicatable operation");
    abort();
  }

// register replcate
  assert(!replVec.empty());
  replMap.addReplicate(val, replVec);

  return replVec;
}

// returns the number of scalar values that can represent this type
// return 0 if such a representation does not exist
int
getNumReplicates(Type & type) {
  // for now only structs
  if (!type.isStructTy()) { return 0; }

  auto & structTy = cast<StructType>(type);

// check if this struct is composed entirely of int/bool or fp types
#if 0
  for (size_t i = 0; i < structTy.getNumElements(); ++i) {
    auto & elemTy = *structTy.getElementType(i);
    if (!elemTy.isIntegerTy() && !elemTy.isFloatingPointTy()) {
      return 0;
    }
  }
#endif

// passed all tests
  // we will need a scalar replica for each elemnt
  return structTy.getNumElements();
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
      if (!isa<ExtractValueInst>(I)) continue;

      auto & extractedVal = *I.getOperand(0);

      int numScalarRepls = getNumReplicates(*extractedVal.getType());
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
