#include <llvm/IR/Instructions.h>
#include <llvm/IR/Constants.h>

#include <rv/transform/splitAllocas.h>
#include <rv/vectorizationInfo.h>

#include <rvConfig.h>
#include "report.h"

using namespace llvm;

#if 1
#define IF_DEBUG_SA IF_DEBUG
#else
#define IF_DEBUG_SA if (false)
#endif

namespace rv {

SplitAllocas::SplitAllocas(VectorizationInfo & _vecInfo)
  : vecInfo(_vecInfo)
{}

void SplitAllocas::AllocaTree::store(IRBuilder<> & builder, VectorizationInfo & vecInfo, VectorShape vectorShape, Value * value) {
  if (type->isStructTy()) {
    for (size_t i = 0; i < children.size(); ++i) {
      auto extract = builder.CreateExtractValue(value, i);
      vecInfo.setVectorShape(*extract, vectorShape);
      children[i]->store(builder, vecInfo, vectorShape, extract);
    }
  } else {
    auto store = builder.CreateStore(value, leafAlloca);
    vecInfo.setVectorShape(*store, vectorShape);
  }
}

Value * SplitAllocas::AllocaTree::load(IRBuilder<> & builder, VectorizationInfo & vecInfo, VectorShape vectorShape) {
  if (type->isStructTy()) {
    Value * loadVal = UndefValue::get(type);
    for (size_t i = 0; i < children.size(); ++i) {
      loadVal = builder.CreateInsertValue(loadVal, children[i]->load(builder, vecInfo, vectorShape), i);
      vecInfo.setVectorShape(*loadVal, vectorShape);
    }
    return loadVal;
  } else {
    auto load = builder.CreateLoad(leafAlloca);
    vecInfo.setVectorShape(*load, vectorShape);
    return load;
  }
}

bool SplitAllocas::analyseUses(Instruction * inst, Type * type) {
  for (auto user : inst->users()) {
    if (auto userInst = dyn_cast<Instruction>(user)) {
      auto * store = dyn_cast<StoreInst>(userInst);
      auto * load = dyn_cast<LoadInst>(userInst);
      auto * gep = dyn_cast<GetElementPtrInst>(userInst);

      if (gep) {
        for (auto id_it = gep->idx_begin(); id_it != gep->idx_end(); ++id_it) {
          auto cst = dyn_cast<ConstantInt>(&*id_it);
          if (!cst) {
            IF_DEBUG_SA { errs() << "skip: non constant gep\n"; }
            return false;
          }
          if (id_it == gep->idx_begin()) {
            // first gep index must be 0
            if (cst->getZExtValue() != 0) {
              IF_DEBUG_SA { errs() << "skip: first index non zero\n"; }
              return false;
            }
          }
        }
        if (!analyseUses(gep, gep->getType())) return false;
      } else if (!load && !store && type->getPointerElementType()->isStructTy()) {
        // not a gep, load or store
        IF_DEBUG_SA { errs() << "skip: unforeseen use" << *userInst << "\n"; }
        return false;
      }
    }
  }

  return true;
}

std::unique_ptr<SplitAllocas::AllocaTree> SplitAllocas::createAllocaTree(llvm::AllocaInst * allocaInst, Type * type, VectorShape vectorShape) {
  if (type->isStructTy()) {
    AllocaTree::Children children;
    for (size_t i = 0; i < type->getStructNumElements(); ++i) {
      children.emplace_back(createAllocaTree(allocaInst, type->getStructElementType(i), vectorShape));
    }
    return std::unique_ptr<AllocaTree>(new AllocaTree(type, std::move(children)));
  } else {
    IF_DEBUG_SA { errs() << "\t- " << *type << "\n"; }
    auto alloca = new AllocaInst(type, allocaInst->getType()->getAddressSpace(), allocaInst->getName(), allocaInst);
    vecInfo.setVectorShape(*alloca, vectorShape);
    return std::unique_ptr<AllocaTree>(new AllocaTree(type, alloca));
  }
}

void SplitAllocas::splitUses(Instruction * inst, AllocaTree * tree, VectorShape vectorShape) {
  std::vector<Instruction *> deadInsts;

  for (auto use_it = inst->use_begin(); use_it != inst->use_end();) {
    auto & use = *use_it;
    if (auto userInst = dyn_cast<Instruction>(use.getUser())) {
      auto * store = dyn_cast<StoreInst>(userInst);
      auto * load = dyn_cast<LoadInst>(userInst);
      auto * gep = dyn_cast<GetElementPtrInst>(userInst);

      IF_DEBUG_SA { errs() << "\t- " << *userInst << "\n"; }

      if (load || store) {
        IRBuilder<> builder(userInst->getParent(), userInst->getIterator());
        if (load)  load->replaceAllUsesWith(tree->load(builder, vecInfo, vectorShape));
        if (store) tree->store(builder, vecInfo, vectorShape, store->getValueOperand());
      } else if (gep) {
        // traverse the tree
        auto cur = tree;
        for (auto id_it = gep->idx_begin() + 1; id_it != gep->idx_end(); ++id_it) {
          assert(!cur->leafAlloca && "must not be a leaf node");
          auto integerCst = cast<ConstantInt>(&*id_it)->getZExtValue();
          cur = cur->children[integerCst].get();
        }
        splitUses(gep, cur, vectorShape);
      } else {
        assert(tree->leafAlloca);
        // changing the operand will remove the current use from the list
        // we hence need to increment the iterator first
        ++use_it;
        userInst->setOperand(use.getOperandNo(), tree->leafAlloca);
        continue; // do not mark the instruction as dead code
      }

      deadInsts.push_back(userInst);
    }
    ++use_it;
  }

  for (auto inst : deadInsts) {
    IF_DEBUG_SA { errs() << "\tdeleting:" << *inst << "\n"; }
    inst->eraseFromParent();
  }
}

bool SplitAllocas::run() {
  IF_DEBUG_SA { errs() << "-- split allocas opt log --\n"; }

  std::vector<AllocaInst *> queue;
  for (auto & bb : vecInfo.getScalarFunction()) {
    auto itBegin = bb.begin(), itEnd = bb.end();
    for (auto it = itBegin; it != itEnd; ) {
      auto * allocaInst = dyn_cast<AllocaInst>(it++);
      if (allocaInst) queue.push_back(allocaInst);
    }
  }

  size_t numSplit = 0;
  for (auto allocaInst : queue) {
    auto vectorShape = vecInfo.getVectorShape(*allocaInst);
    if (vectorShape.isUniform() || !allocaInst->getAllocatedType()->isStructTy())
      continue;

    IF_DEBUG_SA { errs() << "\n# trying to split alloca " << *allocaInst << "\n"; }
    if (analyseUses(allocaInst, allocaInst->getType())) {
      IF_DEBUG_SA { errs() << "--members:\n"; }
      auto root = createAllocaTree(allocaInst, allocaInst->getAllocatedType(), vectorShape);
      IF_DEBUG_SA { errs() << "--uses:\n"; }
      splitUses(allocaInst, root.get(), vectorShape);
      allocaInst->eraseFromParent();
      numSplit++;
    }
  }

  if (numSplit > 0) {
    Report() << "splitAllocas: split " << numSplit << " allocas\n";
  }

  IF_DEBUG_SA { errs() << "-- end of split allocas opt log --\n"; }

  return numSplit > 0;
}

} // namespace rv
