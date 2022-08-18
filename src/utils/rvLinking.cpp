#include "utils/rvLinking.h"

#include <llvm/IR/Instructions.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/Transforms/Utils/ValueMapper.h>
#include <llvm/Transforms/Utils/Cloning.h>

using namespace llvm;

namespace rv {

// TODO move into separate file
Constant & cloneConstant(Constant& constVal, Module & cloneInto) {
  if (isa<GlobalValue>(constVal)) {
    return cloneGlobalIntoModule(cast<GlobalValue>(constVal), cloneInto);
  }
  auto * expr = dyn_cast<ConstantExpr>(&constVal);
  if (!expr) return constVal;

  // descend into operands and replicate
  const ConstantExpr & constExpr = *expr;

  SmallVector<Constant*, 4> clonedOps;
  for (size_t i = 0; i < constExpr.getNumOperands(); ++i) {
    const Constant & cloned = cloneConstant(*constExpr.getOperand(i), cloneInto);
    clonedOps.push_back(const_cast<Constant*>(&cloned));
  }

  return *constExpr.getWithOperands(clonedOps);
}

GlobalValue & cloneGlobalIntoModule(GlobalValue &gv, Module &cloneInto) {
  if (isa<Function>(gv)) {
    auto & func = cast<Function>(gv);
    return cloneFunctionIntoModule(func, cloneInto, func.getName());

  } else {
    assert(isa<GlobalVariable>(gv));
    auto & global = cast<GlobalVariable>(gv);
    auto * clonedGv = cloneInto.getGlobalVariable(global.getName());
    if (clonedGv) return *clonedGv;

    // clone
    auto * clonedGlobal = cast<GlobalVariable>(cloneInto.getOrInsertGlobal(global.getName(), global.getValueType()));

    // clone initializer (could depend on other constants)
    if (global.hasInitializer()) {
      auto * initConst = const_cast<Constant*>(global.getInitializer());
      Constant * clonedInitConst = initConst;
      if (initConst) {
        clonedInitConst = &cloneConstant(*initConst, cloneInto);
      }
      clonedGlobal->setInitializer(clonedInitConst);
    }

    clonedGlobal->setThreadLocalMode(global.getThreadLocalMode());
    clonedGlobal->setAlignment(global.getAlign());
    clonedGlobal->copyAttributesFrom(&global);
    return *clonedGlobal;
  }

  // unsupported global value
  abort();
}

Function &cloneFunctionIntoModule(Function &func, Module &cloneInto, StringRef name) {
  // eg already migrated
  auto * existingFn = cloneInto.getFunction(name);
  if (existingFn && (func.isDeclaration() == existingFn->isDeclaration())) {
    return *existingFn;
  }

  // create function in new module, create the argument mapping, clone function into new function body, return
  Function & clonedFn = *Function::Create(func.getFunctionType(), Function::LinkageTypes::ExternalLinkage,
                                        name, &cloneInto);
  clonedFn.copyAttributesFrom(&func);

  // external decl
  if (func.isDeclaration()) return clonedFn;

  ValueToValueMapTy VMap;
  auto CI = clonedFn.arg_begin();
  for (auto I = func.arg_begin(), E = func.arg_end(); I != E; ++I, ++CI) {
    Argument *arg = &*I, *carg = &*CI;
    carg->setName(arg->getName());
    VMap[arg] = carg;
  }
  // remap constants
  for (auto I = inst_begin(func), E = inst_end(func); I != E; ++I) {
    for (size_t i = 0; i < I->getNumOperands(); ++i) {
      auto * usedConstant = dyn_cast<Constant>(I->getOperand(i));
      if (!usedConstant) continue;

      auto & clonedConstant = cloneConstant(*usedConstant, cloneInto);
      VMap[usedConstant] = &clonedConstant;
    }
  }

  SmallVector<ReturnInst *, 1> Returns; // unused

  CloneFunctionInto(&clonedFn, &func, VMap, CloneFunctionChangeType::ClonedModule, Returns);
  return clonedFn;
}

} // namespace rv
