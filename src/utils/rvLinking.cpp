#include "utils/rvLinking.h"

#include <llvm/IR/Value.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/Transforms/Utils/ValueMapper.h>
#include <llvm/Transforms/Utils/Cloning.h>
#include "rvConfig.h"

#if 0
#define IF_DEBUG_LINK if (true)
#else
#define IF_DEBUG_LINK IF_DEBUG
#endif

using namespace llvm;

namespace rv {

// TODO move into separate file
Value & cloneConstant(Constant& ConstVal, Module & cloneInto, LinkerCallback LCB) {
  IF_DEBUG_LINK errs() << "cloneConst: " << ConstVal << "LCB: " << (bool) LCB << "\n";
  if (isa<GlobalValue>(ConstVal)) {
    Value *UserGV = nullptr;
    if (LCB)
      UserGV = LCB(cast<GlobalValue>(ConstVal), cloneInto);
    if (UserGV && (UserGV != &ConstVal))
      return *UserGV;
    return cloneGlobalIntoModule(cast<GlobalValue>(ConstVal), cloneInto, LCB);
  }
  auto * expr = dyn_cast<ConstantExpr>(&ConstVal);
  if (!expr) return ConstVal;

  // descend into operands and replicate
  const ConstantExpr & constExpr = *expr;

  SmallVector<Constant*, 4> clonedOps;
  for (size_t i = 0; i < constExpr.getNumOperands(); ++i) {
    Value &Cloned = cloneConstant(*constExpr.getOperand(i), cloneInto, LCB);
    Constant *ClonedConst = reinterpret_cast<Constant *>(&Cloned);
    // Bitcast to stay compatible (required for ConstantExpr::getWithOperands
    // really).
    if (ClonedConst->getType() != constExpr.getOperand(i)->getType()) {
      ClonedConst = ConstantExpr::getBitCast(
          ClonedConst, constExpr.getOperand(i)->getType());
    }

    clonedOps.push_back(ClonedConst);
  }

  return *constExpr.getWithOperands(clonedOps);
}

GlobalValue &cloneGlobalIntoModule(GlobalValue &gv, Module &cloneInto,
                                   LinkerCallback LCB) {
  IF_DEBUG_LINK errs() << "cloneGlobal: " << gv << "\n";
  if (isa<Function>(gv)) {
    auto &func = cast<Function>(gv);
    return cloneFunctionIntoModule(func, cloneInto, func.getName(), LCB);
  }
  assert(isa<GlobalVariable>(gv));
  auto &global = cast<GlobalVariable>(gv);
  auto *clonedGv = cloneInto.getGlobalVariable(global.getName());
  // FIXME: accumulating
  if (clonedGv && clonedGv->hasInitializer()) {
    IF_DEBUG_LINK errs() << "\tHAS: " << *clonedGv->getInitializer() << "\n";
    return *clonedGv;
  }

  // clone
  auto *clonedGlobal = cast<GlobalVariable>(
      cloneInto.getOrInsertGlobal(global.getName(), global.getValueType()));

  // clone initializer (could depend on other constants)
  if (global.hasInitializer()) {
    auto *initConst = const_cast<Constant *>(global.getInitializer());
    Constant *clonedInitConst = initConst;
    if (initConst) {
      clonedInitConst = cast<Constant>(&cloneConstant(*initConst, cloneInto, LCB));
    }
    clonedGlobal->setInitializer(clonedInitConst);
  }

  clonedGlobal->setThreadLocalMode(global.getThreadLocalMode());
  clonedGlobal->setAlignment(global.getAlign());
  clonedGlobal->copyAttributesFrom(&global);
  return *clonedGlobal;
}

Function &cloneFunctionIntoModule(Function &func, Module &cloneInto, StringRef name, LinkerCallback LCB) {
  IF_DEBUG_LINK errs() << "cloneFunc: " << func.getName() << "\n";

  // eg already migrated
  auto * existingFn = cloneInto.getFunction(name);
  if (existingFn && (func.isDeclaration() == existingFn->isDeclaration()))
    return *existingFn;

#if 0
  // FIXME: Don't try to re-clone the top-level function we are cloning.
  // Check Callback
  if (LCB) {
    auto *V = LCB(func, cloneInto);
    if (V)
      return cast<Function>(*V);
  }
#endif

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

  // Clone used constants, globals on demand.
  for (auto I = inst_begin(func), E = inst_end(func); I != E; ++I) {
    for (size_t i = 0; i < I->getNumOperands(); ++i) {
      auto * usedConstant = dyn_cast<Constant>(I->getOperand(i));
      if (usedConstant)  {
        auto & clonedConstant = cloneConstant(*usedConstant, cloneInto, LCB);
        VMap[usedConstant] = &clonedConstant;
      }
      auto * UsedGlobal = dyn_cast<GlobalValue>(I->getOperand(i));

      // Recursive self-cloning undesirable at this point.
      if (UsedGlobal == &func)
        continue;

      if (LCB && UsedGlobal) {
        Value *clonedGlobal = UsedGlobal;
        if (LCB)
          clonedGlobal = LCB(*UsedGlobal, cloneInto);
        if (clonedGlobal && (clonedGlobal != UsedGlobal))
          VMap[UsedGlobal] = clonedGlobal;
      }
    }
  }

  SmallVector<ReturnInst *, 1> Returns; // unused

  CloneFunctionInto(&clonedFn, &func, VMap, CloneFunctionChangeType::ClonedModule, Returns);
  return clonedFn;
}

} // namespace rv
