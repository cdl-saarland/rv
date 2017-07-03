/*
 * CommonTypes.h
 *
 *  Created on: ?.2010
 *      Author: Simon Moll
 */

#ifndef COMMONTYPES_HPP
#define COMMONTYPES_HPP

#include <iostream>
#include <map>
#include <set>
#include <stack>
#include <stdio.h>
#include <vector>

#include <llvm/ADT/APFloat.h>
#include <llvm/Support/raw_ostream.h>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h>
#include <llvm/Pass.h>

#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Analysis/Passes.h>
#include <llvm/Analysis/PostDominators.h>
#include <llvm/IR/Dominators.h>
#include <llvm/Transforms/Utils/ValueMapper.h>

//#include <util/llvmShortCuts.h>


#if 1
#define IF_DEBUG_CNS if (false)
#else
#define IF_DEBUG CNS IF_DEBUG
#endif

namespace rv {

typedef llvm::ValueToValueMapTy ValueMap;
// llvm::DenseMap<const llvm::Value*, llvm::Value*> ValueMap;

typedef std::set<const llvm::StructType *> StructTypeSet;
typedef std::set<const llvm::Type *> TypeSet;
typedef std::set<const llvm::StructType *> StructTypeSet;
typedef std::map<const llvm::Type *, std::string> TypeNames;
typedef std::vector<llvm::StructType *> StructTypeVector;
typedef std::vector<llvm::Function *> FunctionVector;

typedef std::vector<llvm::Value *> ValueVector;
typedef std::set<llvm::Value *> ValueSet;
typedef std::set<const llvm::Value *> ConstValueSet;

typedef std::set<llvm::PHINode *> PHISet;

typedef std::set<llvm::Instruction *> InstructionSet;
typedef llvm::Function::ArgumentListType ArgList;
typedef std::vector<llvm::BasicBlock *> BlockVector;
typedef std::set<llvm::BasicBlock *> BlockSet;
typedef std::vector<BlockSet> BlockSetVector;

typedef std::vector<std::string> StringVector;
typedef std::set<std::string> StringSet;

typedef std::set<const llvm::BasicBlock *> ConstBlockSet;
typedef std::pair<llvm::BasicBlock *, llvm::BasicBlock *> BlockPair;
typedef std::vector<std::pair<llvm::BasicBlock *, llvm::BasicBlock *>>
    BlockPairVector;
typedef std::stack<llvm::BasicBlock *> BlockStack;

typedef std::vector<llvm::Loop *> LoopVector;
typedef std::set<llvm::Loop *> LoopSet;

typedef std::set<int> IntSet;


#if 0
/*
 * generic variable descriptor
 */
struct VariableDesc {
  const llvm::Type *type;
  std::string name;
  bool isAlloca;

  VariableDesc(const llvm::Value *val, std::string _name);
  VariableDesc();

  void print(llvm::raw_ostream &out);
};

typedef std::map<llvm::Value *, VariableDesc> VariableMap;
typedef std::map<const llvm::Value *, VariableDesc> ConstVariableMap;

/*
 * region context information - used by extraction passes
 */
struct ExtractorContext {
  llvm::Loop *parentLoop;
  llvm::BasicBlock *continueBlock;
  llvm::BasicBlock *breakBlock;
  llvm::BasicBlock *exitBlock;

  InstructionSet expressionInsts; // distinguishes between insts used in a
                                  // closed expressions and normal insts
                                  // (prechecked-while)
  bool isPrecheckedLoop;

  ExtractorContext();

  /*
  * {B_break, B_continue}
  *
  * (used by ASTExtractor)
  */
  BlockSet getRegularExits() const;

  /*
  * {B_break, B_continue, B_exit}
  *
  * (used by RestructuringPass)
  */
  BlockSet getAnticipatedExits() const;

  void dump() const;
  void dump(std::string prefix) const;
};

/*
 * stacked identifier bindings
 */
class IdentifierScope {
  IdentifierScope *parent;

public:
  ConstVariableMap identifiers;

  IdentifierScope(const ConstVariableMap &_identifiers);
  IdentifierScope(IdentifierScope *_parent,
                  const ConstVariableMap &_identifiers);
  IdentifierScope(IdentifierScope *_parent);

  IdentifierScope *getParent() const;
  const VariableDesc *lookUp(const llvm::Value *value) const;
  void bind(const llvm::Value *val, VariableDesc desc);

  ConstVariableMap::const_iterator begin() const;
  ConstVariableMap::const_iterator end() const;
};
#endif

}


#endif
