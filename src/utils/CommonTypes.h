//===- src/utils/CommonTypes.h - commonly used typedefs --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef COMMONTYPES_HPP
#define COMMONTYPES_HPP

#include <set>
#include <vector>

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Module.h>
#include <llvm/Pass.h>

#include <llvm/Transforms/Utils/ValueMapper.h>

namespace rv {

typedef std::vector<llvm::BasicBlock *> BlockVector;
typedef std::set<llvm::BasicBlock *> BlockSet;
typedef llvm::ValueToValueMapTy ValueMap;

}

#endif
