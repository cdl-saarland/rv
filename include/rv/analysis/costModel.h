//===- rv/analysis/costModel.h - Vector width heuristics --*- C++ -*-===//
//
// Part of the RV Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef RV_ANALYSIS_COSTMODEL_H
#define RV_ANALYSIS_COSTMODEL_H

#include <cstddef>

namespace llvm {
  class Instruction;
  class Type;
  class BasicBlock;
  class TargetTransformInfo;
  class Function;
}

namespace rv {

struct Config;
class PlatformInfo;
class VectorizationInfo;
class Region;

struct VectorMapping;

class CostModel {
  PlatformInfo & platInfo;
  Config & config;
  llvm::TargetTransformInfo & tti;

  bool needsReplication(const llvm::Instruction & inst) const;

public:
  CostModel(PlatformInfo & _platInfo, Config & _config);

  // whether this is an vectorizable LLVM intrinsic
  bool IsVectorizableFunction(llvm::Function & Callee) const;

  // pick a width for @inst
  size_t pickWidthForInstruction(const llvm::Instruction & inst, size_t maxWidth) const;

  // pick a width for @type
  size_t pickWidthForType(const llvm::Type & type, size_t maxWidth) const;

  // pick an appropriate vector width for the mapping in @mapping
  size_t pickWidthForMapping(const VectorMapping & mapping) const;

  // pick a vector width for a single block/the region
  size_t pickWidthForBlock(const llvm::BasicBlock & block, size_t maxWidth) const;
  size_t pickWidthForRegion(const Region & region, size_t maxWidth) const;
};

}

#endif // RV_ANALYSIS_COSTMODEL_H
