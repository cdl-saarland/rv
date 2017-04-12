#include <llvm/Support/raw_ostream.h>
#include <llvm/Analysis/VectorUtils.h>
#include <llvm/Transforms/Utils/LoopUtils.h>
#include "CostModel.h"
#include "rvConfig.h"

namespace llvm {

static cl::opt<unsigned>
ForceTargetInstructionCost("rv-force-target-instruction-cost", cl::init(0), cl::Hidden,
                           cl::desc("A flag that overrides the target's expected cost for "
                                    "an instruction to a single constant value. Mostly "
                                    "useful for getting consistent testing."));

#if ENABLE_VF_AUTO_SELECT

LoopVectorizationCostModel::VectorizationFactor
LoopVectorizationCostModel::selectVectorizationFactor(bool OptForSize) {
  // Width 1 means no vectorize
  VectorizationFactor Factor = {1U, 0U};
  if (OptForSize && RPC->Need) {
    emitAnalysis(VectorizationReport() <<
                                       "runtime pointer checks needed. Enable vectorization of this "
                                               "loop with '#pragma clang loop vectorize(enable)' when "
                                               "compiling with -Os/-Oz");
    IF_DEBUG(dbgs() <<
                 "LV: Aborting. Runtime ptr check is required with -Os/-Oz.\n");
    return Factor;
  }

  if (!EnableCondStoresVectorization && Legal->getNumPredStores()) {
    emitAnalysis(VectorizationReport() <<
                                       "store that is conditionally executed prevents vectorization");
    IF_DEBUG(dbgs() << "LV: No vectorization. There are conditional stores.\n");
    return Factor;
  }

  // Find the trip count.
  unsigned TC = SE->getSmallConstantTripCount(TheLoop);
  IF_DEBUG(dbgs() << "LV: Found trip count: " << TC << '\n');

  MinBWs = computeMinimumValueSizes(TheLoop->getBlocks(), *DB, &TTI);
  unsigned SmallestType, WidestType;
  std::tie(SmallestType, WidestType) = getSmallestAndWidestTypes();
  unsigned WidestRegister = TTI.getRegisterBitWidth(true);
  unsigned MaxSafeDepDist = -1U;
  if (Legal->getMaxSafeDepDistBytes() != -1U) {
    MaxSafeDepDist = Legal->getMaxSafeDepDistBytes() * 8;
  }
  WidestRegister = ((WidestRegister < MaxSafeDepDist) ?
                    WidestRegister : MaxSafeDepDist);
  unsigned MaxVectorSize = WidestRegister / WidestType;

  IF_DEBUG(dbgs() << "LV: The Smallest and Widest types: " << SmallestType << " / "
               << WidestType << " bits.\n");
  IF_DEBUG(dbgs() << "LV: The Widest register is: "
               << WidestRegister << " bits.\n");

  if (MaxVectorSize == 0) {
    IF_DEBUG(dbgs() << "LV: The target has no vector registers.\n");
    MaxVectorSize = 1;
  }

  assert(MaxVectorSize <= 64 && "Did not expect to pack so many elements"
          " into one vector!");

  unsigned VF = MaxVectorSize;
  if (MaximizeBandwidth && !OptForSize) {
    // Collect all viable vectorization factors.
    SmallVector<unsigned, 8> VFs;
    unsigned NewMaxVectorSize = WidestRegister / SmallestType;
    for (unsigned VS = MaxVectorSize; VS <= NewMaxVectorSize; VS *= 2)
      VFs.push_back(VS);

    // For each VF calculate its register usage.
    auto RUs = calculateRegisterUsage(VFs);

    // Select the largest VF which doesn't require more registers than existing
    // ones.
    unsigned TargetNumRegisters = TTI.getNumberOfRegisters(true);
    for (int i = RUs.size() - 1; i >= 0; --i) {
      if (RUs[i].MaxLocalUsers <= TargetNumRegisters) {
        VF = VFs[i];
        break;
      }
    }
  }

  // If we optimize the program for size, avoid creating the tail loop.
  if (OptForSize) {
    // If we are unable to calculate the trip count then don't try to vectorize.
    if (TC < 2) {
      emitAnalysis
              (VectorizationReport() <<
                                     "unable to calculate the loop count due to complex control flow");
      IF_DEBUG(dbgs() << "LV: Aborting. A tail loop is required with -Os/-Oz.\n");
      return Factor;
    }

    // Find the maximum SIMD width that can fit within the trip count.
    VF = TC % MaxVectorSize;

    if (VF == 0) {
      VF = MaxVectorSize;
    }
    else {
      // If the trip count that we found modulo the vectorization factor is not
      // zero then we require a tail.
      emitAnalysis(VectorizationReport() <<
                                         "cannot optimize for size and vectorize at the "
                                                 "same time. Enable vectorization of this loop "
                                                 "with '#pragma clang loop vectorize(enable)' "
                                                 "when compiling with -Os/-Oz");
      IF_DEBUG(dbgs() << "LV: Aborting. A tail loop is required with -Os/-Oz.\n");
      return Factor;
    }
  }

  int UserVF = Hints->getWidth();
  if (UserVF != 0) {
    assert(isPowerOf2_32(UserVF) && "VF needs to be a power of two");
    IF_DEBUG(dbgs() << "LV: Using user VF " << UserVF << ".\n");

    Factor.Width = UserVF;
    return Factor;
  }

  float Cost = expectedCost(1);
#ifndef NDEBUG
  const float ScalarCost = Cost;
#endif /* NDEBUG */
  unsigned Width = 1;
  IF_DEBUG(dbgs() << "LV: Scalar loop costs: " << (int) ScalarCost << ".\n");

  bool ForceVectorization = Hints->getForce() == LoopVectorizeHints::FK_Enabled;
  // Ignore scalar width, because the user explicitly wants vectorization.
  if (ForceVectorization && VF > 1) {
    Width = 2;
    Cost = expectedCost(Width) / (float) Width;
  }

  for (unsigned i = 2; i <= VF; i *= 2) {
    // Notice that the vector loop needs to be executed less times, so
    // we need to divide the cost of the vector loops by the width of
    // the vector elements.
    float VectorCost = expectedCost(i) / (float) i;
    IF_DEBUG(dbgs() << "LV: Vector loop of width " << i << " costs: " <<
                 (int) VectorCost << ".\n");
    if (VectorCost < Cost) {
      Cost = VectorCost;
      Width = i;
    }
  }

  IF_DEBUG(if (ForceVectorization && Width > 1 && Cost >= ScalarCost) {
    dbgs()
            << "LV: Vectorization seems to be not beneficial, "
            << "but was forced by a user.\n"
  });
  IF_DEBUG(dbgs() << "LV: Selecting VF: " << Width << ".\n");
  Factor.Width = Width;
  Factor.Cost = Width * Cost;
  return Factor;
}

#endif

int LoopVectorizationCostModel::expectedCost(unsigned VF) {
  int Cost = 0;

  // For each block.
  for (BasicBlock* BB : TheLoop->blocks()) {
    int BlockCost = 0;

    // For each instruction in the old loop.
    for (Instruction& inst : *BB) {
      // Skip dbg intrinsics.
      // Skip ignored values.
      if (isa<DbgInfoIntrinsic>(inst)) continue;
      if (ValuesToIgnore.count(&inst)) continue;

      int InstCost = getInstructionCost(&inst, VF);

      // Check if we should override the cost.
      if (ForceTargetInstructionCost.getNumOccurrences() > 0) {
        InstCost = ForceTargetInstructionCost;
      }

      BlockCost += InstCost;
      IF_DEBUG(dbgs() << "LV: Found an estimated cost of " << InstCost << " for VF "
                   << VF << " For instruction: " << inst << '\n');
    }

    // We assume that if-converted blocks have a 50% chance of being executed.
    // When the code is scalar then some of the blocks are avoided due to CF.
    // When the code is vectorized we execute all code paths.
    //if (VF == 1 && Legal->blockNeedsPredication(BB)) {
    if (VF == 1 && VecInfo.getVectorShape(*BB).isVarying()) {
      BlockCost /= 2;
    }

    Cost += BlockCost;
  }

  return Cost;
}

/// \brief Check whether the address computation for a non-consecutive memory
/// access looks like an unlikely candidate for being merged into the indexing
/// mode.
///
/// We look for a GEP which has one index that is an induction variable and all
/// other indices are loop invariant. If the stride of this access is also
/// within a small bound we decide that this address computation can likely be
/// merged into the addressing mode.
/// In all other cases, we identify the address computation as complex.
#if 0
static bool
isLikelyComplexAddressComputation(Value *Ptr, LoopVectorizationLegality *Legal,
                                  ScalarEvolution *SE, const Loop *TheLoop)
{
  GetElementPtrInst *Gep = dyn_cast<GetElementPtrInst>(Ptr);
  if (!Gep) return true;

  // We are looking for a gep with all loop invariant indices except for one
  // which should be an induction variable.
  unsigned NumOperands = Gep->getNumOperands();
  for (unsigned i = 1; i < NumOperands; ++i) {
    Value *Opd = Gep->getOperand(i);
    if (!SE->isLoopInvariant(SE->getSCEV(Opd), TheLoop) && !Legal->isInductionVariable(Opd))
      return true;
  }

  // Now we know we have a GEP ptr, %inv, %ind, %inv. Make sure that the step
  // can likely be merged into the address computation.
  unsigned MaxMergeDistance = 64;

  const SCEVAddRecExpr *AddRec = dyn_cast<SCEVAddRecExpr>(SE->getSCEV(Ptr));
  if (!AddRec) return true;

  // Check the step is constant.
  const SCEV *Step = AddRec->getStepRecurrence(*SE);
  // Calculate the pointer stride and check if it is consecutive.
  const SCEVConstant *C = dyn_cast<SCEVConstant>(Step);
  if (!C) return true;

  const APInt &APStepVal = C->getAPInt();

  // Huge step value - give up.
  if (APStepVal.getBitWidth() > 64) return true;

  int64_t StepVal = APStepVal.getSExtValue();
  return StepVal > MaxMergeDistance;
}
#endif

/// A helper function for converting Scalar types to vector types.
/// If the incoming type is void, we return void. If the VF is 1, we return
/// the scalar type.
static Type* ToVectorTy(Type *Scalar, unsigned VF) {
  if (Scalar->isVoidTy() || VF == 1) return Scalar;
  return VectorType::get(Scalar, VF);
}

static Type *smallestIntegerVectorType(Type *T1, Type *T2) {
  IntegerType *I1 = cast<IntegerType>(T1->getVectorElementType());
  IntegerType *I2 = cast<IntegerType>(T2->getVectorElementType());
  return I1->getBitWidth() < I2->getBitWidth() ? T1 : T2;
}

static Type *largestIntegerVectorType(Type *T1, Type *T2) {
  IntegerType *I1 = cast<IntegerType>(T1->getVectorElementType());
  IntegerType *I2 = cast<IntegerType>(T2->getVectorElementType());
  return I1->getBitWidth() > I2->getBitWidth() ? T1 : T2;
}

/// Estimate the overhead of scalarizing a value. Insert and Extract are set if
/// the result needs to be inserted and/or extracted from vectors.
static int getScalarizationOverhead(Type *Ty, bool Insert, bool Extract,
                                    const TargetTransformInfo &TTI)
{
  if (Ty->isVoidTy()) return 0;

  assert(Ty->isVectorTy() && "Can only scalarize vectors");
  int Cost = 0;

  for (unsigned i = 0, e = Ty->getVectorNumElements(); i < e; ++i) {
    if (Insert)  Cost += TTI.getVectorInstrCost(Instruction::InsertElement,  Ty, i);
    if (Extract) Cost += TTI.getVectorInstrCost(Instruction::ExtractElement, Ty, i);
  }

  return Cost;
}

// Estimate cost of a call instruction CI if it were vectorized with factor VF.
// Return the cost of the instruction, including scalarization overhead if it's
// needed. The flag NeedToScalarize shows if the call needs to be scalarized -
// i.e. either vector version isn't available, or is too expensive.
static int getVectorCallCost(CallInst *CI, unsigned VF, const TargetTransformInfo &TTI,
                             const TargetLibraryInfo *TLI, bool &NeedToScalarize)
{
  Function *F = CI->getCalledFunction();
  StringRef FnName = CI->getCalledFunction()->getName();
  Type *ScalarRetTy = CI->getType();
  SmallVector<Type *, 4> Tys, ScalarTys;
  for (auto &ArgOp : CI->arg_operands())
    ScalarTys.push_back(ArgOp->getType());

  // Estimate cost of scalarized vector call. The source operands are assumed
  // to be vectors, so we need to extract individual elements from there,
  // execute VF scalar calls, and then gather the result into the vector return
  // value.
  int ScalarCallCost = TTI.getCallInstrCost(F, ScalarRetTy, ScalarTys);
  if (VF == 1)
    return ScalarCallCost;

  // Compute corresponding vector type for return value and arguments.
  Type *RetTy = ToVectorTy(ScalarRetTy, VF);
  for (unsigned long long i = 0, ie = ScalarTys.size(); i != ie; ++i)
    Tys.push_back(ToVectorTy(ScalarTys[i], VF));

  // Compute costs of unpacking argument values for the scalar calls and
  // packing the return values to a vector.
  int ScalarizationCost = getScalarizationOverhead(RetTy, true, false, TTI);
  for (unsigned long long i = 0, ie = Tys.size(); i != ie; ++i)
    ScalarizationCost += getScalarizationOverhead(Tys[i], false, true, TTI);

  int Cost = ScalarCallCost * VF + ScalarizationCost;

  // If we can't emit a vector call for this function, then the currently found
  // cost is the cost we need to return.
  NeedToScalarize = true;
  if (!TLI || !TLI->isFunctionVectorizable(FnName, VF) || CI->isNoBuiltin())
    return Cost;

  // If the corresponding vector cost is cheaper, return its cost.
  int VectorCallCost = TTI.getCallInstrCost(nullptr, RetTy, Tys);
  if (VectorCallCost < Cost) {
    NeedToScalarize = false;
    return VectorCallCost;
  }
  return Cost;
}

// Estimate cost of an intrinsic call instruction CI if it were vectorized with
// factor VF.  Return the cost of the instruction, including scalarization
// overhead if it's needed.
static int getVectorIntrinsicCost(CallInst *CI, unsigned VF,
                                  const TargetTransformInfo &TTI,
                                  const TargetLibraryInfo *TLI)
{
  Intrinsic::ID ID = getIntrinsicIDForCall(CI, TLI);
  assert(ID && "Expected intrinsic call!");

  Type *RetTy = ToVectorTy(CI->getType(), VF);
  SmallVector<Type *, 4> Tys;
  for (unsigned i = 0, ie = CI->getNumArgOperands(); i != ie; ++i)
    Tys.push_back(ToVectorTy(CI->getArgOperand(i)->getType(), VF));

  return TTI.getIntrinsicInstrCost(ID, RetTy, Tys);
}

int LoopVectorizationCostModel::getInstructionCost(Instruction* I, unsigned VF) {
  // If we know that this instruction will remain uniform, check the cost of
  // the scalar version.
  if (VecInfo.getVectorShape(*I).isUniform()) {
    VF = 1;
  }

  Type* RetTy = I->getType();
  if (VF > 1 && MinBWs.count(I)) {
    RetTy = IntegerType::get(RetTy->getContext(), (unsigned) MinBWs[I]);
  }
  Type* VectorTy = ToVectorTy(RetTy, VF);

  // TODO: We need to estimate the cost of intrinsic calls.
  switch (I->getOpcode()) {
    case Instruction::GetElementPtr:
      // We mark this instruction as zero-cost because the cost of GEPs in
      // vectorized code depends on whether the corresponding memory instruction
      // is scalarized or not. Therefore, we handle GEPs with the memory
      // instruction cost.
      return 0;
    case Instruction::Br: {
      return TTI.getCFInstrCost(I->getOpcode());
    }
    case Instruction::PHI:
      //TODO: IF-converted IFs become selects.
      return 0;
    case Instruction::Add:
    case Instruction::FAdd:
    case Instruction::Sub:
    case Instruction::FSub:
    case Instruction::Mul:
    case Instruction::FMul:
    case Instruction::UDiv:
    case Instruction::SDiv:
    case Instruction::FDiv:
    case Instruction::URem:
    case Instruction::SRem:
    case Instruction::FRem:
    case Instruction::Shl:
    case Instruction::LShr:
    case Instruction::AShr:
    case Instruction::And:
    case Instruction::Or:
    case Instruction::Xor: {
      // Since we will replace the stride by 1 the multiplication should go away.
      // FIXME: For some reason this is later replaced by a constant vector of 1 in the llvm lv
      //if (I->getOpcode() == Instruction::Mul && isStrideMul(I, Legal)) {
      //  return 0;
      //}

      // Certain instructions can be cheaper to vectorize if they have a constant
      // second vector operand. One example of this are shifts on x86.
      TargetTransformInfo::OperandValueKind Op1VK = TargetTransformInfo::OK_AnyValue;
      TargetTransformInfo::OperandValueKind Op2VK = TargetTransformInfo::OK_AnyValue;
      TargetTransformInfo::OperandValueProperties Op1VP = TargetTransformInfo::OP_None;
      TargetTransformInfo::OperandValueProperties Op2VP = TargetTransformInfo::OP_None;
      Value* Op2 = I->getOperand(1);

      // Check for a splat of a constant or for a non uniform vector of constants.
      if (isa<ConstantInt>(Op2)) {
        ConstantInt* CInt = cast<ConstantInt>(Op2);
        if (CInt && CInt->getValue().isPowerOf2()) {
          Op2VP = TargetTransformInfo::OP_PowerOf2;
        }
        Op2VK = TargetTransformInfo::OK_UniformConstantValue;
      }
      else if (isa<ConstantVector>(Op2) || isa<ConstantDataVector>(Op2)) {
        Op2VK = TargetTransformInfo::OK_NonUniformConstantValue;
        Constant* SplatValue = cast<Constant>(Op2)->getSplatValue();
        if (SplatValue) {
          ConstantInt* CInt = dyn_cast<ConstantInt>(SplatValue);
          if (CInt && CInt->getValue().isPowerOf2()) {
            Op2VP = TargetTransformInfo::OP_PowerOf2;
          }
          Op2VK = TargetTransformInfo::OK_UniformConstantValue;
        }
      }

      return TTI.getArithmeticInstrCost(I->getOpcode(), VectorTy, Op1VK, Op2VK, Op1VP, Op2VP);
    }
    case Instruction::Select: {
      SelectInst* SI = cast<SelectInst>(I);
      const SCEV* CondSCEV = SE->getSCEV(SI->getCondition());
      bool ScalarCond = (SE->isLoopInvariant(CondSCEV, TheLoop));
      Type* CondTy = SI->getCondition()->getType();
      if (!ScalarCond) {
        CondTy = VectorType::get(CondTy, VF);
      }

      return TTI.getCmpSelInstrCost(I->getOpcode(), VectorTy, CondTy);
    }
    case Instruction::ICmp:
    case Instruction::FCmp: {
      Type* ValTy = I->getOperand(0)->getType();
      Instruction* Op0AsInstruction = dyn_cast<Instruction>(I->getOperand(0));
      auto It = MinBWs.find(Op0AsInstruction);
      if (VF > 1 && It != MinBWs.end()) {
        ValTy = IntegerType::get(ValTy->getContext(), It->second);
      }
      VectorTy = ToVectorTy(ValTy, VF);
      return TTI.getCmpSelInstrCost(I->getOpcode(), VectorTy);
    }
    case Instruction::Store:
    case Instruction::Load: {
      // TODO implement
      return 0;

#if 0
      StoreInst* SI = dyn_cast<StoreInst>(I);
      LoadInst* LI = dyn_cast<LoadInst>(I);
      Type* ValTy = (SI ? SI->getValueOperand()->getType() : LI->getType());
      VectorTy = ToVectorTy(ValTy, VF);

      unsigned Alignment = SI ? SI->getAlignment() : LI->getAlignment();
      unsigned AS = SI ? SI->getPointerAddressSpace() : LI->getPointerAddressSpace();
      Value* Ptr = SI ? SI->getPointerOperand() : LI->getPointerOperand();
      // We add the cost of address computation here instead of with the gep
      // instruction because only here we know whether the operation is
      // scalarized.
      if (VF == 1) {
        return TTI.getAddressComputationCost(VectorTy) +
               TTI.getMemoryOpCost(I->getOpcode(), VectorTy, Alignment, AS);
      }

      // For an interleaved access, calculate the total cost of the whole
      // interleave group.
      if (Legal->isAccessInterleaved(I)) {
        auto Group = Legal->getInterleavedAccessGroup(I);
        assert(Group && "Fail to get an interleaved access group.");

        // Only calculate the cost once at the insert position.
        if (Group->getInsertPos() != I) {
          return 0;
        }

        unsigned InterleaveFactor = Group->getFactor();
        Type* WideVecTy = VectorType::get(VectorTy->getVectorElementType(),
                                          VectorTy->getVectorNumElements() * InterleaveFactor);

        // Holds the indices of existing members in an interleaved load group.
        // An interleaved store group doesn't need this as it dones't allow gaps.
        SmallVector<unsigned, 4> Indices;
        if (LI) {
          for (unsigned i = 0; i < InterleaveFactor; i++) {
            if (Group->getMember(i)) {
              Indices.push_back(i);
            }
          }
        }

        // Calculate the cost of the whole interleaved group.
        int Cost = TTI.getInterleavedMemoryOpCost(I->getOpcode(), WideVecTy, Group->getFactor(),
                                                  Indices, Group->getAlignment(), AS);

        if (Group->isReverse()) {
          Cost += Group->getNumMembers() *
                  TTI.getShuffleCost(TargetTransformInfo::SK_Reverse, VectorTy, 0);
        }

        // FIXME: The interleaved load group with a huge gap could be even more
        // expensive than scalar operations. Then we could ignore such group and
        // use scalar operations instead.
        return Cost;
      }

      // Scalarized loads/stores.
      const rv::VectorShape& shape = VecInfo.getVectorShape(*Ptr);
      int Stride = shape.hasStridedShape() ? shape.getStride() : 0;
      int ConsecutiveStride = Stride == 1 || Stride == -1 ? Stride : 0;
      bool Reverse = ConsecutiveStride < 0;
      const DataLayout& DL = I->getModule()->getDataLayout();
      unsigned long long ScalarAllocatedSize = DL.getTypeAllocSize(ValTy);
      unsigned long long VectorElementSize   = DL.getTypeStoreSize(VectorTy) / VF;
      if (!ConsecutiveStride || ScalarAllocatedSize != VectorElementSize) {
        bool IsComplexComputation = true;
        // FIXME isLikelyComplexAddressComputation(Ptr, Legal, SE, TheLoop);
        unsigned Cost = 0;
        // The cost of extracting from the value vector and pointer vector.
        Type* PtrTy = ToVectorTy(Ptr->getType(), VF);
        for (unsigned i = 0; i < VF; ++i) {
          //  The cost of extracting the pointer operand.
          Cost += TTI.getVectorInstrCost(Instruction::ExtractElement, PtrTy, i);
          // In case of STORE, the cost of ExtractElement from the vector.
          // In case of LOAD, the cost of InsertElement into the returned
          // vector.
          Cost += TTI.getVectorInstrCost(SI ? Instruction::ExtractElement :
                                         Instruction::InsertElement, VectorTy, i);
        }

        // The cost of the scalar loads/stores.
        Cost += VF * TTI.getAddressComputationCost(PtrTy, IsComplexComputation);
        Cost += VF * TTI.getMemoryOpCost(I->getOpcode(), ValTy->getScalarType(), Alignment, AS);
        return Cost;
      }

      // Wide load/stores.
      int Cost = TTI.getAddressComputationCost(VectorTy);
      if (Legal->isMaskRequired(I)) {
        Cost += TTI.getMaskedMemoryOpCost(I->getOpcode(), VectorTy, Alignment, AS);
      }
      else {
        Cost += TTI.getMemoryOpCost(I->getOpcode(), VectorTy, Alignment, AS);
      }

      if (Reverse) {
        Cost += TTI.getShuffleCost(TargetTransformInfo::SK_Reverse, VectorTy, 0);
      }
      return Cost;
#endif
    }
    case Instruction::ZExt:
    case Instruction::SExt:
    case Instruction::FPToUI:
    case Instruction::FPToSI:
    case Instruction::FPExt:
    case Instruction::PtrToInt:
    case Instruction::IntToPtr:
    case Instruction::SIToFP:
    case Instruction::UIToFP:
    case Instruction::Trunc:
    case Instruction::FPTrunc:
    case Instruction::BitCast: {
      // FIXME: what do we do in this scenario?
      // We optimize the truncation of induction variable.
      // The cost of these is the same as the scalar operation.
      //if (I->getOpcode() == Instruction::Trunc && Legal->isInductionVariable(I->getOperand(0))) {
      //  return TTI.getCastInstrCost(I->getOpcode(), I->getType(), I->getOperand(0)->getType());
      //}

      Type* SrcScalarTy = I->getOperand(0)->getType();
      Type* SrcVecTy = ToVectorTy(SrcScalarTy, VF);
      if (VF > 1 && MinBWs.count(I)) {
        // This cast is going to be shrunk. This may remove the cast or it might
        // turn it into slightly different cast. For example, if MinBW == 16,
        // "zext i8 %1 to i32" becomes "zext i8 %1 to i16".
        //
        // Calculate the modified src and dest types.
        Type* MinVecTy = VectorTy;
        if (I->getOpcode() == Instruction::Trunc) {
          SrcVecTy = smallestIntegerVectorType(SrcVecTy, MinVecTy);
          VectorTy = largestIntegerVectorType(ToVectorTy(I->getType(), VF), MinVecTy);
        }
        else if (I->getOpcode() == Instruction::ZExt || I->getOpcode() == Instruction::SExt) {
          SrcVecTy = largestIntegerVectorType(SrcVecTy, MinVecTy);
          VectorTy = smallestIntegerVectorType(ToVectorTy(I->getType(), VF), MinVecTy);
        }
      }

      return TTI.getCastInstrCost(I->getOpcode(), VectorTy, SrcVecTy);
    }
    case Instruction::Call: {
      bool NeedToScalarize;
      CallInst* CI = cast<CallInst>(I);
      int CallCost = getVectorCallCost(CI, VF, TTI, TLI, NeedToScalarize);
      if (getIntrinsicIDForCall(CI, TLI)) {
        return std::min(CallCost, getVectorIntrinsicCost(CI, VF, TTI, TLI));
      }
      return CallCost;
    }
    default: {
      // We are scalarizing the instruction. Return the cost of the scalar
      // instruction, plus the cost of insert and extract into vector
      // elements, times the vector width.
      unsigned Cost = 0;

      if (!RetTy->isVoidTy() && VF != 1) {
        int InsCost = TTI.getVectorInstrCost(Instruction::InsertElement, VectorTy);
        int ExtCost = TTI.getVectorInstrCost(Instruction::ExtractElement, VectorTy);

        // The cost of inserting the results plus extracting each one of the
        // operands.
        Cost += VF * (InsCost + ExtCost * I->getNumOperands());
      }

      // The cost of executing VF copies of the scalar instruction. This opcode
      // is unknown. Assume that it is the same as 'mul'.
      Cost += VF * TTI.getArithmeticInstrCost(Instruction::Mul, VectorTy);
      return Cost;
    }
  }// end of switch.
}

}
