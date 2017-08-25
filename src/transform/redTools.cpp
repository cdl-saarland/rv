#include "rv/transform/redTools.h"

using namespace llvm;

namespace rv {

static
Instruction&
CreateMinMax(IRBuilder<> & builder, Value & A, Value & B, bool createMin) {
  auto * aTy = A.getType();
  bool isFloat = aTy->isFPOrFPVectorTy();

  Value * cmpInst;
  if (isFloat) {
    cmpInst = builder.CreateFCmpOGT(&A, &B);
  } else {
    cmpInst = builder.CreateICmpSGT(&A, &B);
  }
  return *cast<Instruction>(builder.CreateSelect(cmpInst, createMin ? &B : &A, createMin ? &A : &B));
}

// materialize a single instance of firstArg [[RedKind~OpCode]] secondArg
Instruction&
CreateReductInst(IRBuilder<> & builder, RedKind redKind, Value & firstArg, Value & secondArg) {
  auto * argTy = firstArg.getType();
  bool isFloat = argTy->isFPOrFPVectorTy();

  // TODO infer NSW,NUW, fp math flags
  switch (redKind) {
    case RedKind::Add:
      if (isFloat) {
        return *cast<Instruction>(builder.CreateFAdd(&firstArg, &secondArg, secondArg.getName() + ".r"));
      } else {
        return *cast<Instruction>(builder.CreateAdd(&firstArg, &secondArg, secondArg.getName() + ".r"));
      }

    case RedKind::Or:
        return *cast<Instruction>(builder.CreateOr(&firstArg, &secondArg, secondArg.getName() + ".r"));
    case RedKind::And:
        return *cast<Instruction>(builder.CreateAnd(&firstArg, &secondArg, secondArg.getName() + ".r"));

    case RedKind::Mul:
      if (isFloat) {
        return *cast<Instruction>(builder.CreateFMul(&firstArg, &secondArg, secondArg.getName() + ".r"));
      } else {
        return *cast<Instruction>(builder.CreateMul(&firstArg, &secondArg, secondArg.getName() + ".r"));
      }

    case RedKind::Max:
      return CreateMinMax(builder, firstArg, secondArg, false);

    case RedKind::Min:
      return CreateMinMax(builder, firstArg, secondArg, true);

    default:
      abort(); // unsupported reduction
  }
}

static
bool
IsPower2(int x) {
  return x && !(x & (x - 1));
}

static Type&
GetScalarType(Value & val) {
  auto * valTy = val.getType();
  if (valTy->isVectorTy()) return *valTy->getVectorElementType();
  else return *valTy;
}

// reduce the vector @vectorVal to a scalar value (using redKind)
Value &
CreateVectorReduce(IRBuilder<> & builder, RedKind redKind, Value & vecVal, Value * initVal) {
  uint vecWidth = vecVal.getType()->getVectorNumElements();

  auto * intTy = Type::getInt32Ty(builder.getContext());

  // TODO from LLVM >= 5.0 use reduction intrinsics
  if (IsPower2(vecWidth)) {
    auto * accu = &vecVal;
    for (size_t range = vecWidth / 2; range >= 1; range /= 2) {
      // create a permutation vector
      std::vector<Constant*> shuffleVec;
      shuffleVec.reserve(vecWidth);

      // 4 5 6 7 * * * *
      // 2 3 * * * * * *
      // 1 * * * * * * *
      for (size_t i = 0; i < range; ++i) {
        shuffleVec.push_back(ConstantInt::getSigned(intTy, range + i));
      }
      // fill up with undef elements
      while (shuffleVec.size() < vecWidth) shuffleVec.push_back( UndefValue::get(intTy) );

      // fold
      auto * mask = ConstantVector::get(shuffleVec);
      auto * folded = builder.CreateShuffleVector(accu, UndefValue::get(vecVal.getType()), mask, "fold");

      // Create reduction
      accu = &CreateReductInst(builder, redKind, *accu, *folded);
    }

    Value * reducedVec = builder.CreateExtractElement(accu, ConstantInt::getNullValue(intTy), "reduce_last");

    if (initVal && initVal != &GetNeutralElement(redKind, *reducedVec->getType())) {
      return CreateReductInst(builder, redKind, *reducedVec, *initVal);
    } else {
      return *reducedVec;
    }

  } else {
    // create a scalar reduction chain
    Value * accu = initVal ? initVal : &GetNeutralElement(redKind, GetScalarType(vecVal));

    for (size_t i = 0; i < vecWidth; ++i) {
      auto * laneVal = builder.CreateExtractElement(&vecVal, i, "red_ext");
      accu = &CreateReductInst(builder, redKind, *accu, *laneVal);
    }

    return *accu;
  }
}

Value &
CreateExtract(IRBuilder<> & builder, Value & vecVal, int laneOffset) {
  auto * vecTy = dyn_cast<VectorType>(vecVal.getType());
  if (!vecTy) {
    return vecVal; //uniform value
  }

  const int vectorWidth = vecTy->getNumElements();
  int laneIdx = laneOffset >= 0 ? laneOffset : vectorWidth + laneOffset;
  assert(laneIdx >= 0 && laneIdx < vectorWidth);

  return *builder.CreateExtractElement(&vecVal, laneIdx, vecVal.getName().str() + ".ex." + std::to_string(laneIdx));
}


}
