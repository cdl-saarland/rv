//===- rvTools.cpp ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @author karrenberg
//

#include "rvTools.h"

#include <sstream>   // stringstream
#include <stdexcept> // logic_error
#include <map>

#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/DerivedTypes.h> // VectorType
#include <llvm/IR/Constants.h>    // UndefValue
#include <llvm/IR/Instructions.h> // BitCastInst
#include <llvm/IR/Intrinsics.h>
#include <llvm/IR/Attributes.h>
#include <llvm/Analysis/LoopInfo.h> // Loop

#include <llvm/Bitcode/ReaderWriter.h> // ParseBitcodeFile
#include <llvm/Support/MemoryBuffer.h> // MemoryBuffer
#include <llvm/IRReader/IRReader.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/FileSystem.h>

#include <llvm/ADT/SmallVector.h>
#include <llvm/Support/raw_ostream.h>

#include "rvConfig.h"
#include "rv/rvInfo.h"
#include "metadata.h"

#include "analysis/analysisCfg.h"

using namespace llvm;


typedef std::map<const llvm::Type*,llvm::Type*> TypeMap;

static TypeMap scalarToSimdTypes;
static TypeMap simdToScalarTypes;

void
rv::DumpPath(const rv::PathType & path) {
	bool later = false;
	llvm::errs() << "Path (";
	for (auto block : path) {
		if (later) llvm::errs() << ", ";
		llvm::errs() << block->getName();
		later = true;
	}
	llvm::errs() << ")\n";
}


static
Type*
lookupSimdType(const llvm::Type * scalarType) {
	auto it = scalarToSimdTypes.find(scalarType);
	if (it == scalarToSimdTypes.end()) {
		return nullptr;
	}
	return it->second;
}

static
Type*
lookupScalarType(const llvm::Type * simdType) {
	auto it = simdToScalarTypes.find(simdType);
	if (it == simdToScalarTypes.end()) {
		return nullptr;
	}
	return it->second;
}

static
void
cacheTypePair(llvm::Type * scalarType, llvm::Type * simdType) {
	scalarToSimdTypes[scalarType] = simdType;
	simdToScalarTypes[simdType] = scalarType;
}


bool
rv::IsFlatPointer(const Type & type) {
	auto * ptrTy = dyn_cast<const PointerType>(&type);
	if (ptrTy) {
		auto * elemTy = ptrTy->getPointerElementType();
		if (IsPrimitiveType(*elemTy)) {
			return true;
		}
	}

	return false;
}

bool
rv::IsPrimitiveType(const llvm::Type & type) {
	if (type.isIntegerTy())
		return true;
	else if (type.isFloatTy())
		return true;
	else
		return type.getTypeID() <= Type::X86_MMXTyID; // FIXME Type::isPrimitiveType() is no longer available
#if 0
	// assert(false && "implement!");
	if (type.getNumContainedTypes() > 0)
	if (! type.isFirstClassType()) return false;
	if (type.isAggregateType()) return false;
	if (type.isVectorTy()) return false;
	return true;
#endif
}

// Mutate type of original value and
// replace its uses by the new value.
void
rv::uncheckedReplaceAllUsesWith(Value* value, Value* with)
{
    Type* oldType = value->getType();
    Type* newType = with->getType();
    value->mutateType(newType);
    value->replaceAllUsesWith(with);
    value->mutateType(oldType);
    assert (value->use_empty());
}


// This function defines what we consider matching types
// in terms of uniform/varying.
bool
rv::typesMatch(Type* t1, Type* t2)
{
    assert (t1 && t2);
    if (t1 == t2) return true;

    if (t1->getTypeID() != t2->getTypeID()) return false;

    // check additional possibilities:
    // - structurally equivalent struct types
    // - i8* -> arbitrary pointer
    // - vector types of same element type and same bit size, e.g.:
    // - <16 x i8> == <2 x i64>
    // - <4 x float> == <2 x double>
    // - structs with mixed uniform/varying elements (TODO: should we disallow these?)
#define MATCH_RETURN(TEST) { bool res = TEST; errs() << " }="<< (res ? "yes" : "no") << "\n"; return res; }

    errs() << "Types match? " << *t1 << " vs " << *t2 << " { ";
    switch (t1->getTypeID())
    {
        case Type::VectorTyID:
        {
            const unsigned elems1 = t1->getVectorNumElements();
            const unsigned elems2 = t2->getVectorNumElements();
            const unsigned bitSize1 = t1->getVectorElementType()->getScalarSizeInBits() * elems1;
            const unsigned bitSize2 = t2->getVectorElementType()->getScalarSizeInBits() * elems2;

            if (t1->getVectorElementType()->isFloatingPointTy())
            {
            	MATCH_RETURN(t2->getVectorElementType()->isFloatingPointTy() && bitSize1 == bitSize2)
            }
            else
            {
                assert (t1->getVectorElementType()->isIntegerTy());
                MATCH_RETURN(t2->getVectorElementType()->isIntegerTy() && bitSize1 == bitSize2)
            }

            MATCH_RETURN(false)
        }
        case Type::PointerTyID:
        {
            // if one of the types is a void pointer, any pointer is allowed to match.
            if (t1->getPointerElementType()->isIntegerTy(8)) return true;
            if (t2->getPointerElementType()->isIntegerTy(8)) return true;

            MATCH_RETURN(typesMatch(t1->getPointerElementType(), t2->getPointerElementType()))
        }
        case Type::ArrayTyID:
        {
            if (t1->getArrayNumElements() != t2->getArrayNumElements()) MATCH_RETURN(false)

				MATCH_RETURN(typesMatch(t1->getArrayElementType(), t2->getArrayElementType()))
        }
        case Type::StructTyID:
        {
            StructType* sType1 = cast<StructType>(t1);
            StructType* sType2 = cast<StructType>(t2);
            if (sType1->isLayoutIdentical(sType2)) return true;
            if (sType1->getNumContainedTypes() != sType2->getNumContainedTypes()) return false;

            for (unsigned i=0; i<sType1->getNumContainedTypes(); ++i)
            {
                const bool elemVerified =
                    typesMatch(sType1->getElementType(i), sType2->getElementType(i));
                if (!elemVerified) MATCH_RETURN(false)
            }

            MATCH_RETURN(true)
        }
        default:
        {
        	MATCH_RETURN(false)
        }
    }

    MATCH_RETURN(false)

#undef MATCH_RETURN
}

// Returns 0 if this is a struct type with different factors.
// Returns the vectorization factor otherwise (1 for non-vectorized type).
unsigned
rv::getVectorizationFactor(const Type& type)
{
    switch (type.getTypeID())
    {
        case Type::VectorTyID:
        {
            return type.getVectorNumElements();
        }
        case Type::PointerTyID:
        {
            return getVectorizationFactor(*type.getPointerElementType());
        }
        case Type::ArrayTyID:
        {
            return getVectorizationFactor(*type.getArrayElementType());
        }
        case Type::StructTyID:
        {
            unsigned factor = 1;
            for (unsigned i=0; i<type.getStructNumElements(); ++i)
            {
                const unsigned elemFactor =
                    getVectorizationFactor(*type.getStructElementType(i));

                // If we have disagreeing factors, we indicate an error.
                if (factor > 1 && factor != elemFactor) return 0;
                factor = elemFactor;
            }
            return factor;
        }
        default:
        {
            return 1;
        }
    }
}

bool
rv::typesMatchPumped(const Type& t1, const Type& t2)
{
    if (&t1 == &t2) return true;

    if (t1.getTypeID() != t2.getTypeID()) return false;

    // By computing the factors here instead of only at vector
    // "leaf level" we do redundant calculations but gain a more
    // precise result in that we can be sure there are no disagreeing
    // factors hidden somewhere. It is not necessary to do that on
    // every recursion level but only at the "top level", but I don't
    // care right now.
    const unsigned factor1 = getVectorizationFactor(t1);
    const unsigned factor2 = getVectorizationFactor(t2);

    // Make sure we could determine the factor for both types.
    if (factor1 == 0 || factor2 == 0) return false;

    // Make sure one factor is divisible by the other.
    if (factor1 != factor2 &&
        factor1 % factor2 != 0 &&
        factor2 % factor1 != 0)
    {
        return false;
    }

    switch (t1.getTypeID())
    {
        case Type::VectorTyID:
        {
            return (t1.getVectorElementType()->isFloatingPointTy() &&
                    t2.getVectorElementType()->isFloatingPointTy()) ||
                   (t1.getVectorElementType()->isIntegerTy() &&
                    t2.getVectorElementType()->isIntegerTy());
        }
        case Type::PointerTyID:
        {
            // if one of the types is a void pointer, any pointer is allowed to match.
            if (t1.getPointerElementType()->isIntegerTy(8)) return true;
            if (t2.getPointerElementType()->isIntegerTy(8)) return true;

            return typesMatchPumped(*t1.getPointerElementType(),
                                    *t2.getPointerElementType());
        }
        case Type::ArrayTyID:
        {
            if (t1.getArrayNumElements() != t2.getArrayNumElements()) return false;

            return typesMatchPumped(*t1.getArrayElementType(),
                                    *t2.getArrayElementType());
        }
        case Type::StructTyID:
        {
            const StructType& sType1 = cast<StructType>(t1);
            const StructType& sType2 = cast<StructType>(t2);
            // ARGH, isLayoutIdentical does not take a const parameter...
            if (sType1.isLayoutIdentical(const_cast<StructType*>(&sType2))) return true;
            if (sType1.getNumContainedTypes() != sType2.getNumContainedTypes()) return false;

            for (unsigned i=0; i<sType1.getNumContainedTypes(); ++i)
            {
                const bool validElem =
                    typesMatchPumped(*sType1.getElementType(i),
                                     *sType2.getElementType(i));

                if (!validElem) return false;
            }

            return true;
        }
        default:
        {
            return false;
        }
    }

    return false;
}

bool
rv::HasVaryingBranch(const llvm::BasicBlock & block, const VectorizationInfo& vecInfo) {
	const llvm::BranchInst*  br          = llvm::dyn_cast<llvm::BranchInst>(block.getTerminator());
	return (br && !vecInfo.getVectorShape(*br).isUniform());
}


// Find out which parameters of the scalar function remain scalar in the simd function.
void
rv::findUniformArguments(const Function&          scalarFn,
                          const Function&          vectorFn,
                          SmallVector<bool, 4>&    uniformArgs,
                          const rv::ValueInfoMap* valueInfoMap,
                          const int                maskIndex)
{
    uniformArgs.resize(scalarFn.getArgumentList().size(), false);

    int i = 0;
    Function::const_arg_iterator A_SCALAR = scalarFn.arg_begin();

    for (Function::const_arg_iterator A = vectorFn.arg_begin(),
            AE = vectorFn.arg_end(); A != AE; ++A, ++A_SCALAR, ++i)
    {
        if (i == maskIndex) continue;
        uniformArgs[i] = rv::typesMatch(A_SCALAR->getType(), A->getType());
        if (uniformArgs[i]) errs() << *A_SCALAR << " to " << *A << " uniform\n";
        if (!valueInfoMap || !uniformArgs[i]) continue;
        if (!valueInfoMap->hasMapping(*A_SCALAR)) continue;
        uniformArgs[i] = valueInfoMap->get(*A_SCALAR).mIsResultUniform;
    }
}

bool
rv::hasUniformReturn(const Function& scalarFn,
                      const Function& vectorFn)
{
    return rv::typesMatch(scalarFn.getReturnType(), vectorFn.getReturnType());
}

// NOTE: i8* is vectorizable only if it is not a void-pointer!
//       Thus, this code has to make sure to catch all void-pointer
//       producing structures:
//       - inttoptr
//       - bitcast to i8*
bool
rv::returnsVoidPtr(const Instruction& inst)
{
    if (!isa<CastInst>(inst)) return false;
    if (!inst.getType()->isPointerTy()) return false;

    return inst.getType()->getPointerElementType()->isIntegerTy(8);
}

bool
rv::isVectorizableInst(const Instruction& inst)
{
    switch (inst.getOpcode())
    {
        case Instruction::Switch:
        case Instruction::IndirectBr:
        case Instruction::Resume:
        case Instruction::Unreachable:
        case Instruction::AtomicCmpXchg:
        case Instruction::AtomicRMW:
        case Instruction::Fence:
        case Instruction::PtrToInt:
        case Instruction::IntToPtr:
        case Instruction::VAArg:
        case Instruction::ExtractElement:
        case Instruction::InsertElement:
        case Instruction::ShuffleVector:
        case Instruction::LandingPad:
        case Instruction::Invoke:
        {
            return false;
        }
        case Instruction::BitCast:
        {
            if (returnsVoidPtr(inst)) return false;
        }
    }

    return inst.getType()->isVoidTy() || isVectorizableType(*inst.getType());
}

bool
rv::isVectorizableNonDerivedType(const Type& type)
{
    return type.getPrimitiveSizeInBits() <= 32 && (type.isFloatingPointTy() || type.isIntegerTy()); // FIXME query the simdModel instead
}

bool
rv::isVectorizableType(const Type& type)
{
    // First check most common types.
    if (type.isVoidTy()) return false;
    if (isVectorizableNonDerivedType(type)) return true;

    Type::TypeID typeID = type.getTypeID();

    // If none of these, test for primitive types.
    if (rv::IsPrimitiveType(type)) return false;

    // If also none of these, test derived types.
    switch (typeID)
    {
        case Type::VectorTyID:
        {
            return false;
        }
        case Type::PointerTyID:
        {
            return true;
        }
        case Type::ArrayTyID:
        {
            return isVectorizableType(*cast<ArrayType>(type).getElementType());
        }
        case Type::StructTyID:
        {
            const StructType& sType = cast<StructType>(type);
            for (unsigned i=0; i<sType.getNumContainedTypes(); ++i)
            {
                if (!isVectorizableType(*sType.getElementType(i))) return false;
            }
            return true;
        }
        default:
        {
            return false;
        }
    }
}

// This function only performs a check if the given type is a valid
// vector type, applying the type rules (e.g. SoA layout)
bool
rv::isVectorizedType(const Type& type)
{
    Type::TypeID oldTypeID = type.getTypeID();

    if (IsPrimitiveType(type)) return false;

    // If none of these, test derived types.
    switch (oldTypeID)
    {
        case Type::VectorTyID:
        {
            return true;
        }
        case Type::PointerTyID:
        {
            return isVectorizedType(*cast<PointerType>(type).getElementType());
        }
        case Type::ArrayTyID:
        {
            return isVectorizedType(*cast<ArrayType>(type).getElementType());
        }
        case Type::StructTyID:
        {
            const StructType& sType = cast<StructType>(type);
            for (unsigned i=0; i<sType.getNumContainedTypes(); ++i)
            {
                if (!isVectorizedType(*sType.getElementType(i))) return false;
            }
            return true;
        }
        default:
        {
            return false;
        }
    }
}

bool
rv::hasNestedPointer(const Type& type, const bool inNest)
{
    Type::TypeID oldTypeID = type.getTypeID();

    if (rv::IsPrimitiveType(type)) return false;

    switch (oldTypeID)
    {
        case Type::VectorTyID:
        {
            return false;
        }
        case Type::PointerTyID:
        {
            if (inNest) return true;
            return hasNestedPointer(*type.getPointerElementType(), true);
        }
        case Type::ArrayTyID:
        {
            return hasNestedPointer(*type.getArrayElementType(), inNest);
        }
        case Type::StructTyID:
        {
            for (unsigned i=0; i<type.getStructNumElements(); ++i)
            {
                if (hasNestedPointer(*type.getStructElementType(i)), inNest) return true;
            }
            return false;
        }
        default:
        {
            return true;
        }
    }
}

Value *
rv::createPointerCast(Value * pointer, unsigned factor, Instruction * insertBefore) {
	assert (pointer);

    Type* oldType = pointer->getType();
    if (rv::isVectorizedType(*oldType)) return pointer;

    Type* newType = rv::vectorizeSIMDType(oldType, factor);

    BitCastInst* pktPtrCast = new BitCastInst(pointer,
                                              newType,
                                              "pktPtrCast",
                                              insertBefore);

    rv::setMetadata(pktPtrCast, rv::RV_METADATA_PKT_PTR_CAST);
    rv::setMetadata(pktPtrCast, rv::RV_METADATA_RES_VECTOR);
    rv::setMetadata(pktPtrCast, rv::RV_METADATA_OP_UNIFORM);
    rv::setMetadata(pktPtrCast, rv::RV_METADATA_INDEX_CONSECUTIVE);
    if (rv::hasMetadata(pointer, rv::RV_METADATA_ALIGNED_TRUE))
        rv::setMetadata(pktPtrCast, rv::RV_METADATA_ALIGNED_TRUE);
    else
        rv::setMetadata(pktPtrCast, rv::RV_METADATA_ALIGNED_FALSE);

    IF_DEBUG {
      outs() << "  inserted new pointer cast: "
            << *pktPtrCast << "\n";
    }

    return pktPtrCast;
}

Value*
rv::InsertBroadcast(Value * value, unsigned simdFactor, Instruction & insertBefore) {
	Type * valType = value->getType();

	if (valType->isVectorTy()) {
		assert (valType->getVectorNumElements() == simdFactor);
		return value;
	}

	if (Constant * uniConst = dyn_cast<Constant>(value)) {
		return ConstantVector::getSplat(simdFactor, uniConst);

	} else {
		VectorType * vecType = VectorType::get(valType, simdFactor);
		Type * idxType = Type::getInt8Ty(value->getContext());

		Value * lastVec = ConstantVector::getNullValue(vecType);
		for (unsigned i = 0; i < simdFactor; ++i) {
			 Instruction * insertInst = InsertElementInst::Create(lastVec, value, ConstantInt::get(idxType, i), "", &insertBefore);
			 rv::markMaskOperation(insertInst);
			 lastVec = insertInst;
		}
		return lastVec;
	}
}

// Create SIMD type from scalar type.
// -> only 32bit-float, integers <= 32bit, pointers, arrays and structs allowed
// -> no scalar datatypes allowed
// -> no pointers to pointers allowed
// TODO: i8* should not be transformed to <4 x i8>* !
Type*
rv::vectorizeSIMDType(Type* scalarType, const unsigned vectorizationFactor)
{
	Type * simdType = lookupSimdType(scalarType);
	if (simdType) return simdType;

    if (scalarType->isIntegerTy() || scalarType->isFloatingPointTy())
    {
    	simdType = VectorType::get(scalarType, vectorizationFactor);
    	cacheTypePair(scalarType, simdType);
    	return simdType;
    }

    switch (scalarType->getTypeID())
    {
        case Type::PointerTyID:
        {
            PointerType* pType = cast<PointerType>(scalarType);

#if 0
            // NOTE: This could be used if we use vectors of pointers.
            //       However, it is usually not clear whether we want to
            //       return <W x i32>* or <W x i32*> for i32 even at the
            //       callsite of this function.
            // We never create vectors of pointers for derived types,
            // only <W x iX*>, <W x float*>, <W x double*> etc. are allowed.
            if (createPtrVec &&
                (pType->getElementType()->isFloatingPointTy() ||
                 pType->getElementType()->isIntegerTy()))
            {
                return VectorType::get(scalarType, vectorizationFactor);
            }
#endif

            simdType = PointerType::get(vectorizeSIMDType(pType->getElementType(), vectorizationFactor),
                                    pType->getAddressSpace());
        } break;
        case Type::ArrayTyID:
        {
            ArrayType* aType = cast<ArrayType>(scalarType);
            simdType = ArrayType::get(vectorizeSIMDType(aType->getElementType(), vectorizationFactor),
                                  aType->getNumElements());
        } break;
        case Type::StructTyID:
        {
            StructType* sType = cast<StructType>(scalarType);
            std::vector<Type*> newParams;
            for (unsigned i=0; i<sType->getNumContainedTypes(); ++i)
            {
                newParams.push_back(vectorizeSIMDType(sType->getElementType(i), vectorizationFactor));
            }
            simdType = StructType::get(scalarType->getContext(), newParams, sType->isPacked());
        } break;

        default:
        {
            errs() << "\nERROR: only arguments of type float, int, pointer, "
                    << "array or struct can be vectorized, not '"
                    << *scalarType << "'!\n";
            throw std::logic_error("INTERNAL ERROR: Could not vectorize type!");
        } break;
    }

    cacheTypePair(scalarType, simdType);
    return simdType;
}

// Returns the element type for a splitting operation.
// The input type is required to be vectorized, meaning that e.g. on
// leaf level of a struct, the underlying type is a vector (vector)
// and never a scalar.
// NOTE: This basically applies the vectorization rules in reverse.
// EXAMPLE: { <4 x float>, [ <4 x float>, <4 x float> ], <4 x int> }
//          -> { float, [ float, float ], int }
Type*
rv::getScalarFromVectorizedType(Type* type)
{
	Type * scalarType = lookupScalarType(type);
	if (scalarType) return scalarType;

    assert (isVectorizedType(*type) &&
            "can not extract element type from non-vectorized type!");

    switch(type->getTypeID())
    {
        case Type::VectorTyID:
        {
            VectorType* vType = cast<VectorType>(type);
            scalarType = vType->getScalarType();
        } break;
        case Type::ArrayTyID:
        {
            ArrayType* aType = cast<ArrayType>(type);
            Type* newElementType = getScalarFromVectorizedType(aType->getElementType());
            scalarType = ArrayType::get(newElementType, aType->getNumElements());
        } break;
        case Type::StructTyID:
        {
            StructType* sType = cast<StructType>(type);

            // create struct-type with extracted elements
            std::vector<Type*> params;
            for (StructType::subtype_iterator ST=sType->subtype_begin(),
                    STE=sType->subtype_end(); ST!=STE; ++ST)
            {
                Type* subType = getScalarFromVectorizedType(*ST);
                assert (subType &&
                        "can not extract element type: struct field is not vectorized!");
                params.push_back(subType);
            }

            scalarType = StructType::get(type->getContext(), params, sType->isPacked());
        } break;
        case Type::PointerTyID:
        {
            PointerType* pType = cast<PointerType>(type);

            Type* subType = getScalarFromVectorizedType(pType->getElementType());
            assert (subType &&
                    "can not extract element type: struct field is not vectorized!");
            scalarType = PointerType::get(subType, pType->getAddressSpace());
        } break;
        default:
        {
            assert (false && "bad type for element extraction found!");
            throw std::logic_error("INTERNAL ERROR: bad type for element extraction found!");
        } break;
    }

    cacheTypePair(scalarType, type);
    return scalarType;
}

bool
rv::mayHaveSideEffects(const Instruction&     inst,
                        const FunctionInfoMap* functionInfoMap)
{
    if (isa<LoadInst>(inst)) return true;
    if (isa<StoreInst>(inst)) return true;
    if (!isa<CallInst>(inst)) return false;

    const CallInst * call = cast<const CallInst>(&inst);
    const Function* callee = call->getCalledFunction();
    if (!callee) return true;
    switch (callee->getIntrinsicID()) {
    case Intrinsic::lifetime_end:
    case Intrinsic::lifetime_start:
    	return false;
    }

    if (!functionInfoMap) return true;

    if (!functionInfoMap->hasMapping(*callee)) return true;

    // TODO: This could be too aggressive. Is it always safe to assume that the
    //       side effects only occur for the active instances?
    const int maskIndex = functionInfoMap->getMaskIndex(*callee);
    if (maskIndex != -1) return false;

    return functionInfoMap->mayHaveSideEffects(*callee);
}

// Create a new constant from the value of 'c' that
// requires at most 32 bits. If the value is too large
// to be stored in 32 bit or if the type of the constant
// is a non-numerical type, the function throws an exception.
// NOTE: Not used anywhere anymore.
Constant*
rv::getMax32BitConstant(Constant* c, LLVMContext& context)
{
    assert (c);
    Type* type = c->getType();

    if (type->getPrimitiveSizeInBits() <= 32U)
    {
        return c;
    }

    if (type->isIntegerTy())
    {
        if (isa<UndefValue>(c))
        {
            return UndefValue::get(Type::getInt32Ty(context));
        }

        ConstantInt* opC = cast<ConstantInt>(c);
        const uint64_t intValue = *opC->getValue().getRawData();

        if (!ConstantInt::isValueValidForType(Type::getInt32Ty(context), intValue))
        {
            errs() << "WARNING: Integer constant is too large "
                << "to fit into 32bit - cannot vectorize: "
                << intValue << "\n";
            errs() << "         This may result in reduced performance.\n";
            return nullptr;
        }

        return ConstantInt::get(context, APInt(32, intValue));
    }

    if (type->isFloatingPointTy())
    {
        if (isa<UndefValue>(c))
        {
            return UndefValue::get(Type::getFloatTy(context));
        }

        ConstantFP* opC = cast<ConstantFP>(c);

        if (!ConstantFP::isValueValidForType(Type::getFloatTy(context),
                                             opC->getValueAPF()))
        {
            errs() << "WARNING: Floating point constant is too large "
                << "to fit into 32bit - cannot vectorize: "
                << opC->getValueAPF().convertToDouble() << "\n";
            errs() << "         This may result in reduced performance.\n";
            return nullptr;
        }

        return ConstantFP::get(Type::getFloatTy(context),
                               opC->getValueAPF().convertToDouble());
    }

    errs() << "ERROR: bad type found for constant creation: " << *type << "\n";
    return nullptr;
}

Instruction*
rv::createDummy(Type* type, Instruction* insertBefore)
{
    Constant* c = Constant::getNullValue(type);
    if (type->isPointerTy())
    {
        // we must not insert pointer selects ;)
        return new BitCastInst(c, type, "dummy", insertBefore);
    }
    else
    {
        return SelectInst::Create(Constant::getNullValue(Type::getInt1Ty(type->getContext())),
                                  c,
                                  c,
                                  "dummy",
                                  insertBefore);
    }
}

Instruction*
rv::createNoOp(Type* type, Instruction* insertBefore)
{
    Instruction* noOp = createDummy(type, insertBefore);
    noOp->setName("noop");
    return noOp;
}

// Returns the unique return block of function 'f'.
// We rely on the ReturnUnifier pass and thus terminate as soon as we
// have found a return.
void
rv::findReturnBlocks(Function&                    f,
                      SmallPtrSet<BasicBlock*, 2>& returnBlocks)
{
    for (auto &BB : f)
    {
        assert (BB.getTerminator() &&
                "each basic block has to have a terminator!");
        if (!isa<ReturnInst>(BB.getTerminator())) continue;

        returnBlocks.insert(&BB);
    }

    assert (!returnBlocks.empty() &&
            "Function does not contain a return statement!");
}

// Returns the unique return block of function 'f'.
// We rely on the ReturnUnifier pass and thus terminate as soon as we
// have found a return.
void
rv::findReturnBlocks(const Function&                    f,
                      SmallPtrSet<const BasicBlock*, 2>& returnBlocks)
{
    for (auto &BB : f)
    {
        assert (BB.getTerminator() &&
                "each basic block has to have a terminator!");
        if (!isa<ReturnInst>(BB.getTerminator())) continue;

        returnBlocks.insert(&BB);
    }

    assert (!returnBlocks.empty() &&
            "Function does not contain a return statement!");
}

unsigned
rv::getNumIncomingEdges(const BasicBlock& block)
{
    unsigned numIncomingEdges = 0;
    for (const_pred_iterator P=pred_begin(&block),
            PE=pred_end(&block); P!=PE; ++P)
    {
        ++numIncomingEdges;
    }
    return numIncomingEdges;
}

void
rv::getExitingBlocks(BasicBlock*                  exitBlock,
                      const LoopInfo&              loopInfo,
                      SmallVector<BasicBlock*, 2>& exitingBlocks)
{
    const Loop* outerLoop = loopInfo.getLoopFor(exitBlock);
    for (pred_iterator P=pred_begin(exitBlock), PE=pred_end(exitBlock); P!=PE; ++P)
    {
        BasicBlock* exitingBlock = *P;
        const Loop* loop = loopInfo.getLoopFor(exitingBlock);
        if (loop && loop != outerLoop && loop->isLoopExiting(exitingBlock))
        {
            exitingBlocks.push_back(exitingBlock);
        }
    }

    // Apparently, irreducible loops can turn out to have no exiting blocks due
    // to this criterion (Irreducible4).
    if (exitingBlocks.empty()) return;

    // Sanity check
    Loop* commonLoop = loopInfo.getLoopFor(*exitingBlocks.begin());
    RV_UNUSED(commonLoop);
    for (const auto &exitingBB : exitingBlocks)
    {
        RV_UNUSED(exitingBB);
        assert (loopInfo.getLoopFor(exitingBB) == commonLoop);
        assert (commonLoop->contains(exitingBB));
    }
}

BasicBlock*
rv::getExitBlock(const BasicBlock* exitingBlock, const LoopInfo& loopInfo)
{
    assert (exitingBlock);

    const Loop* loop = loopInfo.getLoopFor(exitingBlock);
    assert (loop && loop->isLoopExiting(exitingBlock));

    const TerminatorInst* terminator = exitingBlock->getTerminator();
    for (unsigned i=0, e=terminator->getNumSuccessors(); i<e; ++i)
    {
        BasicBlock* succBB = terminator->getSuccessor(i);
        const Loop* succLoop = loopInfo.getLoopFor(succBB);
        if (succLoop == loop) continue;
        if (loop->contains(succLoop)) continue;
        assert (succLoop->contains(loop));
        return succBB;
    }

    assert (false && "must never happen!");
    return nullptr;
}

Loop*
rv::getInnermostExitedLoop(const BasicBlock& exitBlock,
                            const LoopInfo&   loopInfo)
{
    const Loop* outerLoop = loopInfo.getLoopFor(&exitBlock);
    for (const_pred_iterator P=pred_begin(&exitBlock), PE=pred_end(&exitBlock); P!=PE; ++P)
    {
        const BasicBlock* exitingBlock = *P;
        Loop* loop = loopInfo.getLoopFor(exitingBlock);
        if (loop && loop != outerLoop && loop->isLoopExiting(exitingBlock))
        {
            assert (!loop->contains(&exitBlock));
            assert (!outerLoop || outerLoop->contains(loop));
            return loop;
        }
    }

    return nullptr;
}

Loop*
rv::getOutermostExitedLoop(const BasicBlock& exitBlock,
                            const LoopInfo&   loopInfo)
{
    Loop* loop = getInnermostExitedLoop(exitBlock, loopInfo);
    while (loop)
    {
        assert (!loop->contains(&exitBlock));
        Loop* parentLoop = loop->getParentLoop();
        if (!parentLoop || parentLoop->contains(&exitBlock)) break;
        loop = parentLoop;
    }
    return loop;
}

Loop*
rv::getCommonLoop(Loop* loopA, Loop* loopB)
{
    assert (loopA && loopB);

    if (loopA == loopB) return loopA;
    if (loopA->contains(loopB)) return loopA;
    if (loopB->contains(loopA)) return loopB;

    while (loopA)
    {
        assert (!loopB->contains(loopA)); // outer test should have succeeded already.
        if (loopA == loopB) return loopA;
        if (loopA->contains(loopB)) return loopA;

        loopA = loopA->getParentLoop();
    }

    return loopA;
}

const Loop*
rv::getCommonLoop(const Loop* loopA, const Loop* loopB)
{
    if (!loopA || !loopB) return nullptr;

    if (loopA == loopB) return loopA;
    if (loopA->contains(loopB)) return loopA;
    if (loopB->contains(loopA)) return loopB;

    while (loopA)
    {
        assert (!loopB->contains(loopA)); // outer test should have succeeded already.
        if (loopA == loopB) return loopA;
        if (loopA->contains(loopB)) return loopA;

        loopA = loopA->getParentLoop();
    }

    return loopA;
}

Loop*
rv::getCommonLoop(const SmallPtrSet<BasicBlock*, 2>& blocks,
                   const LoopInfo&                    loopInfo)
{
    if (blocks.empty()) return nullptr;

    Loop* icLoop = loopInfo.getLoopFor(*blocks.begin());
    if (!icLoop) return nullptr;

    for (const auto &BB : blocks)
    {
        Loop* loop = loopInfo.getLoopFor(BB);
        if (!loop) return nullptr;

        if (loop == icLoop) continue;
        if (icLoop->contains(loop)) continue;
        if (loop->contains(icLoop))
        {
            icLoop = loop;
            continue;
        }

        // Loops are not contained in each other.
        // Check if there is an outer loop that contains both.
        icLoop = rv::getCommonLoop(loop, icLoop);
        if (!icLoop) return nullptr;
    }

    return icLoop;
}

// This implementation relies on the fact that each loop exit has a dedicated
// target (unlike what loop simplify guarantees, which is a target that has no
// incoming edges from a different loop).
bool
rv::isExitOfDivergentLoop(const BasicBlock&        exitBlock,
                           const LoopInfo&          loopInfo,
                           const VectorizationInfo& vecInfo)
{
    if (const BasicBlock* predBB = exitBlock.getUniquePredecessor())
    {
        const Loop* loop = loopInfo.getLoopFor(&exitBlock);
        const Loop* predLoop = loopInfo.getLoopFor(predBB);
        if (predLoop && predLoop->isLoopExiting(predBB) && predLoop != loop &&
            vecInfo.isDivergentLoop(predLoop))
        {
            return true;
        }
    }

    return false;
}

bool
rv::isExitOfDivergentLoop(const BasicBlock&        exitingBlock,
                           const BasicBlock&        exitBlock,
                           const LoopInfo&          loopInfo,
                           const VectorizationInfo& vecInfo)
{
    const Loop* loop = loopInfo.getLoopFor(&exitBlock);
    const Loop* predLoop = loopInfo.getLoopFor(&exitingBlock);
    return predLoop && predLoop->isLoopExiting(&exitingBlock) && predLoop != loop &&
           vecInfo.isDivergentLoop(predLoop);
}

// If 'doNotTraverse' is set, the corresponding block is entered, but no
// successors are visited.
// If 'doNotLeaveLoop' is set, no exit edge of the loop is taken.
// If 'doNotUseBackEdge' is set, the back edge of the loop is not taken.
// This allows to find reachable blocks within a loop where inner or outer loop
// back edges are still allowed.
// If 'doNotUseAnyBackEdge' is set, no back edge is taken.
bool
rv::isReachable(const BasicBlock* target,
                 const BasicBlock* source,
                 const BasicBlock* doNotTraverse,
                 const Loop*       doNotLeaveLoop,
                 const Loop*       doNotUseBackEdge,
                 const bool        doNotUseAnyBackEdge,
                 const LoopInfo*   loopInfo)
{
    assert (target && source);
    assert ((!doNotUseAnyBackEdge || loopInfo) &&
            "must supply loop info if disallowing any back edge");

    SmallPtrSet<const BasicBlock*, 16> visitedBlocks;
    SmallVector<const BasicBlock*, 16> workList;
    workList.push_back(source);

    const BasicBlock* latchBB = doNotUseBackEdge ? doNotUseBackEdge->getLoopLatch() : nullptr;
    const BasicBlock* headerBB = doNotUseBackEdge ? doNotUseBackEdge->getHeader() : nullptr;

    while (!workList.empty())
    {
        const BasicBlock* block = workList.pop_back_val();
        if (block == target) return true;

        if (visitedBlocks.count(block)) continue;
        visitedBlocks.insert(block);

        if (doNotTraverse == block) continue;

        const TerminatorInst* terminator = block->getTerminator();
        for (unsigned i=0, e=terminator->getNumSuccessors(); i<e; ++i)
        {
            const BasicBlock* succBB = terminator->getSuccessor(i);

            // If loop exits are disallowed, and the successor is not part of the loop, ignore it,
            if (doNotLeaveLoop && !doNotLeaveLoop->contains(succBB)) continue;

            // If all loop back edges are disallowed, and this is one, ignore it.
            Loop* succLoop = loopInfo ? loopInfo->getLoopFor(succBB) : nullptr;
            if (doNotUseAnyBackEdge && succLoop &&
                succLoop->getHeader() == succBB &&
                succLoop->getLoopLatch() == block)
            {
                continue;
            }

            // If a specific backedge is disallowed, and this is it, ignore it.
            if (headerBB == succBB && latchBB == block)
            {
                continue;
            }

            workList.push_back(succBB);
        }
    }

    return false;
}

#ifdef RV_ENABLE_LEGACY_API
// This function looks for all paths including paths that take loop back
// edges (but at most once!). If 'ignoreOuterLoops' is set, the back edge of
// this loop and all outer loops is not allowed to be taken.
//
// This is horribly complex. In order to make the situation a little better,
// we keep a set of blocks of which we know that the target can not be
// reached. We could do the same for blocks where we know the target can be
// reached, but this would require to also store the corresponding paths.
bool
rv::collectPaths(const BasicBlock*                   target,
                  const BasicBlock*                   source,
                  PathType&                           currentPath,
                  PathVecType&                        paths,
                  const bool                          ignoreOuterLoops,
                  const LoopInfo&                     loopInfo,
                  SmallPtrSet<const BasicBlock*, 16>& visitedBlocks,
                  SmallPtrSet<const BasicBlock*, 16>& unreachableBlocks)
{
    assert (target && source);

    // If this is the target block, copy and store the current path, then roll back.
    if (source == target)
    {
        currentPath.push_back(source); // Just make sure the path is complete.
        PathType* newPath = new PathType(currentPath);
        paths.push_back(newPath);
        currentPath.pop_back();
        return true;
    }

    if (visitedBlocks.count(source)) return false;
    visitedBlocks.insert(source);
    if (unreachableBlocks.count(source)) return false;

    //outs() << "    visiting block: " << source->getName() << "\n";

    currentPath.push_back(source);

    bool reachable = false;
    const TerminatorInst* terminator = source->getTerminator();
    for (unsigned i=0, e=terminator->getNumSuccessors(); i<e; ++i)
    {
        BasicBlock* succBB = terminator->getSuccessor(i);

        // If the successor is a loop header and the current block
        // is the corresponding latch, check if we are allowed to
        // take that back edge.
        if (ignoreOuterLoops && loopInfo.isLoopHeader(succBB))
        {
            Loop* loop = loopInfo.getLoopFor(succBB);
            if (loop->getLoopLatch() == source)
            {
                Loop* targetLoop = loopInfo.getLoopFor(target);
                if (targetLoop == loop) continue;
                if (!targetLoop->contains(loop)) continue;
            }
        }

        const bool found = collectPaths(target,
                                        succBB,
                                        currentPath,
                                        paths,
                                        ignoreOuterLoops,
                                        loopInfo,
                                        visitedBlocks,
                                        unreachableBlocks);
        reachable |= found;
        if (!found) unreachableBlocks.insert(succBB);
    }

    // Remove the current block from the path.
    currentPath.pop_back();
    // Remove the current block from the set of visited blocks to allow finding
    // all possible paths (even overlapping ones).
    visitedBlocks.erase(source);

    return reachable;
}

// This function only looks for paths within a loop (and its inner loops).
void
rv::collectLoopPaths(const BasicBlock*                   target,
                      const BasicBlock*                   source,
                      const Loop*                         loop,
                      PathType&                           currentPath,
                      PathVecType&                        paths,
                      SmallPtrSet<const BasicBlock*, 16>& visitedBlocks)
{
    assert (target && source);

    // If this is the target block, copy and store the current path, then roll
    // back (even if the block is outside the loop, after all we are looking
    // for loop exits 50% of the time).
    if (source == target)
    {
        currentPath.push_back(source); // Just make sure the path is complete.
        PathType* newPath = new PathType(currentPath);
        paths.push_back(newPath);
        currentPath.pop_back();
        return;
    }

    // Otherwise, if the successor is not part of the loop, stop recursion.
    if (loop && !loop->contains(source)) return;

    if (visitedBlocks.count(source)) return;
    visitedBlocks.insert(source);

    // If this is the loop latch, stop recursion (successors are either outside
    // the loop or the header - both types of edges that we do not follow).
    if (loop && loop->getLoopLatch() == source) return;

    currentPath.push_back(source);

    const TerminatorInst* terminator = source->getTerminator();
    for (unsigned i=0, e=terminator->getNumSuccessors(); i<e; ++i)
    {
        const BasicBlock* succBB = terminator->getSuccessor(i);
        collectLoopPaths(target, succBB, loop, currentPath, paths, visitedBlocks);
    }

    // Remove the current block from the path.
    currentPath.pop_back();
    // Remove the current block from the set of visited blocks to allow finding
    // all possible paths (even overlapping ones).
    visitedBlocks.erase(source);
}
#endif

Loop*
rv::findNestedLoopOfInst(Loop* parentLoop, Instruction* inst)
{
    assert (parentLoop && inst);
    BasicBlock* block = inst->getParent();

    for (auto &SL : *parentLoop)
    {
        if (SL->contains(block)) return SL;
    }

    assert (parentLoop->begin() == parentLoop->end());
    return nullptr;
}

Loop*
rv::findNextNestedLoopOfExit(Loop*       loop,
                              BasicBlock* exitingBlock)
{
    assert (loop && exitingBlock);
    assert (loop->isLoopExiting(exitingBlock));

    for (auto &SL : *loop)
    {
        if (!SL->contains(exitingBlock)) continue;
        if (SL->isLoopExiting(exitingBlock)) return SL;
    }

    return nullptr;
}

Loop*
rv::findTopLevelLoopOfExit(Loop*           loop,
                            BasicBlock*     exitingBlock,
                            BasicBlock*     exitBlock,
                            const LoopInfo& loopInfo)
{
    assert (loop && exitingBlock && exitBlock);
    assert (loop->isLoopExiting(exitingBlock));
    assert (exitBlock->getUniquePredecessor() == exitingBlock); // LoopSimplify allows this.

    Loop* parentLoop = loop->getParentLoop();
    Loop* exitLoop   = loopInfo.getLoopFor(exitBlock);

    // If there is no parent loop, the exit block can not be in a loop either.
    assert (parentLoop || !exitLoop);

    // If there is no parent loop, this is the top level loop of the exit.
    if (!parentLoop) return loop;

    // If the parent loop equals the loop of the exit block, the current loop
    // is the top level loop of this exit.
    if (parentLoop == exitLoop) return loop;

    // Otherwise, recurse into parent loop.
    return findTopLevelLoopOfExit(parentLoop, exitingBlock, exitBlock, loopInfo);
}

Instruction*
rv::generateAlignedAlloc(Type*          targetType,
                          const RVInfo& mInfo,
                          Instruction*   insertBefore)
{
    assert (targetType && insertBefore);

    // Always generate alloca's in entry block of function.
    insertBefore = insertBefore->getParent()->getParent()->getEntryBlock().getTerminator();
    assert (insertBefore);

    AllocaInst* structPtr = new AllocaInst(targetType,
                                           mInfo.mConstInt32Two,
                                           mInfo.mAlignmentSIMD,
                                           "",
                                           insertBefore);

    IntegerType* intPtrType = mInfo.mDataLayout->getIntPtrType(*mInfo.mContext);
    PtrToIntInst* ptr2IntInst = new PtrToIntInst(structPtr,
                                                 intPtrType,
                                                 "",
                                                 insertBefore);

    ConstantInt* intMinus16 = ConstantInt::get(intPtrType, -16, true);
    BinaryOperator* andInst = BinaryOperator::Create(Instruction::And,
                                                     ptr2IntInst,
                                                     intMinus16,
                                                     "",
                                                     insertBefore);

    ConstantInt* intPlus16 = ConstantInt::get(intPtrType, 16, true);
    BinaryOperator* addInst = BinaryOperator::Create(Instruction::Add,
                                                     andInst,
                                                     intPlus16,
                                                     "",
                                                     insertBefore);

    IntToPtrInst* int2PtrInst = new IntToPtrInst(addInst,
                                                 structPtr->getType(),
                                                 "",
                                                 insertBefore);

    return int2PtrInst;
}

Module*
rv::createModuleFromFile(const std::string & fileName, LLVMContext & context) {
    SMDiagnostic smDiag;
    std::unique_ptr<Module> modPtr = parseIRFile(fileName, smDiag, context);
    return modPtr.release();
}

void
rv::writeModuleToFile(const Module& mod, const std::string& fileName)
{
    std::error_code EC;
    raw_fd_ostream file(fileName, EC, sys::fs::OpenFlags::F_RW);
    mod.print(file, nullptr);
    file.close();
    if (EC)
    {
        errs() << "ERROR: printing module to file failed: " << EC.message() << "\n";
    }
}

void
rv::writeFunctionToFile(const Function& f, const std::string & fileName)
{
    std::error_code EC;
    raw_fd_ostream file(fileName, EC, sys::fs::OpenFlags::F_RW);
    f.print(file);
    file.close();
    if (EC)
    {
        errs() << "ERROR: printing function to file failed: " << EC.message() << "\n";
    }
}


// insert print statement that prints 'value' preceeded by 'DEBUG: `message`'
// example what can be generated:
// declare i32 @printf(i8* noalias nocapture, ...) nounwind
// @.str1 = private constant [19 x i8] c"DEBUG: indexA: %d\0A\00", align 1 ; <[19 x i8]*> [#uses=1]
// %printf1 = tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([19 x i8]* @.str1, i64 0, i64 0), i32 %call) ; <i32> [#uses=0]
// Usage Example:
// insertPrintf("else-block executed! - pointerIdx: ", pointerIdx, true, (*elseBB)->getTerminator());
CallInst*
rv::insertPrintf(const std::string& message,
                  Value*             value,
                  const bool         endLine,
                  Instruction*       insertBefore)
{
	assert (value && insertBefore);
	assert (insertBefore->getParent());
	Function* f = insertBefore->getParent()->getParent();
	assert (f);
	Module* mod = f->getParent();
	assert (mod);

	Function* func_printf =  mod->getFunction("printf");

	if (!func_printf)
    {
		PointerType* PointerTy_6 = PointerType::get(
                IntegerType::get(mod->getContext(), 8), 0);

		std::vector<Type*>FuncTy_10_args;
		FuncTy_10_args.push_back(PointerTy_6);
		FunctionType* FuncTy_10 = FunctionType::get(
				/*Result=*/IntegerType::get(mod->getContext(), 32),
				/*Params=*/FuncTy_10_args,
				/*isVarArg=*/true);

		func_printf = Function::Create(
				/*Type=*/FuncTy_10,
				/*Linkage=*/GlobalValue::ExternalLinkage,
				/*Name=*/"printf", mod); // (external, no body)
		func_printf->setCallingConv(CallingConv::C);
        func_printf->addFnAttr(Attribute::AttrKind::NoUnwind);
        func_printf->addFnAttr(Attribute::AttrKind::ReadNone);
        func_printf->addAttribute(1, Attribute::AttrKind::NoCapture);
	}

	const bool valueIsVector = value->getType()->isVectorTy();

	const unsigned vectorSize = valueIsVector ?
        cast<VectorType>(value->getType())->getNumElements() :
        0;

	const unsigned stringSize = message.length() +
		(valueIsVector ? vectorSize*2 : 2) +
		(valueIsVector ? vectorSize-1 : 0) +
		(endLine ? 2 : 1) +
		7;

	ArrayType* ArrayTy_0 = ArrayType::get(IntegerType::get(mod->getContext(), 8),
                                          stringSize);
	GlobalVariable* gvar_array__str =
            new GlobalVariable(/*Module=*/*mod,
                               /*Type=*/ArrayTy_0,
                               /*isConstant=*/true,
                               /*Linkage=*/GlobalValue::PrivateLinkage,
                               /*Initializer=*/0, // has initializer, specified below
                               /*Name=*/".str");
	gvar_array__str->setAlignment(1);

	// Constant Definitions
	std::string str = "";
	switch (value->getType()->getTypeID())
    {
		case Type::IntegerTyID : str = "%d"; break;
		case Type::FloatTyID   : str = "%f"; break;
		case Type::PointerTyID : str = "%x"; break;
		case Type::VectorTyID  :
        {
			std::string tmp;
			switch (value->getType()->getContainedType(0)->getTypeID())
            {
				case Type::IntegerTyID : tmp = "%d"; break;
				case Type::FloatTyID   : tmp = "%f"; break;
				default                : tmp = "%x"; break;
			}
			for (unsigned i=0; i<vectorSize; ++i)
            {
				if (i != 0) str += " ";
				str += tmp;
			}
			break;
		}
		default                : str = "%x"; break;
	}

	std::stringstream sstr;
	sstr << "DEBUG: " << message << str << (endLine ? "\x0A" : "");
	Constant* const_array_11 = ConstantDataArray::getString(mod->getContext(), sstr.str(), true);
	std::vector<Constant*> const_ptr_17_indices;
	ConstantInt* const_int64_18 = ConstantInt::get(mod->getContext(),
                                                   APInt(64, StringRef("0"), 10));
	const_ptr_17_indices.push_back(const_int64_18);
	const_ptr_17_indices.push_back(const_int64_18);
	Constant* const_ptr_17 =
            ConstantExpr::getGetElementPtr(nullptr, gvar_array__str,
                                           ArrayRef<Constant*>(const_ptr_17_indices));

	// Global Variable Definitions
	gvar_array__str->setInitializer(const_array_11);


	std::vector<Value*> int32_51_params;
	int32_51_params.push_back(const_ptr_17);

    if (valueIsVector)
    {
		for (unsigned i=0; i<vectorSize; ++i)
        {
			ExtractElementInst* ei =
                    ExtractElementInst::Create(value,
                                               ConstantInt::get(mod->getContext(), APInt(32, i)),
                                               "printfElem",
                                               insertBefore);
			int32_51_params.push_back(ei);
		}
	}
    else
    {
		int32_51_params.push_back(value);
	}

	CallInst* int32_51 = CallInst::Create(func_printf,
                                          ArrayRef<Value*>(int32_51_params),
                                          "",
                                          insertBefore);
	return int32_51;
}
