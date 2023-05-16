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
#include <llvm/IR/Metadata.h>
#include <llvm/Analysis/LoopInfo.h> // Loop

#include <llvm/Support/MemoryBuffer.h> // MemoryBuffer
#include <llvm/IRReader/IRReader.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/FileSystem.h>

#include <llvm/ADT/SmallVector.h>
#include <llvm/Support/raw_ostream.h>

#include "rv/utils.h"
#include "rvConfig.h"

using namespace llvm;


namespace rv {

static unsigned
GetVectorNumElements(Type* Ty) {
  return cast<FixedVectorType>(Ty)->getNumElements();
}

static Type*
GetVectorElementType(Type* Ty) {
  return cast<FixedVectorType>(Ty)->getElementType();
}

// This function defines what we consider matching types
// in terms of uniform/varying.
bool
typesMatch(Type* t1, Type* t2)
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
        case Type::FixedVectorTyID:
        {
            const unsigned elems1 = GetVectorNumElements(t1);
            const unsigned elems2 = GetVectorNumElements(t2);
            const unsigned bitSize1 = GetVectorElementType(t1)->getScalarSizeInBits() * elems1;
            const unsigned bitSize2 = GetVectorElementType(t2)->getScalarSizeInBits() * elems2;

            auto t1Elem = GetVectorElementType(t1);
            auto t2Elem = GetVectorElementType(t2);
            if (t1Elem->isFloatingPointTy())
            {
            	MATCH_RETURN(t2Elem->isFloatingPointTy() && bitSize1 == bitSize2)
            }
            else
            {
                assert (t1Elem->isIntegerTy());
                MATCH_RETURN(t2Elem->isIntegerTy() && bitSize1 == bitSize2)
            }

            MATCH_RETURN(false)
        }
        case Type::PointerTyID:
        {
            //Opaque pointer types always match.
            MATCH_RETURN(true);
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

Module*
createModuleFromBuffer(const char buffer[], size_t length, LLVMContext & context) {
  std::unique_ptr<MemoryBuffer> mb = MemoryBuffer::getMemBuffer(StringRef(buffer, length), "", false);
  SMDiagnostic smDiag;
  std::unique_ptr<Module> modPtr = parseIR(*mb, smDiag, context);
  if (!modPtr) smDiag.print("rv::createModuleFromBuffer", errs());
  mb.release();
  return modPtr.release();
}

Module*
createModuleFromFile(const std::string & fileName, LLVMContext & context) {
    SMDiagnostic smDiag;
    std::unique_ptr<Module> modPtr = parseIRFile(fileName, smDiag, context);
    if (!modPtr) smDiag.print("rv::createModuleFromFile", errs());
    return modPtr.release();
}

void
writeModuleToFile(const Module& mod, const std::string& fileName)
{
    std::error_code EC;
    raw_fd_ostream file(fileName, EC, sys::fs::OpenFlags::OF_None);
    mod.print(file, nullptr);
    file.close();
    if (EC)
    {
        errs() << "ERROR: printing module to file failed: " << EC.message() << "\n";
    }
}

void
writeFunctionToFile(const Function& f, const std::string & fileName)
{
    std::error_code EC;
    raw_fd_ostream file(fileName, EC, sys::fs::OpenFlags::OF_None);
    f.print(file);
    file.close();
    if (EC)
    {
        errs() << "ERROR: printing function to file failed: " << EC.message() << "\n";
    }
}

void
getExitingBlocks(BasicBlock*                  exitBlock,
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

///// defaulting phi semantics /////
namespace {
   const char * ShadowInputMDName = "rv.shadow.in";
   const char * TotalOperationMDName = "rv.total";
}

void
setTotalOperationTag(Instruction & inst) {
  auto * mdTuple = MDTuple::get(inst.getContext(), {});
  inst.setMetadata(TotalOperationMDName, mdTuple);
}

void
dropTotalOperationTag(Instruction & inst) {
  inst.setMetadata(TotalOperationMDName, nullptr);
}

bool
hasTotalOperationTag(Instruction & inst) {
  // short cut
  if (!inst.hasMetadataOtherThanDebugLoc()) return false;

  // otw, parse default shadow input argument
  auto * totalOpMD = inst.getMetadata(TotalOperationMDName);
  return (bool) totalOpMD;
}

void
setShadowInput(PHINode & phi, Value & defInput) {
  auto * valAsMd = ValueAsMetadata::get(&defInput);
  auto * mdTuple = MDTuple::get(phi.getContext(), {valAsMd});
  phi.setMetadata(ShadowInputMDName, mdTuple);
}

void dropShadowInput(PHINode & phi) {
  phi.setMetadata(ShadowInputMDName, nullptr);
}

Value* getShadowInput(const PHINode & phi) {
  // short cut
  if (!phi.hasMetadataOtherThanDebugLoc()) return nullptr;

  // otw, parse default shadow input argument
  auto * shadowInputMD = phi.getMetadata(ShadowInputMDName);
  if (!shadowInputMD) return nullptr;
  const auto * mdTuple = dyn_cast<MDTuple>(shadowInputMD);
  if (!mdTuple) return nullptr;
  assert(mdTuple->getNumOperands() == 1 && "ill-formed shadow input");
  const auto * valueAsMd = dyn_cast<ValueAsMetadata>(mdTuple->getOperand(0));
  assert(valueAsMd && "ill-formed shadow input");
  return valueAsMd->getValue();
}


} // namespace rv
