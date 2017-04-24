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
#include <llvm/IR/CallSite.h>

#include <llvm/Support/MemoryBuffer.h> // MemoryBuffer
#include <llvm/IRReader/IRReader.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/FileSystem.h>

#include <llvm/ADT/SmallVector.h>
#include <llvm/Support/raw_ostream.h>

#include "rvConfig.h"

using namespace llvm;


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

Module*
rv::createModuleFromBuffer(const char buffer[], size_t length, LLVMContext & context) {
  std::unique_ptr<MemoryBuffer> mb = MemoryBuffer::getMemBuffer(StringRef(buffer, length), "", false);
  SMDiagnostic smDiag;
  std::unique_ptr<Module> modPtr = parseIR(*mb, smDiag, context);
  if (!modPtr) smDiag.print("rv::createModuleFromBuffer", errs());
  mb.release();
  return modPtr.release();
}

Module*
rv::createModuleFromFile(const std::string & fileName, LLVMContext & context) {
    SMDiagnostic smDiag;
    std::unique_ptr<Module> modPtr = parseIRFile(fileName, smDiag, context);
    if (!modPtr) smDiag.print("rv::createModuleFromFile", errs());
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

bool
rv::returnsVoidPtr(const Instruction& inst)
{
    if (!isa<CastInst>(inst)) return false;
    if (!inst.getType()->isPointerTy()) return false;

    return inst.getType()->getPointerElementType()->isIntegerTy(8);
}

// migrated over from llvm/Analysis/ValueTracker.cpp (release_38 version)
unsigned
rv::getBaseAlignment(const Value & V, const DataLayout &DL) {
  unsigned Align = 0;
  if (auto *GO = dyn_cast<GlobalObject>(&V)) {
    Align = GO->getAlignment();
    if (Align == 0) {
      if (auto *GVar = dyn_cast<GlobalVariable>(GO)) {
        Type *ObjectType = GVar->getType()->getElementType();
        if (ObjectType->isSized()) {
          // If the object is defined in the current Module, we'll be giving
          // it the preferred alignment. Otherwise, we have to assume that it
          // may only have the minimum ABI alignment.
          if (GVar->isStrongDefinitionForLinker())
            Align = DL.getPreferredAlignment(GVar);
          else
            Align = DL.getABITypeAlignment(ObjectType);
        }
      }
    }
  } else if (const Argument *A = dyn_cast<Argument>(&V)) {
    Align = A->getType()->isPointerTy() ? A->getParamAlignment() : 0;

    if (!Align && A->hasStructRetAttr()) {
      // An sret parameter has at least the ABI alignment of the return type.
      Type *EltTy = cast<PointerType>(A->getType())->getElementType();
      if (EltTy->isSized())
        Align = DL.getABITypeAlignment(EltTy);
    }
  } else if (const AllocaInst *AI = dyn_cast<AllocaInst>(&V))
    Align = AI->getAlignment();
  else if (auto CS = ImmutableCallSite(&V))
    Align = CS.getAttributes().getParamAlignment(AttributeSet::ReturnIndex);
  else if (const LoadInst *LI = dyn_cast<LoadInst>(&V))
    if (MDNode *MD = LI->getMetadata(LLVMContext::MD_align)) {
      ConstantInt *CI = mdconst::extract<ConstantInt>(MD->getOperand(0));
      Align = CI->getLimitedValue();
    }

  return Align;
}
