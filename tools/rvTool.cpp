/*
 * rvTool.cpp
 *
 *  Created on: Oct 31, 2016
 *      Author: Simon Moll
 */

#include "rvTool.h"

#include <iostream>
#include <cassert>
#include <sstream>

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Constants.h"

#include <llvm/IR/Module.h>
#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Analysis/TargetLibraryInfo.h>

#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Analysis/LoopInfo.h"

#include "llvm/Support/raw_ostream.h"

#include "ArgumentReader.h"

#include "rv/rv.h"
#include "rv/vectorMapping.h"
#include "rv/rvInfo.h"
#include "rv/sleefLibrary.h"
#include "rv/transform/loopExitCanonicalizer.h"
#include "rv/Region/LoopRegion.h"

using namespace llvm;

Module*
createModuleFromFile(const std::string& fileName)
{
    SMDiagnostic diag;
    auto modPtr = llvm::parseIRFile(fileName, diag, llvm::getGlobalContext());
    return modPtr.release();
}

void
writeModuleToFile(Module* mod, const std::string& fileName)
{
    assert (mod);
    std::error_code EC;
    raw_fd_ostream file(fileName, EC, sys::fs::OpenFlags::F_RW);
    mod->print(file, nullptr);
    if (EC)
    {
        errs() << "ERROR: printing module to file failed: " << EC.message() << "\n";
        return;
    }
    file.close();
}

void
normalizeFunction(Function& F)
{
    legacy::FunctionPassManager FPM(F.getParent());
    FPM.add(createLoopSimplifyPass());
    FPM.add(createLCSSAPass());
    FPM.run(F);
}

static Value*
GetInitValue(Loop& loop, PHINode& phi)
{
    for (uint i = 0; i < phi.getNumIncomingValues(); ++i)
    {
        if (!loop.contains(phi.getIncomingBlock(i)))
        {
            return phi.getIncomingValue(i);
        }
    }
    return nullptr;
}

static bool
AdjustStride(Loop& loop, PHINode& phi, uint vectorWidth)
{
    Instruction* increment = nullptr;
    for (uint i = 0; i < phi.getNumIncomingValues(); ++i)
    {
        auto* inVal = phi.getIncomingValue(i);
        auto* inInst = dyn_cast<Instruction>(inVal);
        if (!inInst || !loop.contains(inInst->getParent()))
        {
            continue;
        }
        if (!inInst || (inInst->getOpcode() != Instruction::Add))
        { continue; }
        increment = inInst;
        break;
    }

    // bump up loop increment to vector width
    assert(increment);
    uint constPos = isa<Constant>(increment->getOperand(1)) ? 1 : 0;
    auto* incStep = cast<ConstantInt>(increment->getOperand(constPos));
    assert(incStep->getLimitedValue() == 1);
    auto* vectorIncStep = ConstantInt::getSigned(incStep->getType(), vectorWidth);
    increment->setOperand(constPos, vectorIncStep);

    return true;
}

void
vectorizeLoop(Function& parentFn, Loop& loop, uint vectorWidth, LoopInfo& loopInfo, DFG& dfg,
              CDG& cdg, DominatorTree& domTree, PostDominatorTree& postDomTree)
{
    // assert: function is already normalized

    Module& mod = *parentFn.getParent();
    auto* rvInfo = new rv::RVInfo(&mod,
                                  &mod.getContext(),
                                  &parentFn,
                                  &parentFn,
                                  vectorWidth,
                                  -1, // mask position
                                  false /* disableMemAccessAnalysis */,
                                  false /* disableControlFlowDivAnalysis */,
                                  false /* disableAllAnalyses */,
                                  false,
                                  nullptr);

    // set-up for loop vectorization
    rv::VectorMapping targetMapping(&parentFn, &parentFn, vectorWidth);

    rv::LoopRegion loopRegionImpl(loop);
    rv::Region loopRegion(loopRegionImpl);
    VectorizationInfo vecInfo(parentFn, vectorWidth, loopRegion);

    TargetIRAnalysis irAnalysis;
    TargetTransformInfo tti = irAnalysis.run(parentFn);
    TargetLibraryAnalysis libAnalysis;
    TargetLibraryInfo tli = libAnalysis.run(*parentFn.getParent());
    rv::PlatformInfo platformInfo(&tti, &tli);

    // link in SIMD library
    const bool useSSE = false;
    const bool useAVX = true;
    const bool useAVX2 = false;
    addSleefMappings(useSSE, useAVX, useAVX2, platformInfo);

    // configure initial shape for induction variable
    auto* header = loop.getHeader();
    PHINode* xPhi = cast<PHINode>(&*header->begin());
    auto* xPhiInit = GetInitValue(loop, *xPhi);
    errs() << "Vectorizing loop with induction variable " << *xPhi << "\n";
    vecInfo.setVectorShape(*xPhi, rv::VectorShape::strided(1, vectorWidth));
    vecInfo.setVectorShape(*xPhiInit, rv::VectorShape::strided(1, vectorWidth));

    // configure exit condition to be non-divergent in any case
    auto* exitBlock = loop.getExitingBlock();
    assert(exitBlock && "does not have a unique exit block!");
    vecInfo.setVectorShape(*exitBlock->getTerminator(), rv::VectorShape::uni());
    vecInfo.setVectorShape(*cast<BranchInst>(exitBlock->getTerminator())->getOperand(0),
                           rv::VectorShape::uni());

    bool matched = AdjustStride(loop, *xPhi, vectorWidth);
    assert(matched && "could not match ++i loop pattern");

    rv::VectorizerInterface vectorizer(*rvInfo, &parentFn);

    // vectorizationAnalysis
    vectorizer.analyze(vecInfo, cdg, dfg, loopInfo, postDomTree, domTree);

    // mask analysis
    MaskAnalysis* maskAnalysis = vectorizer.analyzeMasks(vecInfo, loopInfo);
    assert(maskAnalysis);
    maskAnalysis->print(errs(), &mod);

    // mask generator
    bool genMaskOk = vectorizer.generateMasks(vecInfo, *maskAnalysis, loopInfo);
    assert(genMaskOk);

    // control conversion
    bool linearizeOk = vectorizer.linearizeCFG(vecInfo, *maskAnalysis, loopInfo, postDomTree,
                                               domTree);
    assert(linearizeOk);

    const DominatorTree domTreeNew(*vecInfo.getMapping()
                                           .scalarFn); // Control conversion does not preserve the domTree so we have to rebuild it for now
    bool vectorizeOk = vectorizer.vectorize(platformInfo, vecInfo, domTreeNew);
    assert(vectorizeOk);

    // cleanup
    vectorizer.finalize();

    delete maskAnalysis;
    delete rvInfo;
}

// Use case: Outer-loop Vectorizer
void
vectorizeFirstLoop(Function& parentFn, uint vectorWidth)
{
    // normalize
    normalizeFunction(parentFn);

    // build Analysis
    DominatorTree domTree(parentFn);
    PostDominatorTree postDomTree;
    postDomTree.runOnFunction(parentFn);
    LoopInfo loopInfo(domTree);

    // Dominance Frontier Graph
    DFG dfg(domTree);
    dfg.create(parentFn);

    // Control Dependence Graph
    CDG cdg(*postDomTree.DT);
    cdg.create(parentFn);

    // normalize loop exits
    LoopExitCanonicalizer canonicalizer(loopInfo);
    canonicalizer.canonicalize(parentFn);

    if (loopInfo.begin() == loopInfo.end())
    {
        return;
    }

    auto* firstLoop = *loopInfo.begin();

    vectorizeLoop(parentFn, *firstLoop, vectorWidth, loopInfo, dfg, cdg, domTree, postDomTree);

    // mark region
    // run RV
    // replace stride
}


// Use case: Whole-Function Vectorizer
void
vectorizeFunction(rv::VectorMapping& vectorizerJob)
{
    Function* scalarFn = vectorizerJob.scalarFn;
    Function* vectorFn = vectorizerJob.vectorFn;
    Module& mod = *scalarFn->getParent();

    auto* rvInfo = new rv::RVInfo(&mod,
                                  &mod.getContext(),
                                  scalarFn,
                                  vectorFn,
                                  vectorizerJob.vectorWidth,
                                  -1, // mask position
                                  false /* disableMemAccessAnalysis */,
                                  false /* disableControlFlowDivAnalysis */,
                                  false /* disableAllAnalyses */,
                                  false,
                                  nullptr);

    // clone source function for transformations
    ValueToValueMapTy valueMap;
    Function* scalarCopy = CloneFunction(scalarFn, valueMap, false);

    assert (scalarCopy);
    scalarCopy->setCallingConv(scalarFn->getCallingConv());
    scalarCopy->setAttributes(scalarFn->getAttributes());
    scalarCopy->setAlignment(scalarFn->getAlignment());
    scalarCopy->setLinkage(GlobalValue::InternalLinkage);
    scalarCopy->setName(scalarFn->getName() + ".vectorizer.tmp");
    mod.getFunctionList().push_back(scalarCopy);

    // normalize
    normalizeFunction(*scalarCopy);

    // set-up vectorizer
    rv::VectorMapping targetMapping = vectorizerJob;
    targetMapping.scalarFn = scalarCopy;
    VectorizationInfo vecInfo(targetMapping);

    rv::VectorizerInterface vectorizer(*rvInfo, scalarCopy);

    TargetIRAnalysis irAnalysis;
    TargetTransformInfo tti = irAnalysis.run(*scalarCopy);
    TargetLibraryAnalysis libAnalysis;
    TargetLibraryInfo tli = libAnalysis.run(*scalarCopy->getParent());
    rv::PlatformInfo platformInfo(&tti, &tli);

#if 0
    // link in SIMD library
    const bool useSSE = false;
    const bool useSSE41 = false;
    const bool useSSE42 = false;
    const bool useNEON = false;
    const bool useAVX = true;
    rvInfo->addCommonMappings(useSSE, useSSE41, useSSE42, useAVX, useNEON);
#else
    // link in SIMD library
    const bool useSSE = false;
    const bool useAVX = true;
    const bool useAVX2 = false;
    addSleefMappings(useSSE, useAVX, useAVX2, platformInfo);
#endif

    // build Analysis
    DominatorTree domTree(*scalarCopy);
    PostDominatorTree postDomTree;
    postDomTree.runOnFunction(*scalarCopy);
    LoopInfo loopInfo(domTree);

    // Dominance Frontier Graph
    DFG dfg(domTree);
    dfg.create(*scalarCopy);

    // Control Dependence Graph
    CDG cdg(*postDomTree.DT);
    cdg.create(*scalarCopy);

    // normalize loop exits
    LoopExitCanonicalizer canonicalizer(loopInfo);
    canonicalizer.canonicalize(*scalarCopy);

    // vectorizationAnalysis
    vectorizer.analyze(vecInfo, cdg, dfg, loopInfo, postDomTree, domTree);

    // mask analysis
    MaskAnalysis* maskAnalysis = vectorizer.analyzeMasks(vecInfo, loopInfo);
    assert(maskAnalysis);

    // mask generator
    bool genMaskOk = vectorizer.generateMasks(vecInfo, *maskAnalysis, loopInfo);
    assert(genMaskOk);

    // control conversion
    bool linearizeOk = vectorizer.linearizeCFG(vecInfo, *maskAnalysis, loopInfo, postDomTree,
                                               domTree);
    assert(linearizeOk);

    const DominatorTree domTreeNew(*vecInfo.getMapping()
                                           .scalarFn); // Control conversion does not preserve the domTree so we have to rebuild it for now
    bool vectorizeOk = vectorizer.vectorize(platformInfo, vecInfo, domTreeNew);
    assert(vectorizeOk);

    // cleanup
    vectorizer.finalize();

    delete maskAnalysis;
    scalarCopy->eraseFromParent();
}

Type*
vectorizeType(Type* scalarTy, rv::VectorShape shape, uint vectorWidth)
{
    if (scalarTy->isVoidTy()) return scalarTy;
    if (!shape.isDefined() || shape.hasStridedShape()) return scalarTy;

    return VectorType::get(scalarTy, vectorWidth);
}

Function*
createVectorDeclaration(Function& scalarFn, rv::VectorShape resShape,
                        const rv::VectorShapeVec& argShapes, uint vectorWidth)
{
    auto* scalarFnTy = scalarFn.getFunctionType();

    auto* vectorRetTy = vectorizeType(scalarFnTy->getReturnType(), resShape, vectorWidth);

    std::vector<Type*> vectorArgTys;
    for (uint i = 0; i < scalarFnTy->getNumParams(); ++i)
    {
        auto* scalarArgTy = scalarFnTy->getParamType(i);
        rv::VectorShape argShape = argShapes[i];
        vectorArgTys.push_back(vectorizeType(scalarArgTy, argShape, vectorWidth));
    }

    auto* vectorFnTy = FunctionType::get(vectorRetTy, vectorArgTys, false);

    return llvm::Function::Create(vectorFnTy, scalarFn.getLinkage(), scalarFn.getName() + "_SIMD",
                                  scalarFn.getParent());
}

unsigned
readNumber(std::string shapeText, unsigned& pos)
{
    unsigned alignment = 0U;

    auto toint = [](char c) { return static_cast<unsigned>(c - '0'); };

    for (; isdigit(shapeText[pos]); ++pos)
    {
        alignment = alignment * 10 + toint(shapeText[pos]);
    }

    return alignment;
}

unsigned
decodeAlignment(std::string shapeText, unsigned& pos)
{
    if (pos >= shapeText.size()) return 1U;

    if (shapeText[pos] != 'a') return 1U;

    ++pos;

    if (!isdigit(shapeText[pos])) return 1U; // alignment omitted, assume 1

    return readNumber(shapeText, pos);
}

rv::VectorShape
decodeShape(std::string shapeText, unsigned& pos)
{
    char c = shapeText[pos++];
    unsigned stridedOf = 0;

    // For 'S' a following stride is expected
    if (c == 'S')
    {
        assert (isdigit(shapeText[pos]) && "expected a stride after 'S'!");
        stridedOf = readNumber(shapeText, pos);
    }

    unsigned alignment = decodeAlignment(shapeText, pos);

    if (c == 'C')      return rv::VectorShape::cont(alignment);
    else if (c == 'T') return rv::VectorShape::varying(alignment);
    else if (c == 'U') return rv::VectorShape::uni(alignment);
    else if (c == 'S') return rv::VectorShape::strided(stridedOf, alignment);

    return rv::VectorShape::undef();
}

int main(int argc, char** argv)
{
    ArgumentReader reader(argc, argv);

    std::string inFile;
    bool hasFile = reader.readOption<std::string>("-i", inFile);

    std::string kernelName;
    bool hasKernelName = reader.readOption<std::string>("-k", kernelName);

    bool wfvMode = reader.hasOption("-wfv");
    bool loopVecMode = reader.hasOption("-loopvec");

    std::string targetDeclName;
    bool hasTargetDeclName = reader.readOption<std::string>("-t", targetDeclName);

    bool lowerPredicateIntrinsics = reader.hasOption("-lower");

    std::string outFile;
    bool hasOutFile = reader.readOption<std::string>("-o", outFile);

    if (!(hasFile && hasKernelName))
    {
        std::cerr << "Not all arguments specified -wfv/-loopvec) "
                  << "-i MODULE -k KERNELNAME [-target TARGET_DECL]"
                  << "[-o OUTPUT_LL] [-w 8] [--lower]\n";
        return -1;
    }

    // Load module
    llvm::Module* mod = createModuleFromFile(inFile);
    if (!mod)
    {
        errs() << "Could not load module " << inFile << ". Aborting!\n";
        return 1;
    }
    llvm::Function* scalarFn = mod->getFunction(kernelName);
    if (!scalarFn)
    {
        return 2;
    }

    // initialize argument mapping
    // first arg cons, all others uniform mapping
    // TODO apply user mappings

    rv::VectorShape resShape;
    rv::VectorShapeVec argShapes;
    std::string shapeText;
    if (reader.readOption<std::string>("-s", shapeText))
    {
        uint i = 0;
        for (auto & it : scalarFn->getArgumentList()) {
            (void) it;

            if (i >= shapeText.size())
            {
                argShapes.push_back(rv::VectorShape::uni());
                continue;
            }
            rv::VectorShape argShape = decodeShape(shapeText, i);
            argShapes.push_back(argShape);
        }
        if (shapeText.size() > i)
        { // return shape
            assert(shapeText[i] == 'r' && "expected return shape");
            ++i;
            resShape = decodeShape(shapeText, i);
        }

    }
    else
    {
      for (auto& it : scalarFn->getArgumentList()) {
        (void) it;
        argShapes.push_back(rv::VectorShape::uni());
      }
    }

    uint vectorWidth = reader.getOption<uint>("-w", 8);

    if (wfvMode)
    {

        // Create simd decl
        Function* vectorFn = nullptr;
        if (!hasTargetDeclName)
        {
            vectorFn = createVectorDeclaration(*scalarFn, resShape, argShapes, vectorWidth);
        }
        else
        {
            vectorFn = mod->getFunction(targetDeclName);
            // TODO verify shapes
            if (!vectorFn)
            {
                llvm::errs() << "Target declaration " << targetDeclName
                             << " not found. Aborting!\n";
                return 3;
            }
        }
        assert(vectorFn);
        mod->dump();

        rv::VectorMapping vectorizerJob(scalarFn, vectorFn, vectorWidth, -1, resShape, argShapes);

        // Vectorize
        errs() << "\nVectorizing kernel \"" << vectorizerJob.scalarFn->getName()
               << "\" into declaration \"" << vectorizerJob.vectorFn->getName()
               << "\" with vector size " << vectorizerJob.vectorWidth << "... \n";
        vectorizeFunction(vectorizerJob);

    }
    else if (loopVecMode)
    {
        vectorizeFirstLoop(*scalarFn, vectorWidth);
    }

    if (lowerPredicateIntrinsics) {
      errs() << "Lowering predicate intrinsics in function " << scalarFn->getName() << "\n";
      rv::lowerPredicateIntrinsics(*scalarFn);
    }

    //output
    if (hasOutFile)
    {
        writeModuleToFile(mod, outFile);
        errs() << "Final module written to \"" << outFile << "\"\n";
    }
    else
    {
        mod->dump();
    }

    return 0;
}
