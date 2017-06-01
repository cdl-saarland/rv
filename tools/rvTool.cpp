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
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Support/FileSystem.h>

#include <llvm/Analysis/ScalarEvolution.h>
#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/Analysis/MemoryDependenceAnalysis.h>

#include <rv/analysis/reductionAnalysis.h>

#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Passes/PassBuilder.h"

#include "llvm/Support/raw_ostream.h"

#include "ArgumentReader.h"

#include "rv/rv.h"
#include "rv/vectorMapping.h"
#include "rv/sleefLibrary.h"
#include "rv/analysis/maskAnalysis.h"
#include "rv/transform/loopExitCanonicalizer.h"
#include "rv/region/LoopRegion.h"
#include "rv/region/Region.h"

#include "rv/vectorizationInfo.h"

static const char LISTSEPERATOR = '_';
static const char RETURNSHAPESEPERATOR = 'r';

static const char BOTCHAR = 'B';
static const char UNICHAR = 'U';
static const char CONTCHAR = 'C';
static const char STRIDEDCHAR = 'S';
static const char VARCHAR = 'T';

static void
fail(const char * errMsg = nullptr) __attribute__((noreturn));

static void
fail(const char * errMsg) {
  if (errMsg) std::cerr << errMsg << "\nAbort!\n";
  assert(false); // preserve the stack frame in dbg builds
  exit(-1);
}

using namespace llvm;

Module*
createModuleFromFile(const std::string& fileName, LLVMContext & context)
{
    SMDiagnostic diag;
    auto modPtr = llvm::parseIRFile(fileName, diag, context);
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
        fail();
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

static int
AdjustStride(Instruction& increment, uint vectorWidth)
{
    // bump up loop increment to vector width
    uint constPos = isa<Constant>(increment.getOperand(1)) ? 1 : 0;
    auto* incStep = cast<ConstantInt>(increment.getOperand(constPos));
    int oldinc = incStep->getLimitedValue();
    auto* vectorIncStep = ConstantInt::getSigned(incStep->getType(), oldinc * vectorWidth);
    increment.setOperand(constPos, vectorIncStep);

    return oldinc;
}

void
vectorizeLoop(Function& parentFn, Loop& loop, uint vectorWidth, LoopInfo& loopInfo, DFG& dfg,
              CDG& cdg, DominatorTree& domTree, PostDominatorTree& postDomTree)
{
    // assert: function is already normalized
    Module& mod = *parentFn.getParent();


    // set up analysis infrastructure
    FunctionAnalysisManager fam;
    ModuleAnalysisManager mam;

    PassBuilder PB;
    PB.registerFunctionAnalyses(fam);
    PB.registerModuleAnalyses(mam);

    // query LLVM passes
    TargetIRAnalysis irAnalysis;
    TargetTransformInfo tti = irAnalysis.run(parentFn, fam);
    TargetLibraryAnalysis libAnalysis;
    TargetLibraryInfo tli = libAnalysis.run(*parentFn.getParent(), mam);

    ScalarEvolutionAnalysis seAnalysis;
    ScalarEvolution SE = seAnalysis.run(parentFn, fam);


    // set-up for loop vectorization
    rv::VectorMapping targetMapping(&parentFn, &parentFn, vectorWidth);

    rv::LoopRegion loopRegionImpl(loop);
    rv::Region loopRegion(loopRegionImpl);
    rv::VectorizationInfo vecInfo(parentFn, vectorWidth, loopRegion);

    rv::PlatformInfo platformInfo(mod, &tti, &tli);

    MemoryDependenceAnalysis mdAnalysis;
    MemoryDependenceResults MDR = mdAnalysis.run(parentFn, fam);

    // link in SIMD library
    const bool useSSE = false;
    const bool useAVX = true;
    const bool useAVX2 = false;
    const bool useImpreciseFunctions = false;
    addSleefMappings(useSSE, useAVX, useAVX2, platformInfo, useImpreciseFunctions);

    rv::ReductionAnalysis reductionAnalysis(parentFn, loopInfo);
    reductionAnalysis.analyze();

    // configure initial shape for induction variable(s)
    auto* header = loop.getHeader();
    for (auto it = header->begin(); &*it != header->getFirstNonPHI(); ++it) {
        PHINode& phi = cast<PHINode>(*it);
        rv::Reduction* reduction = reductionAnalysis.getReductionInfo(phi);

        Instruction& reductinst = reduction->getReductor();

        if (reductinst.getOpcode() == BinaryOperator::Add &&
            (isa<ConstantInt>(reductinst.getOperand(0)) ||
             isa<ConstantInt>(reductinst.getOperand(1))))
        {
            int oldinc = AdjustStride(reductinst, vectorWidth);
            errs() << "Vectorizing loop with induction variable " << phi << "\n";
            vecInfo.setVectorShape(phi, rv::VectorShape::strided(oldinc, vectorWidth));
        } else {
            vecInfo.setVectorShape(phi, rv::VectorShape::varying(vectorWidth));
        }
    }

    // configure exit condition to be non-divergent in any case
    auto* exitBlock = loop.getExitingBlock();
    if (!exitBlock) fail("loop does not have a unique exit block!");
    vecInfo.setVectorShape(*exitBlock->getTerminator(), rv::VectorShape::uni());
    vecInfo.setVectorShape(*cast<BranchInst>(exitBlock->getTerminator())->getOperand(0),
                           rv::VectorShape::uni());

    rv::VectorizerInterface vectorizer(platformInfo);

    // vectorizationAnalysis
    vectorizer.analyze(vecInfo, cdg, dfg, loopInfo, postDomTree, domTree);

    // control conversion
    vectorizer.linearize(vecInfo, cdg, dfg, loopInfo, postDomTree, domTree);
    // if (!maskEx) fail("mask generation failed.");
#if 0

    // control conversion
    bool linearizeOk = vectorizer.linearizeCFG(vecInfo, *maskEx, loopInfo, domTree);
    if (!linearizeOk) fail("linearization failed.");
#endif

    const DominatorTree domTreeNew(*vecInfo.getMapping()
                                           .scalarFn); // Control conversion does not preserve the domTree so we have to rebuild it for now
    bool vectorizeOk = vectorizer.vectorize(vecInfo, domTreeNew, loopInfo, SE, MDR, nullptr);
    if (!vectorizeOk) fail("vector code generation failed");

    // cleanup
    vectorizer.finalize();
}

// Use case: Outer-loop Vectorizer
void
vectorizeFirstLoop(Function& parentFn, uint vectorWidth)
{
    // normalize
    normalizeFunction(parentFn);

    // build Analysis
    DominatorTree domTree(parentFn);

    // normalize loop exits
    {
      LoopInfo loopInfo(domTree);
      LoopExitCanonicalizer canonicalizer(loopInfo);
      canonicalizer.canonicalize(parentFn);
      domTree.recalculate(parentFn);
    }

    // compute actual analysis structures
    LoopInfo loopInfo(domTree);

    if (loopInfo.begin() == loopInfo.end())
    {
        return;
    }

    // Dominance Frontier Graph
    DFG dfg(domTree);
    dfg.create(parentFn);

    // post dom
    PostDominatorTree postDomTree;

    // Control Dependence Graph
    postDomTree.recalculate(parentFn);
    CDG cdg(postDomTree);
    cdg.create(parentFn);


    // dump normalized function
    {
      errs() << "-- normalized functions --\n";
      parentFn.print(errs());
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
    Module& mod = *scalarFn->getParent();

    // clone source function for transformations
    ValueToValueMapTy valueMap;
    Function* scalarCopy = CloneFunction(scalarFn, valueMap, nullptr);

    assert (scalarCopy);
    scalarCopy->setCallingConv(scalarFn->getCallingConv());
    scalarCopy->setAttributes(scalarFn->getAttributes());
    scalarCopy->setAlignment(scalarFn->getAlignment());
    scalarCopy->setLinkage(GlobalValue::InternalLinkage);
    scalarCopy->setName(scalarFn->getName() + ".vectorizer.tmp");

    // normalize
    normalizeFunction(*scalarCopy);
    FunctionAnalysisManager fam;
    ModuleAnalysisManager mam;

    // setup LLVM analysis infrastructure
    PassBuilder PB;
    PB.registerFunctionAnalyses(fam);
    PB.registerModuleAnalyses(mam);

    // platform API
    TargetIRAnalysis irAnalysis;
    TargetTransformInfo tti = irAnalysis.run(*scalarCopy, fam);
    TargetLibraryAnalysis libAnalysis;
    TargetLibraryInfo tli = libAnalysis.run(*scalarCopy->getParent(), mam);
    rv::PlatformInfo platformInfo(mod, &tti, &tli);

    rv::VectorizerInterface vectorizer(platformInfo);

    // link in SIMD library
    const bool useSSE = false;
    const bool useAVX = true;
    const bool useAVX2 = false;
    const bool useImpreciseFunctions = false;
    addSleefMappings(useSSE, useAVX, useAVX2, platformInfo, useImpreciseFunctions);

    // set-up vecInfo overlay and define vectorization job (mapping)
    rv::VectorMapping targetMapping = vectorizerJob;
    targetMapping.scalarFn = scalarCopy;
    rv::VectorizationInfo vecInfo(targetMapping);

    // build Analysis
    DominatorTree domTree(*scalarCopy);
    // normalize loop exits
    {
      LoopInfo loopInfo(domTree);
      LoopExitCanonicalizer canonicalizer(loopInfo);
      canonicalizer.canonicalize(*scalarCopy);
      domTree.recalculate(*scalarCopy);
    }

    LoopInfo loopInfo(domTree);

    ScalarEvolutionAnalysis seAnalysis;
    ScalarEvolution SE = seAnalysis.run(*scalarCopy, fam);

    MemoryDependenceAnalysis mdAnalysis;
    MemoryDependenceResults MDR = mdAnalysis.run(*scalarCopy, fam);

    // Dominance Frontier Graph
    DFG dfg(domTree);
    dfg.create(*scalarCopy);

    // post dom
    PostDominatorTree postDomTree;
    postDomTree.recalculate(*scalarCopy);

    // Control Dependence Graph
    CDG cdg(postDomTree);
    cdg.create(*scalarCopy);


    // dump normalized function
    {
      errs() << "-- normalized functions --\n";
      scalarCopy->print(errs());
    }

    // vectorizationAnalysis
    vectorizer.analyze(vecInfo, cdg, dfg, loopInfo, postDomTree, domTree);

    // mask generator
    vectorizer.linearize(vecInfo, cdg, dfg, loopInfo, postDomTree, domTree);
    // if (!maskEx) fail("mask generation failed.");

#if 0
    // control conversion
    bool linearizeOk = vectorizer.linearizeCFG(vecInfo, *maskEx, loopInfo, domTree);
    if (!linearizeOk) fail("linearization failed.");
#endif

    // Control conversion does not preserve the domTree so we have to rebuild it for now
    const DominatorTree domTreeNew(*vecInfo.getMapping().scalarFn);
    bool vectorizeOk = vectorizer.vectorize(vecInfo, domTreeNew, loopInfo, SE, MDR, nullptr);
    if (!vectorizeOk) fail("vector code generation failed.");

    // cleanup
    vectorizer.finalize();

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

unsigned readNumber(std::stringstream& shapeText)
{
    unsigned number;
    shapeText >> number;
    assert (!shapeText.fail() && "expected a number!");
    return number;
}

unsigned decodeAlignment(std::stringstream& shapeText)
{
    if (shapeText.get() != 'a')
        return shapeText.unget(), 1U; // expect 'a' or rollback, return 1

    return readNumber(shapeText);
}

template <char SEPERATOR, typename Elem_t, typename Elem_Reader_t, typename ... ReaderArgTypes>
void readList(std::stringstream& listText,
              std::vector<Elem_t>& vec,
              Elem_Reader_t reader,
              ReaderArgTypes... args)
{
    bool next;
    do {
        vec.push_back(reader(listText, args...));  // read one element
        int c = listText.peek();
        next = c == SEPERATOR;            // check if the list ends here
        if (next) listText.ignore(1);     // skip seperator
    } while (next);
}

rv::VectorShape decodeShape(std::stringstream& shapestream)
{
    int c = shapestream.get();

    if (c == BOTCHAR) return rv::VectorShape::undef();

    unsigned alignment = decodeAlignment(shapestream);

    if (c == CONTCHAR)      return rv::VectorShape::cont   (alignment);
    else if (c == VARCHAR) return rv::VectorShape::varying(alignment);
    else if (c == UNICHAR) return rv::VectorShape::uni    (alignment);

    unsigned stridedOf = readNumber(shapestream);

    if (c == STRIDEDCHAR) return rv::VectorShape::strided(stridedOf, alignment);

    fail("Expected stride specifier.");
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

    bool lowerIntrinsics = reader.hasOption("-lower");

    std::string outFile;
    bool hasOutFile = reader.readOption<std::string>("-o", outFile);

    if (!(hasFile && hasKernelName))
    {
        std::cerr << "Not all arguments specified -wfv/-loopvec) "
                  << "-i MODULE -k KERNELNAME [-target TARGET_DECL]"
                  << "[-o OUTPUT_LL] [-w 8] [--lower]\n";
        return -1;
    }

    LLVMContext context;

    // Load module
    llvm::Module* mod = createModuleFromFile(inFile, context);
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
        std::stringstream shapestream(shapeText);
        readList<LISTSEPERATOR>(shapestream, argShapes, decodeShape);

        if (argShapes.size() != scalarFn->getArgumentList().size())
            fail("Number of specified shapes unequal to argument number.");

        if (shapestream.peek() != EOF)
        { // return shape
            if (shapestream.get() != RETURNSHAPESEPERATOR) fail("expected return shape");
            resShape = decodeShape(shapestream);
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

    if (lowerIntrinsics) {
      errs() << "Lowering intrinsics in function " << scalarFn->getName() << "\n";
      rv::lowerIntrinsics(*scalarFn);
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
