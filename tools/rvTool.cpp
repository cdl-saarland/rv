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

#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/IR/Verifier.h"

#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/Compiler.h"

#include "ArgumentReader.h"

#include "rv/rv.h"
#include "rv/vectorMapping.h"
#include "rv/sleefLibrary.h"
#include "rv/passes.h"
#include "rv/rvDebug.h"
#include "rv/utils.h"

#include "rv/vectorizationInfo.h"
#include "rv/region/LoopRegion.h"
#include "rv/region/FunctionRegion.h"
#include "rv/region/Region.h"

#include "rv/analysis/reductionAnalysis.h"
#include "rv/transform/singleReturnTrans.h"
#include "rv/transform/remTransform.h"
#include "rv/transform/loopExitCanonicalizer.h"


static bool OnlyAnalyze = false;

static const char LISTSEPERATOR = '_';
static const char RETURNSHAPESEPERATOR = 'r';

static const char BOTCHAR = 'B';
static const char UNICHAR = 'U';
static const char CONTCHAR = 'C';
static const char STRIDEDCHAR = 'S';
static const char VARCHAR = 'T';

// be verbose (rvTool level only)
static bool verbose = false;
#define IF_VERBOSE if (verbose)


static void LLVM_ATTRIBUTE_NORETURN fail();

static void fail() {
  std::cerr << '\n';
  assert(false);
  exit(-1);
}

template<class ...Args>
static void
fail(std::string arg, Args... rest) {
  std::cerr << arg;
  fail(rest...);
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
        fail("ERROR: printing module to file failed: ", EC.message());
    }
    file.close();
}

void
normalizeFunction(Function& F)
{
    legacy::FunctionPassManager FPM(F.getParent());
    FPM.add(rv::createCNSPass());
    FPM.add(createPromoteMemoryToRegisterPass());
    FPM.add(createLoopSimplifyPass());
    FPM.add(createLCSSAPass());
    FPM.run(F);

    rv::FunctionRegion funcRegion(F);
    rv::Region regWrapper(funcRegion);
    rv::SingleReturnTrans::run(regWrapper);
}

void
vectorizeLoop(Function& parentFn, Loop& loop, unsigned vectorWidth, LoopInfo& loopInfo,
              DominatorTree& domTree, PostDominatorTree& postDomTree)
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

    rv::ReductionAnalysis reductionAnalysis(parentFn, loopInfo);
    reductionAnalysis.analyze(loop);

    ValueSet uniOverrides;
    rv::RemainderTransform remTrans(parentFn, domTree, postDomTree, loopInfo, reductionAnalysis);
    auto * preparedLoop = remTrans.createVectorizableLoop(loop, uniOverrides, vectorWidth, vectorWidth);

    if (!preparedLoop) {
      fail("remTrans could not transform to a vectorizable loop.");
    }

    // configure RV
    rv::Config config;
    config.useSLEEF = true;
    config.print(outs());

    // setup region
    rv::LoopRegion loopRegionImpl(*preparedLoop);
    rv::Region loopRegion(loopRegionImpl);
    rv::VectorizationInfo vecInfo(parentFn, vectorWidth, loopRegion);

    rv::PlatformInfo platformInfo(mod, &tti, &tli);

    MemoryDependenceAnalysis mdAnalysis;
    MemoryDependenceResults MDR = mdAnalysis.run(parentFn, fam);

    // link in SIMD library
    addSleefResolver(config, platformInfo, 35);

// Check reduction patterns of vector loop phis
  // configure initial shape for induction variable
  for (auto & inst : *preparedLoop->getHeader()) {
    auto * phi = dyn_cast<PHINode>(&inst);
    if (!phi) continue;

    IF_VERBOSE { errs() << "loopVecPass: header phi  " << *phi << " : "; }

    rv::StridePattern * pat = reductionAnalysis.getStrideInfo(*phi);
    rv::VectorShape phiShape;
    if (pat) {
      IF_VERBOSE { pat->dump(); }
      phiShape = pat->getShape(vectorWidth);

    } else {
      rv::Reduction * redInfo = reductionAnalysis.getReductionInfo(*phi);

      if (!redInfo) {
        errs() << "\n\tskip: unrecognized phi use in vector loop " << preparedLoop->getName() << "\n";
        fail();
      } else {
        IF_VERBOSE { redInfo->dump(); }
        phiShape = redInfo->getShape(vectorWidth);
      }
    }

    IF_VERBOSE { errs() << "header phi " << phi->getName() << " has shape " << phiShape.str() << "\n"; }

    if (phiShape.isDefined()) { vecInfo.setPinnedShape(*phi, phiShape); }
  }

  // set uniform overrides
  IF_VERBOSE { errs() << "-- Setting remTrans uni overrides --\n"; }
  for (auto * val : uniOverrides) {
    IF_VERBOSE { errs() << "- " << *val << "\n"; }
    vecInfo.setPinnedShape(*val, rv::VectorShape::uni());
  }


    rv::VectorizerInterface vectorizer(platformInfo, config);

    // early math func lowering
    vectorizer.lowerRuntimeCalls(vecInfo, loopInfo);
    domTree.recalculate(parentFn);
    postDomTree.recalculate(parentFn);

    loopInfo.print(errs());
    loopInfo.verify(domTree);


    // vectorizationAnalysis
    vectorizer.analyze(vecInfo, domTree, postDomTree, loopInfo);

    if (OnlyAnalyze) {
      vecInfo.print(outs());
      return;
    }

    // control conversion
    vectorizer.linearize(vecInfo, domTree, postDomTree, loopInfo);
    // if (!maskEx) fail("mask generation failed.");

    DominatorTree domTreeNew(*vecInfo.getMapping()
                                           .scalarFn); // Control conversion does not preserve the domTree so we have to rebuild it for now
    bool vectorizeOk = vectorizer.vectorize(vecInfo, domTreeNew, loopInfo, SE, MDR, nullptr);
    if (!vectorizeOk) fail("vector code generation failed");

    // cleanup
    vectorizer.finalize();
}

// Use case: Outer-loop Vectorizer
void
vectorizeFirstLoop(Function& parentFn, unsigned vectorWidth)
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

    // post dom
    PostDominatorTree postDomTree;
    postDomTree.recalculate(parentFn);

    // dump normalized function
    IF_VERBOSE {
      errs() << "-- normalized functions --\n";
      parentFn.print(errs());
    }

    auto* firstLoop = *loopInfo.begin();
    vectorizeLoop(parentFn, *firstLoop, vectorWidth, loopInfo, domTree, postDomTree);

    // mark region
    // run RV
    // replace stride
}
using ShapeMap = std::map<std::string, rv::VectorShape>;

// Use case: Whole-Function Vectorizer
void
vectorizeFunction(rv::VectorMapping& vectorizerJob, ShapeMap extraShapes)
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

    // configure RV
    rv::Config config;
    config.useSLEEF = true;
    config.print(outs());

    // link in SIMD library
    addSleefResolver(config, platformInfo, 35);

    rv::VectorizerInterface vectorizer(platformInfo, config);

    // set-up vecInfo overlay and define vectorization job (mapping)
    rv::VectorMapping targetMapping = vectorizerJob;
    targetMapping.scalarFn = scalarCopy;
    rv::FunctionRegion funcRegion(*scalarCopy);
    rv::Region funcRegionWrapper(funcRegion);
    rv::VectorizationInfo vecInfo(funcRegionWrapper, targetMapping);

    // transfer extra shapes
    for (auto & it : extraShapes) {
      auto name = it.first;
      auto shape = it.second;

      auto * gv = mod.getGlobalVariable(name);
      auto * vecFun = mod.getFunction(name);

      if (vecFun) {
        // interpret <shape> as result shape
        rv::VectorMapping vecFuncMap(vecFun, vecFun, 0);
        vecFuncMap.resultShape = shape;
        platformInfo.addMapping(vecFuncMap);
        outs() << "rvTool func mapping: "; vecFuncMap.dump(outs());

      } else if (gv) {
        // interpret <shape> as shape of gvar address
        vecInfo.setPinnedShape(*gv, shape);
      }
    }

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

    // post dom
    PostDominatorTree postDomTree;
    postDomTree.recalculate(*scalarCopy);

    // Control Dependence Graph

    // dump normalized function
    IF_VERBOSE {
      errs() << "-- normalized functions --\n";
      scalarCopy->print(errs());
    }

    // early math func lowering
    vectorizer.lowerRuntimeCalls(vecInfo, loopInfo);
    domTree.recalculate(*scalarCopy);
    postDomTree.recalculate(*scalarCopy);

    loopInfo.print(errs());
    loopInfo.verify(domTree);

    // vectorizationAnalysis
    vectorizer.analyze(vecInfo, domTree, postDomTree, loopInfo);
    if (OnlyAnalyze) {
      vecInfo.print(outs());
      return;
    }

    // mask generator
    vectorizer.linearize(vecInfo, domTree, postDomTree, loopInfo);
    // if (!maskEx) fail("mask generation failed.");

    // Control conversion does not preserve the domTree so we have to rebuild it for now
    DominatorTree domTreeNew(*vecInfo.getMapping().scalarFn);
    bool vectorizeOk = vectorizer.vectorize(vecInfo, domTreeNew, loopInfo, SE, MDR, nullptr);
    if (!vectorizeOk) fail("vector code generation failed.");

    // cleanup
    vectorizer.finalize();

    scalarCopy->eraseFromParent();
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

template <typename Elem_Reader_t>
void readList(int sep,
              std::stringstream& listText,
              Elem_Reader_t reader)
{
    bool next;
    do {
        reader(listText);  // read one element
        int c = listText.peek();
        next = c == sep;            // check if the list ends here
        if (next) listText.ignore(1);     // skip seperator
    } while (next);
}
#if 0
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
#endif

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

static void
PrintHelp() {
  std::cerr << "RV command line tool (rvTool)\n"
            << "No operation specified (-wfv,-loopvec,-normalize,-lower)\n"
            << "-i MODULE [-k KERNELNAME] [-t TARGET_DECL] [-normalize] [-lower] [-v] "
            << "[-o OUTPUT_LL] [-w 8] [-s SHAPES] [-x GV_SHAPES]\n"
            << "commands:\n"
            << "-wfv/-loopvec : vectorize a whole-function or an outer loop\n"
            << "-analyze      : normalize, print vectorization analysis results and exit.\n"
            << "-lower        : lower predicate intrinsics in scalar kernel.\n"
            << "-normalize    : normalize kernel and quit.\n"
            << "options:\n"
            << "-i MODULE     : LLVM input module.\n"
            << "-o MODULE     : LLVM output module.\n"
            << "-k KERNEL     : name of the function to vectorize/function with loop to vectorize.\n"
            << "-t DECL       : target SIMD declaration (WFV mode only, will be auto generated if missing).\n"
            << "-s SHAPES     : WFV argument shapes.\n"
            << "-x GVSHAPES   : comma-separated list of global value and function-return shapes, e.g. \"gvar=C,func=S4\".\n"
            << "-w WIDTH      : vectorization factor.\n"
            << "-v            : enable verbose output (rvTool level output).\n";
}



int main(int argc, char** argv)
{
    ArgumentReader reader(argc, argv);

    std::string inFile;
    bool hasFile = reader.readOption<std::string>("-i", inFile);

    std::string kernelName;
    bool hasKernelName = reader.readOption<std::string>("-k", kernelName);

    OnlyAnalyze = reader.hasOption("-analyze"); // global
    bool wfvMode = reader.hasOption("-wfv");
    bool loopVecMode = reader.hasOption("-loopvec");

    std::string targetDeclName;
    bool hasTargetDeclName = reader.readOption<std::string>("-t", targetDeclName);

    bool lowerIntrinsics = reader.hasOption("-lower");

    std::string outFile;
    bool hasOutFile = reader.readOption<std::string>("-o", outFile);

    bool runNormalize = reader.hasOption("-normalize");

    // verbose debug output (rvTool level)
    verbose = reader.hasOption("-v");

    if (!hasFile) {
      PrintHelp();
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

    bool broken = verifyModule(*mod, &errs());
    if (broken) {
        errs() << "Broken module!\n";
        return 1;
    }

    bool finish = false;

    // run normalization and quit
    if (runNormalize) {
      for (auto & func : *mod) {
        normalizeFunction(func);
        bool broken = verifyFunction(func, &errs());
        if (broken) {
          errs() << func.getName() << "\n";
          fail("Function broken");
          return -1;
        }
      }

      finish = true;
    }

    // parse additional global variable shapes "-x gvName=shape,gvName2=shape2"
    ShapeMap shapeMap;
    std::string extraShapeText;
    if (reader.readOption<std::string>("-x", extraShapeText)) {
      std::stringstream shapeStream(extraShapeText);
      readList(',', shapeStream, [&shapeMap](std::stringstream & in){
          std::string gvName, shapeText;
          if (!std::getline(in, gvName, '=')) { fail("could not parse global variable shape!"); }
          rv::VectorShape gvShape = decodeShape(in);
          shapeMap[gvName] = gvShape;
          std::cerr << "USER SHAPE " << gvName << " set to " << gvShape.str() << "\n";
      });
    }


    // TODO factor out
    if (!finish) {
    // WFV / loopVec mode
      if (!hasKernelName) {
          std::cerr << "kernel name argument missing!\n";
          return -1;
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
          // allow functions without arguments
          if (shapestream.peek() != 'r') {
            readList<>(LISTSEPERATOR, shapestream, [&argShapes](decltype(shapestream)& in) {
                 argShapes.push_back(decodeShape(in));
            });
          }

          // fail on excessive specification
          if (argShapes.size() > scalarFn->arg_size()) {
              fail("too many arg shapes specified");
          }

          // pad with uniform shapes
          while (argShapes.size() < scalarFn->arg_size()) {
            argShapes.push_back(rv::VectorShape::uni());
          }

          if (shapestream.peek() != EOF)
          { // return shape
              if (shapestream.get() != RETURNSHAPESEPERATOR) fail("expected return shape");
              resShape = decodeShape(shapestream);
          }

      }
      else
      {
        for (auto& it : scalarFn->args()) {
          (void) it;
          argShapes.push_back(rv::VectorShape::uni());
        }
      }

      unsigned vectorWidth = reader.getOption<unsigned>("-w", 8);

      if (wfvMode)
      {

          // Create simd decl
          Function* vectorFn = nullptr;
          if (!hasTargetDeclName)
          {
              vectorFn = rv::createVectorDeclaration(*scalarFn, resShape, argShapes, vectorWidth);
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
          IF_VERBOSE errs() << "\nVectorizing kernel \"" << vectorizerJob.scalarFn->getName()
                 << "\" into declaration \"" << vectorizerJob.vectorFn->getName()
                 << "\" with vector size " << vectorizerJob.vectorWidth << "... \n";

          vectorizeFunction(vectorizerJob, shapeMap);

      }
      else if (loopVecMode)
      {
          vectorizeFirstLoop(*scalarFn, vectorWidth);
      }

      if (lowerIntrinsics) {
        IF_VERBOSE errs() << "Lowering intrinsics in function " << scalarFn->getName() << "\n";
        rv::lowerIntrinsics(*scalarFn);
      }

    } // !finish

    if (OnlyAnalyze) return 0;

    //output
    if (hasOutFile)
    {
        writeModuleToFile(mod, outFile);
        IF_VERBOSE errs() << "Final module written to \"" << outFile << "\"\n";
    }
    else
    {
      mod->print(llvm::outs(), nullptr, false, true);
    }

    return 0;
}
