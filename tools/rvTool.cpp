/*
 * rvTool.cpp
 *
 *  Created on: Oct 31, 2016
 *      Author: Simon Moll
 */

#include "rvTool.h"

#include <cassert>
#include <iostream>
#include <sstream>

#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

#include <llvm/IR/Module.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/Support/SourceMgr.h>

#include <llvm/Analysis/MemoryDependenceAnalysis.h>
#include <llvm/Analysis/ScalarEvolution.h>
#include <llvm/Analysis/TargetLibraryInfo.h>

#include "llvm/Analysis/LoopInfo.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Utils.h"
#include "llvm/Transforms/Utils/Cloning.h"

#include "llvm/Support/Compiler.h"
#include "llvm/Support/raw_ostream.h"

#include "ArgumentReader.h"

#include "rv/passes.h"
#include "rv/resolver/resolvers.h"
#include "rv/rv.h"
#include "rv/rvDebug.h"
#include "rv/utils.h"
#include "rv/vectorMapping.h"
#include "rv/passes/PassManagerSession.h"

#include "rv/region/FunctionRegion.h"
#include "rv/region/LoopRegion.h"
#include "rv/region/Region.h"
#include "rv/vectorizationInfo.h"

#include "rv/analysis/UndeadMaskAnalysis.h"
#include "rv/analysis/reductionAnalysis.h"
#include "rv/passes/loopExitCanonicalizer.h"
#include "rv/transform/remTransform.h"
#include "rv/transform/singleReturnTrans.h"

using namespace llvm;

#define ValidAnalysisString "da|udm"
static std::string PrintAnalysis = "";

static bool AnalyzeOnly() { return !PrintAnalysis.empty(); }

static bool PrintOnlyDA() { return PrintAnalysis == "da"; }
static bool PrintOnlyUDM() { return PrintAnalysis == "udm"; }

static bool HasValidAnalysisSetting() {
  return !AnalyzeOnly() || PrintOnlyDA() | PrintOnlyUDM();
}

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

[[noreturn]] static void fail();

static void fail() {
  std::cerr << '\n';
  assert(false);
  exit(-1);
}

template <class... Args> static void fail(std::string arg, Args... rest) {
  std::cerr << arg;
  fail(rest...);
}

Module *createModuleFromFile(const std::string &fileName,
                             LLVMContext &context) {
  SMDiagnostic diag;
  auto modPtr = llvm::parseIRFile(fileName, diag, context);
  return modPtr.release();
}

void writeModuleToFile(Module *mod, const std::string &fileName) {
  assert(mod);
  std::error_code EC;
  raw_fd_ostream file(fileName, EC, sys::fs::OpenFlags::OF_None);
  mod->print(file, nullptr);
  if (EC) {
    fail("ERROR: printing module to file failed: ", EC.message());
  }
  file.close();
}

void normalizeFunction(Function &F) {
  legacy::FunctionPassManager FPM(F.getParent());
  FPM.add(createFixIrreduciblePass());
  FPM.add(createPromoteMemoryToRegisterPass());
  FPM.add(createLoopSimplifyPass());
  FPM.add(createLCSSAPass());
  FPM.run(F);

  rv::FunctionRegion funcRegion(F);
  rv::Region regWrapper(funcRegion);
  rv::SingleReturnTrans::run(regWrapper);
}

void vectorizeLoop(Function &parentFn, Loop &TheLoop, unsigned vectorWidth,
                   int ulpErrorBound, FunctionAnalysisManager &FAM) {
  // assert: function is already normalized
  Module &mod = *parentFn.getParent();

  // query LLVM passes
  TargetIRAnalysis irAnalysis;
  TargetTransformInfo tti = irAnalysis.run(parentFn, FAM);
  TargetLibraryAnalysis libAnalysis;
  TargetLibraryInfo tli = libAnalysis.run(parentFn, FAM);

  // configure RV
  auto config = rv::Config::createForFunction(parentFn);
  config.maxULPErrorBound = ulpErrorBound;
  IF_VERBOSE { config.print(outs()); }

  // set-up for loop vectorization
  rv::ReductionAnalysis reductionAnalysis(parentFn, FAM);
  reductionAnalysis.analyze(TheLoop);

  ValueSet uniOverrides;
  rv::RemainderTransform remTrans(parentFn, FAM, reductionAnalysis);
  auto LoopPrep = remTrans.createVectorizableLoop(
      TheLoop, uniOverrides, false, vectorWidth, 1);
  auto *preparedLoop = LoopPrep.TheLoop;

  if (!preparedLoop) {
    fail("remTrans could not transform to a vectorizable loop.");
  }

  // setup region
  rv::LoopRegion loopRegionImpl(*preparedLoop);
  rv::Region loopRegion(loopRegionImpl);
  rv::VectorizationInfo vecInfo(parentFn, vectorWidth, loopRegion);
  assert(!LoopPrep.EntryAVL && "AVL support broken!");
  // vecInfo.setEntryAVL(LoopPrep.EntryAVL);

  rv::PlatformInfo platInfo(mod, &tti, &tli);

  MemoryDependenceAnalysis mdAnalysis;
  MemoryDependenceResults MDR = mdAnalysis.run(parentFn, FAM);

  // link in SIMD library
  addSleefResolver(config, platInfo);
  // vectorize recursively
  addRecursiveResolver(config, platInfo);

  // Check reduction patterns of vector loop phis
  // configure initial shape for induction variable
  for (auto &inst : *preparedLoop->getHeader()) {
    auto *phi = dyn_cast<PHINode>(&inst);
    if (!phi)
      continue;

    IF_VERBOSE { errs() << "loopVecPass: header phi  " << *phi << " : "; }

    rv::StridePattern *pat = reductionAnalysis.getStrideInfo(*phi);
    rv::VectorShape phiShape;
    if (pat) {
      IF_VERBOSE { pat->dump(); }
      phiShape = pat->getShape(vectorWidth);

    } else {
      rv::Reduction *redInfo = reductionAnalysis.getReductionInfo(*phi);

      if (!redInfo) {
        errs() << "\n\tskip: unrecognized phi use in vector loop "
               << preparedLoop->getName() << "\n";
        fail();
      } else {
        IF_VERBOSE { redInfo->dump(); }
        phiShape = redInfo->getShape(vectorWidth);
      }
    }

    IF_VERBOSE {
      errs() << "header phi " << phi->getName() << " has shape "
             << phiShape.str() << "\n";
    }

    if (phiShape.isDefined()) {
      vecInfo.setPinnedShape(*phi, phiShape);
    }
  }

  // set uniform overrides
  IF_VERBOSE { errs() << "-- Setting remTrans uni overrides --\n"; }
  for (auto *val : uniOverrides) {
    IF_VERBOSE { errs() << "- " << *val << "\n"; }
    vecInfo.setPinnedShape(*val, rv::VectorShape::uni());
  }

  rv::VectorizerInterface vectorizer(platInfo, config);

  // early math func lowering
  vectorizer.lowerRuntimeCalls(vecInfo, FAM);

  IF_VERBOSE {
    auto &LI = *FAM.getCachedResult<LoopAnalysis>(vecInfo.getScalarFunction());
    auto &DT =
        FAM.getResult<DominatorTreeAnalysis>(vecInfo.getScalarFunction());
    LI.print(errs());
    LI.verify(DT);
  }

  // vectorizationAnalysis
  vectorizer.analyze(vecInfo, FAM);

  if (PrintOnlyDA()) {
    vecInfo.print(outs());
    return;
  }

  if (PrintOnlyUDM()) {
    // Expand all block masks and show which block masks are considered 'undead'
    // (never all false).
    rv::UndeadMaskAnalysis UDM(vecInfo, FAM);
    rv::MaskExpander maskEx(vecInfo, FAM);
    maskEx.expandRegionMasks();
    UDM.print(outs());
    return;
  }

  // control conversion
  vectorizer.linearize(vecInfo, FAM);
  // if (!maskEx) fail("mask generation failed.");

  DominatorTree domTreeNew(
      *vecInfo.getMapping()
           .scalarFn); // Control conversion does not preserve the domTree so we
                       // have to rebuild it for now
  bool vectorizeOk = vectorizer.vectorize(vecInfo, FAM, nullptr);
  if (!vectorizeOk)
    fail("vector code generation failed");

  // cleanup
  vectorizer.finalize();
}

// Use case: Outer-loop Vectorizer
void vectorizeFirstLoop(Function &parentFn, unsigned vectorWidth,
                        int ulpErrorBound) {
  // normalize
  normalizeFunction(parentFn);

  // normalize loop exits
  {
    DominatorTree domTree(parentFn);
    LoopInfo loopInfo(domTree);
    LoopExitCanonicalizer canonicalizer(loopInfo);
    canonicalizer.canonicalize(parentFn);
  }

  // setup LLVM analysis infrastructure
  rv::PassManagerSession PMS;

  // run other analysis up front that may invalidate LoopInfo
  // ScalarEvolutionAnalysis seAnalysis;
  // ScalarEvolution SE = seAnalysis.run(parentFn, FAM);

  // compute actual analysis structures
  auto &LI = PMS.FAM.getResult<LoopAnalysis>(parentFn);

  if (LI.begin() == LI.end()) {
    return;
  }

  // dump normalized function
  IF_VERBOSE {
    errs() << "-- normalized functions --\n";
    parentFn.print(errs());
    errs() << "-- normalized loop info --\n";
    LI.print(errs());
  }

  // the loop to vectorize
  auto *firstLoop = *LI.begin();

  // HAVE TO maintain FAM.getCachedResult<LoopAnalysis> from this point on! (or
  // the loop region gets invalidated)

  vectorizeLoop(parentFn, *firstLoop, vectorWidth, ulpErrorBound, PMS.FAM);

  // mark region
  // run RV
  // replace stride
}
using ShapeMap = std::map<std::string, rv::VectorShape>;

// Use case: Whole-Function Vectorizer
void vectorizeFunction(rv::VectorMapping &vectorizerJob, ShapeMap extraShapes,
                       bool generateVectorName, int ulpErrorBound) {
  Function *scalarFn = vectorizerJob.scalarFn;
  Module &mod = *scalarFn->getParent();

  // setup LLVM analysis infrastructure
  rv::PassManagerSession PMS;

  // platform API
  TargetIRAnalysis irAnalysis;
  TargetTransformInfo tti = irAnalysis.run(*scalarFn, PMS.FAM);
  TargetLibraryAnalysis libAnalysis;
  TargetLibraryInfo tli = libAnalysis.run(*scalarFn, PMS.FAM);
  rv::PlatformInfo platInfo(mod, &tti, &tli);

  // assign a proper vector function name
  if (generateVectorName) {
    auto scaName = scalarFn->getName();
    auto mangledVectorName = platInfo.createMangledVectorName(
        scaName, vectorizerJob.argShapes, vectorizerJob.vectorWidth,
        vectorizerJob.maskPos);
    vectorizerJob.vectorFn->setName(mangledVectorName);
  }

  // clone source function for transformations
  ValueToValueMapTy valueMap;
  Function *scalarCopy = CloneFunction(scalarFn, valueMap, nullptr);
  // normalize
  normalizeFunction(*scalarCopy);
  {
    DominatorTree domTree(*scalarCopy);
    LoopInfo loopInfo(domTree);
    LoopExitCanonicalizer canonicalizer(loopInfo);
    canonicalizer.canonicalize(*scalarCopy);
  }

  // emit a rv_entry_mask call to get a hold off the "future" function entry
  // mask (which will ultimately be available as a function argument in the
  // vectorized function we are about to generate).
  if (vectorizerJob.maskPos >= 0) {
    MaterializeEntryMask(*scalarCopy, platInfo);
  }

  assert(scalarCopy);
  scalarCopy->setCallingConv(scalarFn->getCallingConv());
  scalarCopy->setAttributes(scalarFn->getAttributes());
  scalarCopy->setAlignment(MaybeAlign(scalarFn->getAlignment()));
  scalarCopy->setLinkage(GlobalValue::InternalLinkage);
  scalarCopy->setName(scalarFn->getName() + ".vectorizer.tmp");

  // request LI
  PMS.FAM.getResult<LoopAnalysis>(*scalarCopy);

  // configure RV
  auto config = rv::Config::createForFunction(*scalarFn);
  config.maxULPErrorBound = ulpErrorBound;
  IF_VERBOSE { config.print(outs()); }

  // link in SIMD library
  addSleefResolver(config, platInfo);
  // vectorize recursively
  addRecursiveResolver(config, platInfo);

  rv::VectorizerInterface vectorizer(platInfo, config);

  // set-up vecInfo overlay and define vectorization job (mapping)
  rv::VectorMapping targetMapping = vectorizerJob;
  targetMapping.scalarFn = scalarCopy;
  rv::FunctionRegion funcRegion(*scalarCopy);
  rv::Region funcRegionWrapper(funcRegion);
  rv::VectorizationInfo vecInfo(funcRegionWrapper, targetMapping);

  // transfer extra shapes
  for (auto &it : extraShapes) {
    auto name = it.first;
    auto shape = it.second;

    auto *gv = mod.getGlobalVariable(name);
    auto *vecFun = mod.getFunction(name);

    if (vecFun) {
      // interpret <shape> as result shape
      rv::VectorMapping vecFuncMap(vecFun, vecFun, 0,
                                   rv::CallPredicateMode::SafeWithoutPredicate);
      vecFuncMap.resultShape = shape;
      platInfo.addMapping(vecFuncMap);
      outs() << "rvTool func mapping: ";
      vecFuncMap.print(outs());

    } else if (gv) {
      // interpret <shape> as shape of gvar address
      vecInfo.setPinnedShape(*gv, shape);
    }
  }

  // dump normalized function
  IF_VERBOSE {
    errs() << "-- normalized functions --\n";
    scalarCopy->print(errs());
  }

  // early math func lowering
  vectorizer.lowerRuntimeCalls(vecInfo, PMS.FAM);

  // vectorizationAnalysis
  vectorizer.analyze(vecInfo, PMS.FAM);
  if (PrintOnlyDA()) {
    vecInfo.print(outs());
    return;
  }

  if (PrintOnlyUDM()) {
    // Expand all block masks and show which block masks are considered 'undead'
    // (never all false).
    rv::UndeadMaskAnalysis UDM(vecInfo, PMS.FAM);
    rv::MaskExpander maskEx(vecInfo, PMS.FAM);
    maskEx.expandRegionMasks();
    UDM.print(outs());
    return;
  }

  // mask generator
  vectorizer.linearize(vecInfo, PMS.FAM);
  // if (!maskEx) fail("mask generation failed.");

  // Control conversion does not preserve the domTree so we have to rebuild it
  // for now
  DominatorTree domTreeNew(*vecInfo.getMapping().scalarFn);
  bool vectorizeOk = vectorizer.vectorize(vecInfo, PMS.FAM, nullptr);
  if (!vectorizeOk)
    fail("vector code generation failed.");

  // cleanup
  vectorizer.finalize();

  scalarCopy->eraseFromParent();
}

unsigned readNumber(std::stringstream &shapeText) {
  unsigned number;
  shapeText >> number;
  assert(!shapeText.fail() && "expected a number!");
  return number;
}

unsigned decodeAlignment(std::stringstream &shapeText) {
  if (shapeText.get() != 'a')
    return shapeText.unget(), 1U; // expect 'a' or rollback, return 1

  return readNumber(shapeText);
}

template <typename Elem_Reader_t>
void readList(int sep, std::stringstream &listText, Elem_Reader_t reader) {
  bool next;
  do {
    reader(listText); // read one element
    int c = listText.peek();
    next = c == sep; // check if the list ends here
    if (next)
      listText.ignore(1); // skip seperator
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

rv::VectorShape decodeShape(std::stringstream &shapestream) {
  int c = shapestream.get();

  if (c == BOTCHAR)
    return rv::VectorShape::undef();

  unsigned alignment = decodeAlignment(shapestream);

  if (c == CONTCHAR)
    return rv::VectorShape::cont(alignment);
  else if (c == VARCHAR)
    return rv::VectorShape::varying(alignment);
  else if (c == UNICHAR)
    return rv::VectorShape::uni(alignment);

  unsigned stridedOf = readNumber(shapestream);

  if (c == STRIDEDCHAR)
    return rv::VectorShape::strided(stridedOf, alignment);

  fail("Expected stride specifier.");
  abort();
}

static void PrintHelp() {
  std::cerr
      << "RV command line tool (rvTool)\n"
      << "No command specified!\n"
      << "-i MODULE [-k KERNELNAME] [-t TARGET_DECL] [-normalize] ..."
         "[-lower] [-v] "
      << "[-o OUTPUT_LL] [-w 8] [-s SHAPES] [-x GV_SHAPES]\n"
      << "\nCommands:\n"
      << "-wfv/-loopvec      : vectorize a whole-function or an outer loop\n"
      << "-analyze=" ValidAnalysisString
         "    : print analysis results (may imply "
         "transformations up to that point in the pipeline.\n"
      << "-lower-func        : lower predicate intrinsics in scalar kernel.\n"
      << "-lower             : lower predicate intrinsics in entire module.\n"
      << "-normalize         : normalize kernel and quit.\n"
      << "\nOptions:\n"
      << "-i MODULE          : LLVM input module.\n"
      << "-o MODULE          : LLVM output module.\n"
      << "-k KERNEL          : name of the function to vectorize/function "
         "with loop to vectorize.\n"
      << "-t DECL            : target SIMD declaration (WFV mode only, will "
         "be auto generated if missing).\n"
      << "-s SHAPES          : WFV argument shapes.\n"
      << "-m MaskPos         : (wfv only) mask argument position.\n"
      << "--math-prec <ulp>  : ULP error bound on math functions (n/10).\n"
      << "-x GVSHAPES        : comma-separated list of global value and "
         "function-return shapes, e.g. \"gvar=C,func=S4\".\n"
      << "-w WIDTH           : vectorization factor.\n"
      << "-v                 : enable verbose output (rvTool level output).\n";
}

int main(int argc, char **argv) {
  ArgumentReader reader(argc, argv);

  // verbose debug output (rvTool level)
  verbose = reader.hasOption("-v");

  std::string inFile;
  bool hasFile = reader.readOption<std::string>("-i", inFile);

  std::string kernelName;
  bool hasKernelName = reader.readOption<std::string>("-k", kernelName);

  PrintAnalysis = reader.getOption<std::string>("-analyze", "");
  if (!HasValidAnalysisSetting()) {
    errs() << "Invalid setting for '-analyze', expected " ValidAnalysisString
              ".\n";
    return 1;
  }

  bool wfvMode = reader.hasOption("-wfv");
  bool loopVecMode = reader.hasOption("-loopvec");

  std::string targetDeclName;
  bool hasTargetDeclName = reader.readOption<std::string>("-t", targetDeclName);

  bool lowerIntrinsicsFunc = reader.hasOption("-lower-func");
  bool lowerIntrinsicsMod = reader.hasOption("-lower");
  int maskPos = -1;
  reader.readOption<int>("-m", maskPos);

  std::string outFile;
  bool hasOutFile = reader.readOption<std::string>("-o", outFile);

  bool runNormalize = reader.hasOption("-normalize");

  int ulpErrorBound = 10;
  reader.readOption<int>("--math-prec", ulpErrorBound);
  IF_VERBOSE {
    errs() << "SLEEF ulpErrorBound: " << (ulpErrorBound / 10.0) << "\n";
  }

  if (!hasFile) {
    PrintHelp();
    return -1;
  }

  LLVMContext context;

  // Load module
  llvm::Module *mod = createModuleFromFile(inFile, context);
  if (!mod) {
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
    for (auto &func : *mod) {
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
    readList(',', shapeStream, [&shapeMap](std::stringstream &in) {
      std::string gvName, shapeText;
      if (!std::getline(in, gvName, '=')) {
        fail("could not parse global variable shape!");
      }
      rv::VectorShape gvShape = decodeShape(in);
      shapeMap[gvName] = gvShape;
      std::cerr << "USER SHAPE " << gvName << " set to " << gvShape.str()
                << "\n";
    });
  }

  // TODO factor out
  if (!finish) {
    // WFV / loopVec mode
    if (!hasKernelName) {
      std::cerr << "kernel name argument missing!\n";
      return -1;
    }

    llvm::Function *scalarFn = mod->getFunction(kernelName);
    if (!scalarFn) {
      return 2;
    }
    // initialize argument mapping
    // first arg cons, all others uniform mapping
    // TODO apply user mappings

    rv::VectorShape resShape;
    rv::VectorShapeVec argShapes;
    std::string shapeText;
    if (reader.readOption<std::string>("-s", shapeText)) {
      std::stringstream shapestream(shapeText);
      // allow functions without arguments
      if (shapestream.peek() != 'r') {
        readList<>(LISTSEPERATOR, shapestream,
                   [&argShapes](decltype(shapestream) &in) {
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

      if (shapestream.peek() != EOF) { // return shape
        if (shapestream.get() != RETURNSHAPESEPERATOR)
          fail("expected return shape");
        resShape = decodeShape(shapestream);
      }

    } else {
      for (auto &it : scalarFn->args()) {
        (void)it;
        argShapes.push_back(rv::VectorShape::uni());
      }
    }

    unsigned vectorWidth = reader.getOption<unsigned>("-w", 8);

    if (wfvMode) {
      // request SIMD decl (with a requested name, if any)
      Function *vectorFn =
          hasTargetDeclName ? mod->getFunction(targetDeclName) : nullptr;
      if (!vectorFn) {
        vectorFn = rv::createVectorDeclaration(*scalarFn, resShape, argShapes,
                                               vectorWidth, maskPos);
        vectorFn->copyAttributesFrom(scalarFn);

        if (hasTargetDeclName)
          vectorFn->setName(targetDeclName);
      }
      assert(vectorFn);

      auto predMode = maskPos >= 0
                          ? rv::CallPredicateMode::PredicateArg
                          : rv::CallPredicateMode::SafeWithoutPredicate;
      rv::VectorMapping vectorizerJob(scalarFn, vectorFn, vectorWidth, maskPos,
                                      resShape, argShapes, predMode);

      // Vectorize
      IF_VERBOSE errs() << "\nVectorizing kernel \""
                        << vectorizerJob.scalarFn->getName()
                        << "\" into declaration \""
                        << vectorizerJob.vectorFn->getName()
                        << "\" with vector size " << vectorizerJob.vectorWidth
                        << "... \n";

      // vectorize and assign a mangled name (if no specific name was requested
      // beforehand)
      vectorizeFunction(vectorizerJob, shapeMap, !hasTargetDeclName,
                        ulpErrorBound);

    } else if (loopVecMode) {
      vectorizeFirstLoop(*scalarFn, vectorWidth, ulpErrorBound);
    }

    if (lowerIntrinsicsFunc) {
      IF_VERBOSE errs() << "Lowering intrinsics in function "
                        << scalarFn->getName() << "\n";
      rv::lowerIntrinsics(*scalarFn);
    } else if (lowerIntrinsicsMod) {
      IF_VERBOSE errs() << "Lowering intrinsics in module\n";
      rv::lowerIntrinsics(*mod);
    }

  } // !finish

  if (AnalyzeOnly())
    return 0;

  // output
  if (hasOutFile) {
    writeModuleToFile(mod, outFile);
    IF_VERBOSE errs() << "Final module written to \"" << outFile << "\"\n";
  } else {
    mod->print(llvm::outs(), nullptr, false, true);
  }

  return 0;
}
