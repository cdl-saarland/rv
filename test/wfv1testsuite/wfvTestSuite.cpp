//===- wfvTestSuite.cpp ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @author karrenberg
//

#include <string.h> //strcmp
#include <iostream>

#include <rv/rvInfo.h>
#include "llvmWrapper.h"

#include <llvm/IR/Verifier.h>

#include <rv/rv.h>
#include <rv/vectorizationInfo.h>
#include <rv/analysis/maskAnalysis.h>
#include <rv/transforms/loopExitCanonicalizer.h>
#include <rv/vectorMapping.h>

#define RV_RUN_ON_FUNCTION_NEW_INTERFACE_BEGIN(scalarName) \
    Function* scalarName = LLVMWrapper::getFunction(#scalarName, module); \
    Function* scalarName##_SIMD = LLVMWrapper::getFunction(#scalarName "_SIMD", module); \
    if (!scalarName) \
    { \
        std::cerr << "ERROR: Function '" #scalarName "' not found!\n"; \
        abort(); failure = true; \
    } \
    if (!scalarName##_SIMD) \
    { \
        std::cerr << "ERROR: Function '" #scalarName "_SIMD' not found!\n"; \
        abort(); failure = true; \
    } \
    if (scalarName && scalarName##_SIMD) \
    { \
        legacy::FunctionPassManager FPM(module); \
        FPM.add(createLoopSimplifyPass()); \
        FPM.add(createLCSSAPass()); \
        FPM.run(*scalarName); \
        \
        TimerGroup* timer = new TimerGroup("Whole-Function Vectorization"); \
        rv::RVInfo* rvInfo = new rv::RVInfo(module, \
                                       &module->getContext(), \
                                       scalarName, \
                                       scalarName##_SIMD, \
                                       vectorizationFactor, \
                                       -1, \
                                       disableMemAccessAnalysis, \
                                       disableControlFlowDivAnalysis, \
                                       disableAllAnalyses, \
                                       verbose, \
                                       timer); \
        \
        ValueToValueMapTy valueMap; \
        const Function* scalarFunction = rvInfo->mScalarFunction; \
        Function* scalarCopy = llvm::CloneFunction(scalarFunction, valueMap, false); \
        \
        assert (scalarCopy); \
        scalarCopy->setCallingConv(scalarFunction->getCallingConv()); \
        scalarCopy->setAttributes(scalarFunction->getAttributes()); \
        scalarCopy->setAlignment(scalarFunction->getAlignment()); \
        scalarCopy->setLinkage(GlobalValue::InternalLinkage); \
        scalarCopy->setName(scalarFunction->getName()+".wfv.tmp"); \
        rvInfo->mModule->getFunctionList().push_back(scalarCopy); \
        \
        /* Map all user-defined uniform/consecutive/aligned values */ \
        /* from the original scalar source function to the function */ \
        /* that we will be working on (tempF). */ \
        rvInfo->mValueInfoMap.mapValueInformation(valueMap); \
        \
        rv::VectorizerInterface wfv(*rvInfo, scalarCopy); \
        const bool useSSE   = !useAVX; \
        const bool useSSE41 = useSSE; \
        const bool useSSE42 = useSSE; \
        const bool useNEON  = false; \
        rvInfo->addCommonMappings(useSSE, useSSE41, useSSE42, useAVX, useNEON); \
        \
        const DominatorTree domTree(*scalarCopy); \
        PostDominatorTree postDomTree; \
        postDomTree.runOnFunction(*scalarCopy); \
        \
        DFG dfg(domTree); \
        CDG cdg(*postDomTree.DT); \
        dfg.create(*scalarCopy); \
        cdg.create(*scalarCopy); \
        \
        LoopInfo loopInfo(domTree); \
        \
        LoopExitCanonicalizer canonicalizer(loopInfo); \
        canonicalizer.canonicalize(*scalarCopy); \


#define RV_RUN_ON_FUNCTION_NEW_INTERFACE_END(scalarName) \
        rv::VectorMapping targetMapping = rvInfo->inferTargetMapping(scalarCopy); \
        VectorizationInfo vecInfo(targetMapping); \
        \
        wfv.analyze(vecInfo, cdg, dfg, loopInfo, postDomTree, domTree);\
        MaskAnalysis* maskAnalysis = wfv.analyzeMasks(vecInfo, loopInfo);\
        assert(maskAnalysis); \
        bool genMaskOk = wfv.generateMasks(vecInfo, *maskAnalysis, loopInfo); \
        assert(genMaskOk); \
        bool linearizeOk = wfv.linearizeCFG(vecInfo, *maskAnalysis, loopInfo, postDomTree, domTree); \
        assert(linearizeOk); \
	    const DominatorTree domTreeNew(*vecInfo.getMapping().scalarFn); \
        bool vectorizeOk = wfv.vectorize(vecInfo, domTreeNew); \
        assert(vectorizeOk); \
        wfv.finalize(); \
        delete maskAnalysis; \
        scalarCopy->eraseFromParent(); \
    } \
    ((void)0)

#define RV_RUN_ON_FUNCTION(scalarName) \
    if (std::string(#scalarName).substr(0, 5) != "test_") \
    { \
        std::cerr << "ERROR: Test function name has to start with 'test_'!\n"; \
        abort(); failure = true; \
    } \
    else \
    { \
        RV_RUN_ON_FUNCTION_NEW_INTERFACE_BEGIN(scalarName); \
        RV_RUN_ON_FUNCTION_NEW_INTERFACE_END(scalarName); \
    } \
    ((void)0)

#define RV_RUN_ON_FUNCTION_NEW_INTERFACE_WITH_SINGLE_CALL(scalarName, calledFnName, maskIndex) \
    Function* scalarName = LLVMWrapper::getFunction(#scalarName, module); \
    Function* scalarName##_SIMD = LLVMWrapper::getFunction(#scalarName "_SIMD", module); \
    if (!scalarName) \
    { \
        std::cerr << "ERROR: Function '" #scalarName "' not found!\n"; \
        abort(); failure = true; \
    } \
    if (!scalarName##_SIMD) \
    { \
        std::cerr << "ERROR: Function '" #scalarName "_SIMD' not found!\n"; \
        abort(); failure = true; \
    } \
    if (scalarName && scalarName##_SIMD) \
    { \
        TimerGroup* timer = new TimerGroup("Whole-Function Vectorization"); \
        rv::RVInfo* rvInfo = new rv::RVInfo(module, \
                                       &module->getContext(), \
                                       scalarName, \
                                       scalarName##_SIMD, \
                                       vectorizationFactor, \
                                       -1, \
                                       disableMemAccessAnalysis, \
                                       disableControlFlowDivAnalysis, \
                                       disableAllAnalyses, \
                                       verbose, \
                                       timer); \
        \
        ValueToValueMapTy valueMap; \
        const Function* scalarFunction = rvInfo->mScalarFunction; \
        Function* scalarCopy = llvm::CloneFunction(scalarFunction, valueMap, false); \
        \
        assert (scalarCopy); \
        scalarCopy->setCallingConv(scalarFunction->getCallingConv()); \
        scalarCopy->setAttributes(scalarFunction->getAttributes()); \
        scalarCopy->setAlignment(scalarFunction->getAlignment()); \
        scalarCopy->setLinkage(GlobalValue::InternalLinkage); \
        scalarCopy->setName(scalarFunction->getName()+".wfv.tmp"); \
        rvInfo->mModule->getFunctionList().push_back(scalarCopy); \
        \
        /* Map all user-defined uniform/consecutive/aligned values */ \
        /* from the original scalar source function to the function */ \
        /* that we will be working on (tempF). */ \
        rvInfo->mValueInfoMap.mapValueInformation(valueMap); \
        \
        rv::VectorizerInterface wfv(*rvInfo, scalarCopy); \
        \
        const bool useSSE   = !useAVX; \
        const bool useSSE41 = useSSE; \
        const bool useSSE42 = useSSE; \
        const bool useNEON  = false; \
        rvInfo->addCommonMappings(useSSE, useSSE41, useSSE42, useAVX, useNEON); \
        \
        const DominatorTree domTree(*scalarCopy); \
        PostDominatorTree postDomTree; \
        postDomTree.runOnFunction(*scalarCopy); \
        \
        DFG dfg(domTree); \
        CDG cdg(*postDomTree.DT); \
        dfg.create(*scalarCopy); \
        cdg.create(*scalarCopy); \
        \
        LoopInfo loopInfo(domTree); \
        \
        LoopExitCanonicalizer canonicalizer(loopInfo); \
        canonicalizer.canonicalize(*scalarCopy); \
        \
        rvInfo->addSIMDMapping(*calledFnName, *calledFnName##_SIMD, maskIndex, true); \
        \
        rv::VectorMapping targetMapping = rvInfo->inferTargetMapping(scalarCopy); \
        VectorizationInfo vecInfo(targetMapping); \
        \
        wfv.analyze(vecInfo, cdg, dfg, loopInfo, postDomTree, domTree); \
        MaskAnalysis* maskAnalysis = wfv.analyzeMasks(vecInfo, loopInfo); \
        assert(maskAnalysis); \
        bool genMaskOk = wfv.generateMasks(vecInfo, *maskAnalysis, loopInfo); \
        assert(genMaskOk); \
        bool linearizeOk = wfv.linearizeCFG(vecInfo, *maskAnalysis, loopInfo, postDomTree, domTree); \
        assert(linearizeOk); \
        const DominatorTree domTreeNew(*vecInfo.getMapping().scalarFn); \
        bool vectorizeOk = wfv.vectorize(vecInfo, domTreeNew); \
        assert(vectorizeOk); \
        wfv.finalize(); \
        delete maskAnalysis; \
        scalarCopy->eraseFromParent(); \
        if (verifyFunction(*calledFnName##_SIMD)) abort(); \
    } \
    ((void)0)

#define RV_SETUP_TESTSUITE(fileName) \
    llvm::Module* module = LLVMWrapper::createModuleFromFile(fileName); \
    if (!module) return -1; \
    \
    if (optimize) \
    { \
        std::cout << "\noptimizing test suite module... "; \
        LLVMWrapper::optimizeModuleSimple(module); \
        std::cout << "done.\n"; \
    } ((void)0)

#define RV_COMPILE_AND_RUN_TESTSUITE(testSuiteNr) \
    if (failure) \
    { \
        std::cerr << "\nERROR: WFV of at least one function failed!\n\n"; \
        return -1; \
    } \
    \
    if (verify) \
    { \
        std::cout << "verifying bitcode of new module... "; \
        LLVMWrapper::verifyModule(module); \
        std::cout << "done.\n"; \
    } \
    \
    if (dump) \
    { \
        std::cout << "writing module to file... "; \
        LLVMWrapper::writeModuleToFile(module, "testModule" #testSuiteNr ".pkt.ll"); \
        std::cout << "done.\n"; \
    } \
    \
    llvm::Function* mainFn = LLVMWrapper::getFunction("main", module); \
    assert (mainFn); \
    \
	if (!onlyAnalyze) { \
		/* Erase functions that are not used (= functions that were not vectorized). */ \
		/* We can replace all uses by a null value since the code will not be executed anyway. */ \
		std::vector<Function*> eraseVec;\
		std::vector<Instruction*> eraseVec2; \
		for (Function & F : *module) \
		{ \
			Function* fn = &F; \
			if (mainFn == fn) continue; \
			if (!fn->isDeclaration()) continue; \
			if (!fn->getName().startswith("test_")) continue; \
			eraseVec.push_back(fn); \
			for (llvm::Function::use_iterator U=fn->use_begin(), UE=fn->use_end(); U!=UE; ++U) \
			{ \
				if (!isa<Instruction>(*U)) continue; \
				Instruction* useI = cast<Instruction>(*U); \
				Type* type = useI->getType(); \
				if (!type->isVoidTy()) useI->replaceAllUsesWith(Constant::getNullValue(useI->getType())); \
				eraseVec2.push_back(useI); \
			} \
		} \
		for (std::vector<Instruction*>::iterator it=eraseVec2.begin(), E=eraseVec2.end(); it!=E; ++it) \
		{ \
			assert ((*it)->use_empty()); \
			(*it)->eraseFromParent(); \
		} \
		for (std::vector<Function*>::iterator it=eraseVec.begin(), E=eraseVec.end(); it!=E; ++it) \
		{ \
			std::cout << "removing unused function: " << (*it)->getName().str() << "\n"; \
			(*it)->eraseFromParent(); \
		} \
		\
		std::cout << "JIT compiling test suite... "; \
		llvm::ExecutionEngine* engine = LLVMWrapper::createExecutionEngine(module, useAVX); \
		void* mainPtr = LLVMWrapper::getPointerToFunction(mainFn, engine); \
		\
		if (!mainPtr) \
		{ \
			std::cout << "FAILED!\n"; \
			return -1; \
		} \
		else \
		{ \
			std::cout << "done.\n"; \
		} \
		\
		exitCode = LLVMWrapper::executeMain(mainPtr, argc, argv); \
		if (exitCode != 0) return exitCode; \
	}


int
main(int argc, char** argv)
{
    atexit(LLVMWrapper::shutdown);

    // print copyright information
    std::cout << "\n\
********************************************************************************\n\
*     Legacy Whole-Function Vectorization Test Suite                           *\n\
*                                                                              *\n\
*     Add new tests to the test_rv based suite.                                *\n\
*     Refer to test/README for details.                                        *\n\
********************************************************************************\n\
    \n";

    ///////////////////////////////////////////////////////////////////////////
    //                           read arguments                              //
    ///////////////////////////////////////////////////////////////////////////

    bool        verbose                       = false;
    bool        useAVX                        = false;
    bool        disableMemAccessAnalysis      = false;
    bool        disableControlFlowDivAnalysis = false;
    bool        disableAllAnalyses            = false;
    bool        optimize                      = true;
    bool        verify                        = true;
    bool        dump                          = false;
    bool        runTestSuite1                 = true;
    bool        onlyAnalyze                   = false;
    std::string filename1                     = "test/wfv1testsuite/wfvTests.bc";

    bool displayUsage = false;
    bool error = false;

    for (int i=1; i<argc; ++i)
    {
        if (strcmp(argv[i], "-mavx") == 0)
        {
            useAVX = true;
            continue;
        }
        if (strcmp(argv[i], "-a") == 0 || strcmp(argv[i], "--analyze") == 0 )
		{
			onlyAnalyze = true;
			continue;
		}
        if (strcmp(argv[i], "-no-avx") == 0)
        {
            useAVX = false;
            continue;
        }
        if (strcmp(argv[i], "-no-mem-analysis") == 0)
        {
            disableMemAccessAnalysis = true;
            continue;
        }
        if (strcmp(argv[i], "-no-cf-analysis") == 0)
        {
            disableControlFlowDivAnalysis = true;
            continue;
        }
        if (strcmp(argv[i], "-no-analyses") == 0)
        {
            disableAllAnalyses = true;
            continue;
        }
        if (strcmp(argv[i], "-v") == 0 || strcmp(argv[i], "-verbose") == 0)
        {
            verbose = true;
            continue;
        }
        if (strcmp(argv[i], "-d") == 0 || strcmp(argv[i], "-dump") == 0)
        {
            dump = true;
            continue;
        }
        if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "-help") == 0)
        {
            displayUsage = true;
            continue;
        }
        if (strcmp(argv[i], "-O0") == 0 || strcmp(argv[i], "-no-opt") == 0)
        {
            optimize = false;
            continue;
        }
        if (strcmp(argv[i], "-no-verify") == 0)
        {
            verify = false;
            continue;
        }
#if 0
        if (strcmp(argv[i], "-test1") == 0)
        {
            runTestSuite1 = true;
            runTestSuite2 = false;
            runTestSuite3 = false;
            continue;
        }
        if (strcmp(argv[i], "-test2") == 0)
        {
            runTestSuite1 = false;
            runTestSuite2 = true;
            runTestSuite3 = false;
            continue;
        }
        if (strcmp(argv[i], "-test3") == 0)
        {
            runTestSuite1 = false;
            runTestSuite2 = false;
            runTestSuite3 = true;
            continue;
        }
#endif

        std::cerr << "ERROR: Unknown argument found: " << argv[i] << "\n\n";
        displayUsage = true;
        error = true;
    }

    if (displayUsage)
    {
        std::cerr << "Usage: " << argv[0] << "\n"
                  << "Options:\n"
                  << "  -no-mem-analysis : disable memory access analysis\n"
                  << "  -no-cf-analysis  : disable control flow divergence analysis\n"
                  << "  -no-analyses     : disable all analyses\n"
                  << "  -no-sse41        : disable generation of SSE4.1 intrinsics\n"
                  << "  -mavx            : enable generation of AVX intrinsics\n"
                  << "  -no-avx          : disable generation of AVX intrinsics\n"
                  << "  -v -verbose      : enable verbose output (only valid for debug builds)\n"
                  << "  -O0 -no-opt      : disable module optimization after WFV\n"
                  << "  -no-verify       : disable verification of module after WFV\n"
                  << "  -d -dump         : enable dumping of test module\n"
                  << "  -h -help         : display this help message\n\n"
                  << "  -a -analyze      : only analyze and do not perform execution tests\n\n";
        return error ? -1 : 0;
    }

    const unsigned vectorizationFactor = useAVX ? 8 : 4;
    bool           failure   = false;
    int            exitCode  = 0;

    if (runTestSuite1)
    {
        //
        // test suite 1
        //

        RV_SETUP_TESTSUITE(filename1);

        // arithmetic only
        RV_RUN_ON_FUNCTION(test_001_simple);

        // simple control flow
        RV_RUN_ON_FUNCTION(test_002_if01);
        RV_RUN_ON_FUNCTION(test_003_if02);
        RV_RUN_ON_FUNCTION(test_004_if03);
        RV_RUN_ON_FUNCTION(test_005_if04);
        RV_RUN_ON_FUNCTION(test_006_if05);
        RV_RUN_ON_FUNCTION(test_007_if06);
        RV_RUN_ON_FUNCTION(test_008_if07);
        RV_RUN_ON_FUNCTION(test_009_if08);
        RV_RUN_ON_FUNCTION(test_010_if09);
        RV_RUN_ON_FUNCTION(test_011_if10);
        RV_RUN_ON_FUNCTION(test_012_if11);
        RV_RUN_ON_FUNCTION(test_013_if12);

        // simple loops
        RV_RUN_ON_FUNCTION(test_014_loop01);
        RV_RUN_ON_FUNCTION(test_015_loop02);
        RV_RUN_ON_FUNCTION(test_016_loop03);
        RV_RUN_ON_FUNCTION(test_017_loop04);
        RV_RUN_ON_FUNCTION(test_018_loop05);
        RV_RUN_ON_FUNCTION(test_019_loop06);

        // more complex loops
        RV_RUN_ON_FUNCTION(test_020_loopc01);
        RV_RUN_ON_FUNCTION(test_021_loopc02);
        RV_RUN_ON_FUNCTION(test_022_loopc03);
        RV_RUN_ON_FUNCTION(test_023_loopc04);
        RV_RUN_ON_FUNCTION(test_024_loopc05);
        RV_RUN_ON_FUNCTION(test_025_loopc06);
        RV_RUN_ON_FUNCTION(test_026_loopc07);
        RV_RUN_ON_FUNCTION(test_027_loopc08);
        RV_RUN_ON_FUNCTION(test_028_loopc09);

        // loops with multiple exits
        RV_RUN_ON_FUNCTION(test_029_loopmx01);
        RV_RUN_ON_FUNCTION(test_030_loopmx02);
        RV_RUN_ON_FUNCTION(test_031_loopmx03);
        RV_RUN_ON_FUNCTION(test_032_loopmx04);
        RV_RUN_ON_FUNCTION(test_033_loopmx05);
        RV_RUN_ON_FUNCTION(test_034_loopmx06);
        RV_RUN_ON_FUNCTION(test_035_loopmx07);
        RV_RUN_ON_FUNCTION(test_036_loopmx08);
        RV_RUN_ON_FUNCTION(test_037_loopmx09);
        RV_RUN_ON_FUNCTION(test_038_loopmx10);
        RV_RUN_ON_FUNCTION(test_039_loopmx11);
        RV_RUN_ON_FUNCTION(test_040_loopmx12);
        RV_RUN_ON_FUNCTION(test_041_loopmx13);

        // nested loops
        RV_RUN_ON_FUNCTION(test_042_loopns01);
        RV_RUN_ON_FUNCTION(test_043_loopns02);
        RV_RUN_ON_FUNCTION(test_044_loopns03);
        RV_RUN_ON_FUNCTION(test_045_loopns04);
        RV_RUN_ON_FUNCTION(test_046_loopns05);
        RV_RUN_ON_FUNCTION(test_047_loopns06);
        RV_RUN_ON_FUNCTION(test_048_loopns07);
        RV_RUN_ON_FUNCTION(test_049_loopns08);
        RV_RUN_ON_FUNCTION(test_050_loopns09);
        RV_RUN_ON_FUNCTION(test_051_loopns10);
        RV_RUN_ON_FUNCTION(test_052_loopns11);
        RV_RUN_ON_FUNCTION(test_053_loopns12);
        RV_RUN_ON_FUNCTION(test_054_loopns13);
        RV_RUN_ON_FUNCTION(test_055_loopns14);
        RV_RUN_ON_FUNCTION(test_056_loopns15);

        // nested loops with multiple exits
        RV_RUN_ON_FUNCTION(test_057_loopnsmx01);
        RV_RUN_ON_FUNCTION(test_058_loopnsmx02);
        RV_RUN_ON_FUNCTION(test_059_loopnsmx03);
        RV_RUN_ON_FUNCTION(test_060_loopnsmx04);
        RV_RUN_ON_FUNCTION(test_061_loopnsmx05);
        RV_RUN_ON_FUNCTION(test_062_loopnsmx06);
        RV_RUN_ON_FUNCTION(test_063_loopnsmx07);
        RV_RUN_ON_FUNCTION(test_064_loopnsmx08);
        RV_RUN_ON_FUNCTION(test_065_loopnsmx09);
        RV_RUN_ON_FUNCTION(test_066_loopnsmx10);
        RV_RUN_ON_FUNCTION(test_067_loopnsmx11);
        RV_RUN_ON_FUNCTION(test_068_loopnsmx12);
        RV_RUN_ON_FUNCTION(test_069_loopnsmx13);
        RV_RUN_ON_FUNCTION(test_070_loopnsmx14);
        RV_RUN_ON_FUNCTION(test_071_loopnsmx15);
        RV_RUN_ON_FUNCTION(test_072_loopnsmx16);

        // function calls
        RV_RUN_ON_FUNCTION(test_073_call01);
        RV_RUN_ON_FUNCTION(test_074_call02);
        RV_RUN_ON_FUNCTION(test_075_call03);
        RV_RUN_ON_FUNCTION(test_076_call04);
        RV_RUN_ON_FUNCTION(test_077_call05);
        RV_RUN_ON_FUNCTION(test_078_call06);
        RV_RUN_ON_FUNCTION(test_079_call07);
        RV_RUN_ON_FUNCTION(test_080_call08);
        // Create "native" functions for call9/call10.
        llvm::Function* noinlinecall2      = LLVMWrapper::getFunction("noinlinecall2",
                                                                      module);
        llvm::Function* noinlinecall2_SIMD = LLVMWrapper::getFunction("noinlinecall2_SIMD",
                                                                      module);
        llvm::Function* noinlinecall3      = LLVMWrapper::getFunction("noinlinecall3",
                                                                      module);
        llvm::Function* noinlinecall3_SIMD = LLVMWrapper::getFunction("noinlinecall3_SIMD",
                                                                      module);
        assert (noinlinecall2 && noinlinecall2_SIMD &&
                noinlinecall3 && noinlinecall3_SIMD);

        RV_RUN_ON_FUNCTION_NEW_INTERFACE_WITH_SINGLE_CALL(test_081_call09, noinlinecall2, -1);
        RV_RUN_ON_FUNCTION_NEW_INTERFACE_WITH_SINGLE_CALL(test_082_call10, noinlinecall3, 1);

        // misc tests
        RV_RUN_ON_FUNCTION(test_083_misc);
        RV_RUN_ON_FUNCTION(test_084_ocl_mandelbrot);
        RV_RUN_ON_FUNCTION(test_085_noise);
#if 0
        RV_RUN_ON_FUNCTION(test_086_ocl_aobench);
#endif
        //RV_RUN_ON_FUNCTION(test_087_ocl_aobench_inlined); // Sometimes fails due to struct/writeback issue.

//#define RUN_IRREDUCIBLE_TESTS
#ifdef RUN_IRREDUCIBLE_TESTS
        // tests for irreducible control flow
        RV_RUN_ON_FUNCTION(test_088_irreducible1);
        RV_RUN_ON_FUNCTION(test_089_irreducible2);
        RV_RUN_ON_FUNCTION(test_090_irreducible3);
        RV_RUN_ON_FUNCTION(test_091_irreducible4);
#endif

        RV_COMPILE_AND_RUN_TESTSUITE(1)
    }

    return exitCode;
}

