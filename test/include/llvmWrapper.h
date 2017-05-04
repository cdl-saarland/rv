//===- llvmWrapper.h ----------------*- C++ -*-===//
//
//                     The Region Vectorizer
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// @author karrenberg
//

#ifndef _LLVMWRAPPER_H
#define _LLVMWRAPPER_H

#ifdef __INTEL_COMPILER
#   pragma warning( disable : 279 )
#endif

//#define DEBUG_LLVM_PASSES
#ifdef DEBUG_LLVM_PASSES
#   include <llvm/Support/Debug.h>
#endif

#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/Debug.h> //dbgs()

#include <vector>
#include <map>

//bitcode reading
#include <llvm/IR/Module.h>
#include <llvm/Bitcode/ReaderWriter.h>
#include <llvm/Support/MemoryBuffer.h>
#include <llvm/IR/Verifier.h>

// linking
#include <llvm/Linker/Linker.h>

// execution
// ExecutionEngine
#include "llvm/CodeGen/LinkAllCodegenComponents.h"
#include "llvm/ExecutionEngine/SectionMemoryManager.h"
#include "llvm/ExecutionEngine/MCJIT.h"

#include <llvm/Support/TargetSelect.h>

// optimization
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/LinkAllPasses.h>
#include <llvm/CodeGen/Passes.h>
#include <llvm/Transforms/IPO.h> //FunctionInliningPass
#include <llvm/Transforms/Utils/Cloning.h> //InlineFunction
#include <llvm/Transforms/IPO/PassManagerBuilder.h>
#include <llvm/Support/Timer.h>
#include <llvm/IRReader/IRReader.h>

#include <llvm/IR/LLVMContext.h>

#include "llvm/Support/ManagedStatic.h" // llvm_shutdown()

#include <llvm/IR/InstIterator.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/SourceMgr.h>

#include <system_error>

using namespace llvm;

namespace LLVMWrapper {

//
// create module, search function, etc.
//

Module*
createModuleFromFile(const std::string & fileName, LLVMContext & context)
{
   SMDiagnostic diag;
   auto modPtr = llvm::parseIRFile(fileName, diag, context);
   return modPtr.release();
}

void
verifyModule(Module* module)
{
    assert(module && "module must not be nullptr");
    //use LLVMs own verifier pass
    std::string errorMessage;
    llvm::raw_string_ostream emStream(errorMessage);
    StringRef ref;

    if (verifyModule(*module, &emStream))
    {
        errs() << "\nverification of module failed:\n" << errorMessage << "\n";
    }
}

//@returns the function or null if it was not found in the specified module
Function*
getFunction(const std::string& name, Module* module)
{
    assert (module);
    return module->getFunction(name);
}

std::string
getFunctionName(const Function* f)
{
    assert (f);
    return f->getName();
}

Module*
getModule(Function* f)
{
    assert (f);
    if (!f->getParent())
    {
        errs() << "ERROR: function '" << f->getName()
                << "' has no parent module!\n";
        return nullptr;
    }
    return f->getParent();
}

//
// linking
//

// link source module into target module
Module*
linkInModule(Module* target, Module* source)
{
    assert (source && target);

    llvm::Linker linker(*target);
    std::string errorMessage;
    if (linker.linkInModule(std::unique_ptr<Module>(source)))
    {
        errs() << "ERROR: Could not link source module into cloned target: "
                << errorMessage << "\n";
        return nullptr;
    }

    return target;
}

//
// execution
//

//we must prevent creation of several JITs
static ExecutionEngine *globalExecEngine = nullptr;
static SectionMemoryManager * memoryManager = nullptr;

ExecutionEngine*
getExecutionEngine()
{
    if (!globalExecEngine)
    {
        errs() << "ERROR: no execution engine available, create one first!\n";
        return nullptr;
    }
    return globalExecEngine;
}


ExecutionEngine*
createExecutionEngine(Module* mod, const bool useAVX)
{
    assert (mod);

    std::unique_ptr<Module> modPtr(mod);
    // FIXME don't recompile the entire for every kernel
#if 1
    if (!globalExecEngine)
#endif
    {
        //we first have to initialize the native target for code generation
        //InitializeAllTargetMCs();
        //LLVMLinkInMCJIT();
    	bool initFailed =
    			InitializeNativeTarget() ||
				InitializeNativeTargetAsmPrinter() ||
				InitializeNativeTargetAsmParser();

        if (initFailed)
        {
            errs() << "ERROR: could not initialize native target (required for "
                    << "LLVM execution engine)\n";
            return NULL;
        }

        std::string errorMessage = "";

        EngineBuilder eb(std::move(modPtr));
        eb.setErrorStr(&errorMessage);
#if 0
        // eb.setMCJITMemoryManager(MCJITMemoryManager::CreateDefaultMemManager());
        eb.setOptLevel(CodeGenOpt::Aggressive);
        // eb.setAllocateGVsWithCode(false);
        eb.setRelocationModel(Reloc::Default);
        eb.setCodeModel(CodeModel::JITDefault); // Default crashes with "invalid rip-relative address"
#endif
        eb.setMArch("x86-64");
        std::vector<std::string> attrs;
        attrs.push_back("sse");
        attrs.push_back("sse2");
        attrs.push_back("sse3");
        attrs.push_back("ssse3");
        attrs.push_back("sse4a");
		// eb.setUseMCJIT(true); // use MCJIT where possible!
#ifdef WFV_ENABLE_EXPERIMENTAL_MODEL
        if (true) {
#else
		if (useAVX) {      // FIXME query the host machine for this
#endif
            outs() << "Creating ExecutionEngine with AVX support!\n";
            attrs.push_back("avx");
            eb.setMCPU("corei7-avx");
        } else {
        	eb.setMCPU("penryn");
			attrs.push_back("-avx");
            // Force AVX disabled.
        }
        eb.setMAttrs(attrs);

        // Configure as specified in lli
        // MCJIT construction
        memoryManager = new SectionMemoryManager();
        std::unique_ptr<RTDyldMemoryManager> RTDyldMM(memoryManager);
        eb.setMCJITMemoryManager(std::move(RTDyldMM));
        eb.setEngineKind(EngineKind::JIT);

#if 0
        TargetOptions Options;
        Options.JITEmitDebugInfo = true;
		Options.JITEmitDebugInfoToDisk = true;
		eb.setTargetOptions(Options);
#endif

        eb.setVerifyModules(true);

        globalExecEngine = eb.create();


        // ExecutionEngine configuration
        globalExecEngine->DisableLazyCompilation(true);
        globalExecEngine->finalizeObject();

        // run global ctors
        globalExecEngine->runStaticConstructorsDestructors(false);

        if (errorMessage != "")
        {
            errs() << "ERROR: could not create execution engine for module "
                    << mod->getModuleIdentifier() << ": " << errorMessage << "\n";
            return NULL;
        }

        if (!globalExecEngine)
        {
            errs() << "ERROR: could not create execution engine for module "
                    << mod->getModuleIdentifier() << "!\n";
            return NULL;
        }
    }
#if 1
    else {
    	// finalize the old module
    	// globalExecEngine->runStaticConstructorsDestructors(true);
        /*if (globalExecEngine->removeModule(mod))
        {
            errs() << "WARNING: module '"
                    << mod->getModuleIdentifier()
                    << "' was already added to execution engine"
                    << " (removed and re-added)!\n";
        } */
		globalExecEngine->addModule(std::move(modPtr));
    }
#endif
#ifdef WFVOPENCL_ENABLE_TRAMP
        // Register profiling API
#define ADD_MAPPING(FUNC) \
		{ llvm::Function * theFunc = mod->getFunction(#FUNC); \
		  if (theFunc) globalExecEngine->addGlobalMapping(theFunc, reinterpret_cast<void*>(FUNC)); }
        ADD_MAPPING(PROFILING_start)
        ADD_MAPPING(PROFILING_end)

        ADD_MAPPING(PROFILING_trackEdge)
        ADD_MAPPING(PROFILING_trackEdge4)
        ADD_MAPPING(PROFILING_trackMem)

        ADD_MAPPING(PROFILING_event_kernelStarted)
        ADD_MAPPING(PROFILING_event_kernelFinished)
#undef ADD_MAPPING
#endif

    // globalExecEngine->runStaticConstructorsDestructors(false);

    return globalExecEngine;
}

void* getPointerToFunction(Function * func, ExecutionEngine * execEngine)
{
    void* fncPtr = execEngine->getPointerToFunction(func);

    globalExecEngine->finalizeObject();
    memoryManager->invalidateInstructionCache();

    return fncPtr;
}

void* getPointerToFunction(Module* mod, std::string functionName, const bool useAVX) {

    ExecutionEngine *execEngine = createExecutionEngine(mod, useAVX);
    if (!execEngine) exit(-1);

    Function* f = mod->getFunction(functionName);
    if (!f) {
        errs() << "Error: Function '" << functionName << "' not found in module "
            << mod->getModuleIdentifier() << "!\n";
        exit(-1);
    }

    void * fncPtr = execEngine->getPointerToFunction(f);

    globalExecEngine->finalizeObject();
    memoryManager->invalidateInstructionCache();

    return fncPtr;
}

ExecutionEngine*
createNewExecutionEngine(Module* mod, const bool useAVX)
{
    assert (mod);
    if (globalExecEngine)
    {
        globalExecEngine->runStaticConstructorsDestructors(true);
        // FIXME: needs further update
        //globalExecEngine->DeregisterAllTables();
        globalExecEngine->clearAllGlobalMappings();
        delete globalExecEngine;
        globalExecEngine = nullptr;
    }

    return createExecutionEngine(mod, useAVX);
}


bool
removeModule(Module* mod, ExecutionEngine* engine)
{
    assert (mod && engine);
    return engine->removeModule(mod);
}

void
deleteModule(llvm::Module * mod)
{
    llvm::ExecutionEngine *engine = getExecutionEngine();
    if (engine && mod)
    {
        engine->clearGlobalMappingsFromModule(mod);
        if (!engine->removeModule(mod))
        {
            errs() << "WARNING: could not remove module '"
                    << mod->getModuleIdentifier() << "' from engine!\n";
        }
    }
    delete mod;
}

int
executeMain(void* mainPtr, int argc, char **argv)
{
    TimerGroup tg("executeMain");
    llvm::Timer t("executeMain", tg);

    t.startTimer();
    // Cast mainPtr to function with arguments int and char**,
    // then invoke it with argc and argv
    // Directly casting is not allowed (pointer-to-object to
    // pointer-to-function cast).
    //int res = ((int (*)(int, char**))mainPtr)(argc,argv);
    typedef int (*FnPtrType)(int, char**);
    FnPtrType mainPtrTyped = nullptr;
    *reinterpret_cast<void**>(&mainPtrTyped) = mainPtr;
    const int res = mainPtrTyped(argc, argv);

    t.stopTimer();
    tg.print(dbgs());

    return res;
}

// This has to be called by every compilation unit that includes this wrapper.
static void
shutdown()
{
    if (globalExecEngine)
    {
        globalExecEngine->runStaticConstructorsDestructors(true);
        delete globalExecEngine;
    }

    llvm_shutdown(); // "pointer being freed was not allocated"
}

//
// optimization
//

void
inlineFunctionCalls(Function* f)
{
    bool functionChanged = true;
    while (functionChanged)
    {
        functionChanged = false;
        for (Function::iterator BB=f->begin(); BB!=f->end(); ++BB)
        {
            bool blockChanged = false;
            for (BasicBlock::iterator I=BB->begin(); !blockChanged && I!=BB->end();)
            {
                if (!isa<CallInst>(I))
                {
                    ++I;
                    continue;
                }

                CallInst* call = cast<CallInst>(I++);
                Function* callee = call->getCalledFunction();
                if (!callee)
                {
                    continue;
                }

                if (callee->hasFnAttribute(Attribute::AttrKind::NoInline))
                {
                    continue;
                }

                // Possibly deleted by InlineFunction().
                const std::string calleeName = callee->getName();

                InlineFunctionInfo IFI(nullptr, nullptr);
                blockChanged = InlineFunction(call, IFI);
                functionChanged |= blockChanged;
            }
        }
    }
}

// Transform all functions in the module to only use registers if possible
// (= construct SSA from load-store-code that is often produced by front ends)
// Adding the following line would perform optimizations similar to GCC's -O3
// createStandardModulePasses(&Passes, 3, false, false, true, true, false, createFunctionInliningPass());
void
optimizeModuleSimple(Module* mod)
{
    assert (mod);

    legacy::PassManager Passes;

    // Remove debug symbols etc. (vectorizer can not handle it) (#17)
    // NOTE: This also removes all variable- and block-names, which is
    //       not wanted for debugging purposes.
#ifndef DEBUG
    Passes.add(createStripSymbolsPass());
#endif

    // Transform to SSA.
    Passes.add(createTypeBasedAAWrapperPass());
    Passes.add(createBasicAAWrapperPass());
    Passes.add(createScalarReplAggregatesPass(-1, true));
    Passes.add(createPromoteMemoryToRegisterPass());

    // Remove dead code (vectorizer can not handle it).
    Passes.add(createDeadCodeEliminationPass());

    Passes.run(*mod);
}

void
optimizeModule(Module* mod)
{
    assert (mod);

    legacy::PassManager pm;

    PassManagerBuilder Builder;
    Builder.OptLevel = 3;
    Builder.LoopVectorize = false; // default: false
    Builder.BBVectorize = false; // default: false
    Builder.SLPVectorize = false; // default: false
    Builder.DisableUnrollLoops = false; // default: false
    Builder.populateModulePassManager(pm);

    pm.run(*mod);

    return;
}


//
// output / debugging
//

void
writeModuleToFile(Module* mod, const std::string& fileName)
{
    assert (mod);
    std::error_code EC;
    raw_fd_ostream file(fileName, EC, sys::fs::OpenFlags::F_RW);
    mod->print(file, nullptr);
    if (EC)	{
        errs() << "ERROR: printing module to file failed: " << EC.message() << "\n";
        return;
    }
    file.close();
}

void
writeFunctionToFile(const Function& f, const std::string& fileName)
{
	std::error_code EC;
	    raw_fd_ostream file(fileName, EC, sys::fs::OpenFlags::F_RW);
    f.print(file);
    file.close();
    if (EC)
    {
        errs() << "ERROR: printing function to file failed: " << EC.message() << "\n";
        return;
    }
    file.close();
}

} // namespace LLVMWrapper

#endif /* _LLVMWRAPPER_H */
