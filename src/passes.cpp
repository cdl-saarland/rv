#include "rv/passes.h"

#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/IPO/AlwaysInliner.h"
#include "rv/transform/loopExitCanonicalizer.h"

using namespace llvm;

namespace rv {

void
addPreparatoryPasses(legacy::PassManagerBase & PM) {
   PM.add(createLoopSimplifyPass());
   PM.add(createLCSSAPass());
   PM.add(createLoopExitCanonicalizerPass()); // required for divLoopTrans
}

void
addCleanupPasses(legacy::PassManagerBase & PM) {
   // post rv cleanup
   PM.add(createAlwaysInlinerLegacyPass());
   PM.add(createInstructionCombiningPass());
   PM.add(createAggressiveDCEPass());
}

void
addOuterLoopVectorizer(legacy::PassManagerBase & PM, Config config) {
   // PM.add(rv::createCNSPass());
   PM.add(rv::createLoopVectorizerPass());
}


void addWholeFunctionVectorizer(llvm::legacy::PassManagerBase & PM) {
  PM.add(rv::createWFVPass());
}

}
