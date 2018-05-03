#include "rv/passes.h"

#include "llvm/Transforms/Utils.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/IPO/AlwaysInliner.h"
#include "rv/transform/loopExitCanonicalizer.h"

using namespace llvm;

namespace rv {

static void
AddCleanupPasses(legacy::PassManagerBase & PM) {
   // post rv cleanup
   PM.add(createAlwaysInlinerLegacyPass());
   PM.add(createAggressiveInstCombinerPass());
   PM.add(createAggressiveDCEPass());
}

void
addOuterLoopVectorizer(legacy::PassManagerBase & PM, Config config) {
   // PM.add(rv::createCNSPass());
   PM.add(createLoopSimplifyPass());
   PM.add(createLCSSAPass());
   PM.add(createLoopExitCanonicalizerPass()); // required for divLoopTrans
   PM.add(rv::createLoopVectorizerPass());

   AddCleanupPasses(PM);
}


void addWholeFunctionVectorizer(llvm::legacy::PassManagerBase & PM) {
   PM.add(createLoopSimplifyPass());
  PM.add(createLCSSAPass());
  PM.add(createLoopExitCanonicalizerPass()); // required for divLoopTrans
  PM.add(rv::createWFVPass());

  AddCleanupPasses(PM);
}

}
