#include "rv/passes.h"

#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Utils.h"
#include "llvm/Transforms/AggressiveInstCombine/AggressiveInstCombine.h"
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
   PM.add(createAggressiveInstCombinerPass());
   PM.add(createAggressiveDCEPass());
}

void
addOuterLoopVectorizer(legacy::PassManagerBase & PM) {
   PM.add(rv::createLoopVectorizerPass());
}


void
addWholeFunctionVectorizer(llvm::legacy::PassManagerBase & PM) {
  PM.add(rv::createWFVPass());
}

void
addLowerBuiltinsPass(legacy::PassManagerBase & PM) {
   PM.add(rv::createLowerRVIntrinsicsPass());
}

void
addRVPasses(llvm::legacy::PassManagerBase & PM) {
  // normalize loops
  addPreparatoryPasses(PM);

  // vectorize scalar functions that have VectorABI attributes
  addWholeFunctionVectorizer(PM);

  // vectorize annotated loops
  addOuterLoopVectorizer(PM);

  // DCE, instcombine, ..
  addCleanupPasses(PM);
}

}
