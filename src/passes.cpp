#include "rv/passes.h"

#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/IPO/AlwaysInliner.h"
#include "rv/transform/loopExitCanonicalizer.h"

using namespace llvm;

namespace rv {

void
addOuterLoopVectorizer(legacy::PassManagerBase & PM, Config config) {
   // PM.add(rv::createCNSPass());
   PM.add(createLoopSimplifyPass());
   PM.add(createLCSSAPass());
   PM.add(createLoopExitCanonicalizerPass()); // required for divLoopTrans
   PM.add(rv::createLoopVectorizerPass());

   // post rv cleanup
   PM.add(createAlwaysInlinerLegacyPass());
   PM.add(createInstructionCombiningPass());
   PM.add(createAggressiveDCEPass());
}


}
