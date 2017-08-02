#ifndef RV_REDOPT_H
#define RV_REDOPT_H

namespace llvm {
  class DominatorTree;
  class PHINode;
}




namespace rv {

class VectorizationInfo;
class ReductionAnalysis;
class Reduction;

class ReductionOptimization {
  VectorizationInfo & vecInfo;
  ReductionAnalysis & reda;
  llvm::DominatorTree & dt;

  bool optimize(llvm::PHINode & phi, Reduction & red);
public:
  ReductionOptimization(VectorizationInfo & vecInfo, ReductionAnalysis & reda, llvm::DominatorTree & domTree);
  bool run();
};


}

#endif // RV_REDOPT_H
