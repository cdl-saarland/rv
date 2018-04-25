#include "rv/utils.h"

using namespace llvm;

namespace rv {

Type*
vectorizeType(Type* scalarTy, VectorShape shape, uint vectorWidth)
{
    if (scalarTy->isVoidTy()) return scalarTy;
    if (!shape.isDefined() || shape.hasStridedShape()) return scalarTy;

    return VectorType::get(scalarTy, vectorWidth);
}

Function*
createVectorDeclaration(Function& scalarFn, VectorShape resShape,
                        const VectorShapeVec& argShapes, uint vectorWidth,
                        int maskPos)
{
    auto* scalarFnTy = scalarFn.getFunctionType();

    auto* vectorRetTy = vectorizeType(scalarFnTy->getReturnType(), resShape, vectorWidth);

    std::vector<Type*> vectorArgTys;
    bool hasMaskArg = maskPos >= 0;
    int argIdx = 0; // scalar fn argument Idx
    for (int i = 0; i < (int) scalarFnTy->getNumParams() + hasMaskArg; ++i) {
        if (i != maskPos) {
          // actual scalar function argument
          auto* scalarArgTy = scalarFnTy->getParamType(argIdx);
          VectorShape argShape = argShapes[argIdx];
          vectorArgTys.push_back(vectorizeType(scalarArgTy, argShape, vectorWidth));
          ++argIdx;
        } else {
          // mask argument
          auto* boolTy = IntegerType::getInt1Ty(scalarFn.getContext());
          VectorShape argShape = VectorShape::varying();
          vectorArgTys.push_back(vectorizeType(boolTy, argShape, vectorWidth));
        }
    }

    auto* vectorFnTy = FunctionType::get(vectorRetTy, vectorArgTys, false);

    return llvm::Function::Create(vectorFnTy, scalarFn.getLinkage(), scalarFn.getName() + "_SIMD",
                                  scalarFn.getParent());
}

} // namespace rv
